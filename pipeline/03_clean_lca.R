# -----------------------------------------------------------------------------
# Step 3 - Clean, de-duplicate and classify LCA filings.
#
# Input : data-raw/interim/lca_extract/*.parquet   (from 02_extract_lca.R)
# Output: data-raw/interim/lca_clean.parquet       (one row per certified case)
#         data/data_quality.csv                    (record counts per step)
#
# Memory-lean by design (peak ~1-1.5 GB): files are processed one at a time,
# newest first, reading only the needed columns. A case that re-appears in an
# older file is skipped, so the most recent determination always wins.
# Categorical columns are stored as factors and case numbers as numbers.
#
#   Rscript pipeline/03_clean_lca.R
# -----------------------------------------------------------------------------

source("pipeline/00_config.R")
suppressPackageStartupMessages(library(nanoparquet))
for (f in list.files("pipeline/lib", pattern = "\\.R$", full.names = TRUE)) source(f)
validate_taxonomy()

extract_dir <- file.path(PATHS$interim, "lca_extract")
files <- list.files(extract_dir, pattern = "\\.parquet$", full.names = TRUE)
if (!length(files)) cli_abort("No extracted files found. Run 02_extract_lca.R first.")

# Newest first: FY2026_Q3 > FY2025_Q4 > ... > FY2020_Q1 > FY2019 (full year)
fy <- as.integer(sub(".*FY(\\d{4}).*", "\\1", basename(files)))
q  <- suppressWarnings(as.integer(sub(".*_Q(\\d).*", "\\1", basename(files))))
q[is.na(q)] <- 4L
files <- files[order(-fy, -q)]
# Optional: PIPELINE_ONLY=FY2024_Q1 processes matching files only (quick checks)
only <- Sys.getenv("PIPELINE_ONLY", "")
if (nzchar(only)) files <- files[grepl(only, basename(files))]

NEEDED <- c("case_number", "case_status", "received_date", "decision_date", "visa_class",
            "job_title", "soc_code", "full_time", "new_employment", "change_employer",
            "employer_name", "worksite_city", "worksite_state",
            "wage_from", "wage_unit", "pw_wage", "pw_unit", "pw_level")

as_count <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  v[is.na(v)] <- 0
  v
}
yes_no <- function(x) {
  x <- toupper(trimws(as.character(x)))
  fifelse(x %in% c("Y", "YES"), TRUE, fifelse(x %in% c("N", "NO"), FALSE, NA))
}
# "I-200-25273-349981" -> 20025273349981 (exact in a double, 8 bytes per row)
case_id_of <- function(x) as.numeric(gsub("\\D", "", x))

LEVELS <- list(
  role_id     = ROLES$role_id,
  family_id   = FAMILIES$family_id,
  role_method = c("title", "soc", "none"),
  level_title = SENIORITY_LEVELS$level_id,
  level_dol   = DOL_LEVELS$level_id,
  state       = US_STATES$state_abb
)

steps <- c(raw = 0, unique = 0, certified = 0, recent = 0, full_time = 0, wage = 0, pw = 0)
seen <- numeric(0)
parts <- vector("list", length(files))

cli_h1("Cleaning {length(files)} files (newest first)")
for (k in seq_along(files)) {
  d <- as.data.table(read_parquet(files[k], col_select = NEEDED))
  steps["raw"] <- steps["raw"] + nrow(d)

  # ---- De-duplicate: latest determination wins -----------------------------------
  d[, case_id := case_id_of(case_number)]
  # Guard: numeric ids must be exact (<= 15 digits) and as unique as the strings
  if (any(nchar(gsub("\\D", "", d$case_number)) > 15, na.rm = TRUE) ||
      uniqueN(d$case_number) != uniqueN(d$case_id)) {
    cli_abort("Case numbers in {basename(files[k])} cannot be converted to unique numeric ids")
  }
  d[, case_number := NULL]
  d[, decided := as.integer(parse_excel_date(decision_date))]
  setorder(d, case_id, -decided)
  d <- d[!duplicated(case_id)]
  newer <- d$case_id %in% seen
  n_superseded <- sum(newer)
  seen <- c(seen, d$case_id)
  d <- d[!newer]
  steps["unique"] <- steps["unique"] + nrow(d)

  # ---- Filters -------------------------------------------------------------------
  d <- d[toupper(trimws(case_status)) %chin% c("CERTIFIED", "CERTIFIED - WITHDRAWN")]
  steps["certified"] <- steps["certified"] + nrow(d)
  d[, received := as.IDate(parse_excel_date(received_date))]
  d <- d[!is.na(received) & received >= ANALYSIS$start_date]
  steps["recent"] <- steps["recent"] + nrow(d)
  d <- d[yes_no(full_time) %in% TRUE]
  steps["full_time"] <- steps["full_time"] + nrow(d)
  d[, wage_annual := annualize_wage(parse_money(wage_from), wage_unit)]
  d <- d[plausible_wage(wage_annual, ANALYSIS$min_annual_wage, ANALYSIS$max_annual_wage)]
  steps["wage"] <- steps["wage"] + nrow(d)
  d[, pw_annual := annualize_wage(parse_money(pw_wage), pw_unit)]
  d <- d[is.na(pw_annual) | wage_annual >= 0.9 * pw_annual]
  steps["pw"] <- steps["pw"] + nrow(d)

  # ---- Classify roles and seniority (unique titles in this file) -----------------
  d[, `:=`(job_title = trimws(job_title), soc_code = normalize_soc(soc_code))]
  titles <- unique(d[, .(job_title, soc_code)])
  titles[, title_norm := normalize_title(job_title)]
  cls <- classify_role(titles$title_norm, titles$soc_code)
  titles[, `:=`(role_id = cls$role_id, role_method = cls$method,
                level_title = classify_seniority(title_norm))]
  d[titles, on = .(job_title, soc_code),
    `:=`(title_norm = i.title_norm, role_id = i.role_id, role_method = i.role_method,
         level_title = i.level_title)]
  d[, family_id := ROLES$family_id[match(role_id, ROLES$role_id)]]

  out <- d[, .(
    case_id, received,
    visa_class    = factor(trimws(visa_class)),
    family_id     = factor(family_id, LEVELS$family_id),
    role_id       = factor(role_id, LEVELS$role_id),
    role_method   = factor(role_method, LEVELS$role_method),
    level_title   = factor(level_title, LEVELS$level_title),
    level_dol     = factor(dol_wage_level(pw_level), LEVELS$level_dol),
    new_hire      = as_count(new_employment) > 0 | as_count(change_employer) > 0,
    state         = factor(normalize_state(worksite_state), LEVELS$state),
    worksite_city = toupper(trimws(worksite_city)),
    employer_name = trimws(employer_name),
    wage_annual, pw_annual,
    soc_code      = factor(soc_code),
    title_norm
  )]
  parts[[k]] <- out
  # Some DOL releases are cumulative (e.g. FY2021 Q3 repeats Q1-Q2), so older
  # files can be entirely superseded - that is expected, not an error.
  cli_alert_info("{basename(files[k])}: {format(nrow(out), big.mark = ',')} kept, {format(n_superseded, big.mark = ',')} superseded by newer files")
  rm(d, out, titles, cls, newer)
  invisible(gc(verbose = FALSE))
}
rm(seen)
lca <- rbindlist(parts)
rm(parts)
invisible(gc(verbose = FALSE))

# ---- Employers: group spellings and parent companies (on unique names only) ----------
emp <- lca[, .N, by = employer_name]
emp[, key := employer_key(employer_name)]
emp[, parent := employer_parent(key)]
emp[, top_spelling := employer_name[which.max(N)], by = key]
emp[, employer := fifelse(is.na(parent), top_spelling, parent)]
lca[emp, on = "employer_name", employer := i.employer]
lca[, employer_name := NULL]
rm(emp)

# ---- Time keys ---------------------------------------------------------------------
# Label each distinct date once (a few thousand) and join, instead of
# formatting millions of dates.
dates <- unique(lca[, .(received)])
dates[, `:=`(half = factor(half_year_label(received)), month = factor(format(received, "%Y-%m")))]
lca[, year := year(received)]
lca[dates, on = "received", `:=`(half = i.half, month = i.month)]
rm(dates)
setorder(lca, received)
write_parquet(lca, file.path(PATHS$interim, "lca_clean.parquet"))

labels <- c(
  raw       = "Raw records in DOL disclosure files",
  unique    = "Unique case numbers (latest determination kept)",
  certified = "Certified (incl. certified then withdrawn)",
  recent    = sprintf("Received on or after %s", ANALYSIS$start_date),
  full_time = "Full-time positions",
  wage      = sprintf("Annual wage between $%s and $%s", format(ANALYSIS$min_annual_wage, big.mark = ",", scientific = FALSE),
                      format(ANALYSIS$max_annual_wage, big.mark = ",", scientific = FALSE)),
  pw        = "Offered wage consistent with prevailing wage"
)
funnel <- data.table(step = unname(labels[names(steps)]), n = as.numeric(steps))
funnel[, pct_of_raw := round(100 * n / n[1], 1)]
fwrite(funnel, file.path(PATHS$out, "data_quality.csv"))
print(funnel)
cli_alert_success("Saved {format(nrow(lca), big.mark = ',')} clean filings, {min(lca$received)} to {max(lca$received)}")
