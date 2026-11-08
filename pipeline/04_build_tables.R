# -----------------------------------------------------------------------------
# Step 4 - Aggregate clean filings into the small, app-ready tables in data/.
#
# Input : data-raw/interim/lca_clean.parquet  (from 03_clean_lca.R)
# Output: data/salary_trends.csv     period x role/family x seniority x filing type
#         data/salary_by_state.csv   year x role/family x seniority x state
#         data/top_employers.csv     year x role/family x employer (top N)
#         data/top_cities.csv        year x role/family x city (top N)
#         data/top_titles.csv        most common job titles per role
#         data/volume_monthly.csv    monthly filing counts per family
#         data/level_crosswalk.csv   title-based vs DOL wage-level seniority
#         data/role_quality.csv      classification coverage per role
#         data/taxonomy.csv          role / family / level dictionary
#
#   Rscript pipeline/04_build_tables.R
# -----------------------------------------------------------------------------

source("pipeline/00_config.R")
suppressPackageStartupMessages(library(nanoparquet))
for (f in list.files("pipeline/lib", pattern = "\\.R$", full.names = TRUE)) source(f)

# Read only the columns the tables need (keeps memory to a few hundred MB)
lca <- as.data.table(read_parquet(
  file.path(PATHS$interim, "lca_clean.parquet"),
  col_select = c("received", "year", "half", "month", "family_id", "role_id", "role_method",
                 "level_title", "level_dol", "new_hire", "state", "worksite_city", "employer",
                 "wage_annual", "title_norm")
))
for (j in c("family_id", "role_id", "role_method", "level_title", "level_dol", "state", "half", "month")) {
  if (is.factor(lca[[j]])) set(lca, j = j, value = droplevels(lca[[j]]))
}
cli_alert_info("{format(nrow(lca), big.mark = ',')} clean filings loaded")

MIN_N  <- ANALYSIS$min_n_publish
PROBS  <- c(p10 = .10, p25 = .25, p50 = .50, p75 = .75, p90 = .90)

# `sel__` is a logical row selection: grouping on a selection avoids copying
# the whole table for each subset.
wage_stats <- function(d, by, sel__ = NULL) {
  if (is.null(sel__)) sel__ <- rep(TRUE, nrow(d))
  out <- d[sel__, {
    q <- quantile(wage_annual, probs = PROBS, names = FALSE, type = 7)
    list(n = .N, p10 = q[1], p25 = q[2], p50 = q[3], p75 = q[4], p90 = q[5],
         mean = mean(wage_annual))
  }, keyby = by]
  for (j in intersect(by, names(out))) if (is.factor(out[[j]])) set(out, j = j, value = as.character(out[[j]]))
  stat_cols <- c("p10", "p25", "p50", "p75", "p90", "mean")
  out[, (stat_cols) := lapply(.SD, function(v) round(fifelse(n >= MIN_N, v, NA_real_))),
      .SDcols = stat_cols]
  out
}

# Role "views": individual role, whole family, or all roles.
ROLE_VIEWS <- list(
  role   = c("family_id", "role_id"),
  family = "family_id",
  all    = character(0)
)
fill_role_view <- function(dt) {
  if (!"family_id" %in% names(dt)) dt[, family_id := "all"]
  if (!"role_id" %in% names(dt)) dt[, role_id := "all"]
  dt
}

# ---- 1. Salary trends ----------------------------------------------------------
cli_h1("Salary trends")
LEVEL_VIEWS <- list(all = character(0), title = "level_title", dol = "level_dol")

trend_parts <- list()
for (filing in c("all", "new_hire")) {
  base_sel <- if (filing == "all") rep(TRUE, nrow(lca)) else lca$new_hire %in% TRUE
  for (ptype in c("year", "half")) {
    pcol <- if (ptype == "year") "year" else "half"
    for (rv in names(ROLE_VIEWS)) {
      for (lv in names(LEVEL_VIEWS)) {
        sel <- if (lv == "dol") base_sel & !is.na(lca$level_dol) else base_sel
        by <- c(pcol, ROLE_VIEWS[[rv]], LEVEL_VIEWS[[lv]])
        s <- wage_stats(lca, by, sel)
        setnames(s, pcol, "period")
        s[, period := as.character(period)]
        s <- fill_role_view(s)
        if (lv == "all") s[, level_id := "all"] else setnames(s, LEVEL_VIEWS[[lv]], "level_id")
        s[, `:=`(period_type = ptype, level_basis = lv, filing = filing)]
        trend_parts[[length(trend_parts) + 1]] <- s
      }
    }
  }
}
trends <- rbindlist(trend_parts, use.names = TRUE)
setcolorder(trends, c("period_type", "period", "filing", "family_id", "role_id",
                      "level_basis", "level_id"))
setorder(trends, period_type, period, filing, family_id, role_id, level_basis, level_id)
fwrite(trends, file.path(PATHS$out, "salary_trends.csv"))
cli_alert_success("salary_trends.csv: {format(nrow(trends), big.mark = ',')} rows")

# ---- 2. Salary by state ----------------------------------------------------------
cli_h1("Salary by state")
state_parts <- list()
has_state <- !is.na(lca$state)
for (rv in names(ROLE_VIEWS)) {
  for (lv in c("all", "title")) {
    by <- c("year", ROLE_VIEWS[[rv]], LEVEL_VIEWS[[lv]], "state")
    s <- wage_stats(lca, by, has_state)
    s <- fill_role_view(s)
    if (lv == "all") s[, level_id := "all"] else setnames(s, "level_title", "level_id")
    state_parts[[length(state_parts) + 1]] <- s[n >= MIN_N, .(year, family_id, role_id, level_id,
                                                                state, n, p25, p50, p75)]
  }
}
by_state <- rbindlist(state_parts, use.names = TRUE)
fwrite(by_state, file.path(PATHS$out, "salary_by_state.csv"))
cli_alert_success("salary_by_state.csv: {format(nrow(by_state), big.mark = ',')} rows")

# ---- 3. Top employers and cities ---------------------------------------------------
top_by <- function(d, group_col, n_top, min_n, sel__) {
  parts <- list()
  for (rv in names(ROLE_VIEWS)) {
    view <- c("year", ROLE_VIEWS[[rv]])
    by <- c(view, group_col)
    # 1. Count every group (fast), keep the n_top largest with >= min_n filings
    cnt <- d[sel__, .N, by = by][N >= min_n]
    setorderv(cnt, c(view, "N"), c(rep(1L, length(view)), -1L))
    cnt <- cnt[, head(.SD, n_top), by = view]
    # 2. Compute percentiles only for the kept groups
    s <- d[cnt, on = by, .(n = .N, p25 = quantile(wage_annual, .25, names = FALSE),
                           p50 = median(wage_annual), p75 = quantile(wage_annual, .75, names = FALSE),
                           new_hire_share = mean(new_hire)), by = .EACHI]
    for (j in by) if (is.factor(s[[j]])) set(s, j = j, value = as.character(s[[j]]))
    s <- fill_role_view(s)
    setorder(s, year, family_id, role_id, -n)
    parts[[rv]] <- s
  }
  out <- rbindlist(parts, use.names = TRUE)
  out[, c("p25", "p50", "p75") := lapply(.SD, round), .SDcols = c("p25", "p50", "p75")]
  out[, new_hire_share := round(new_hire_share, 3)]
  out
}

cli_h1("Top employers and cities")
employers <- top_by(lca, "employer", ANALYSIS$top_employers, 10L,
                    !is.na(lca$employer) & nzchar(lca$employer))
fwrite(employers, file.path(PATHS$out, "top_employers.csv"))
cli_alert_success("top_employers.csv: {format(nrow(employers), big.mark = ',')} rows")

clean_city <- function(city) {
  c <- gsub("[^A-Z ]", " ", toupper(city))
  c <- gsub("\\s+", " ", trimws(c))
  c <- sub("^(CITY OF|TOWN OF|VILLAGE OF) ", "", c)
  c[c %in% c("NEW YORK CITY", "NYC", "MANHATTAN", "NEW YORK NY")] <- "NEW YORK"
  c[c %in% c("ST LOUIS", "SAINT LOUIS")] <- "ST. LOUIS"
  c
}
# Clean each distinct city name once, then join (instead of 5M string operations)
city_map <- unique(lca[!is.na(state) & !is.na(worksite_city) & nzchar(worksite_city), .(worksite_city, state)])
city_map[, city := paste0(display_title(clean_city(worksite_city)), ", ", state)]
lca[city_map, on = .(worksite_city, state), city := i.city]
rm(city_map)
cities <- top_by(lca, "city", 20L, MIN_N, !is.na(lca$city))
fwrite(cities, file.path(PATHS$out, "top_cities.csv"))
cli_alert_success("top_cities.csv: {format(nrow(cities), big.mark = ',')} rows")

# ---- 4. Top job titles per role (all years) -------------------------------------
titles <- lca[!is.na(title_norm) & nzchar(title_norm), .(n = .N, p50 = round(median(wage_annual))),
              by = .(family_id, role_id, title_norm)]
titles[, `:=`(family_id = as.character(family_id), role_id = as.character(role_id))]
titles[, share := round(n / sum(n), 4), by = role_id]
setorder(titles, role_id, -n)
titles <- titles[, head(.SD, 15), by = role_id]
titles[, title := display_title(title_norm)]
fwrite(titles[, .(family_id, role_id, title, n, share, p50)], file.path(PATHS$out, "top_titles.csv"))

# ---- 5. Monthly filing volume -------------------------------------------------------
vol <- rbind(
  lca[, .(n = .N, n_new_hire = sum(new_hire)), by = .(month, family_id)][
    , `:=`(month = as.character(month), family_id = as.character(family_id))],
  lca[, .(n = .N, n_new_hire = sum(new_hire)), by = .(month)][
    , `:=`(month = as.character(month), family_id = "all")],
  use.names = TRUE
)
setorder(vol, family_id, month)
fwrite(vol, file.path(PATHS$out, "volume_monthly.csv"))

# ---- 6. Validation tables -------------------------------------------------------------
xwalk <- lca[!is.na(level_dol), .(n = .N, p50 = round(median(wage_annual))),
             by = .(level_title, level_dol)]
xwalk[, `:=`(level_title = as.character(level_title), level_dol = as.character(level_dol))]
xwalk[, share_of_title_level := round(n / sum(n), 4), by = level_title]
fwrite(xwalk[order(level_title, level_dol)], file.path(PATHS$out, "level_crosswalk.csv"))

quality <- lca[, .(n = .N, share_title_rule = round(mean(role_method == "title"), 3),
                   unique_titles = uniqueN(title_norm)), by = .(family_id, role_id)]
quality[, `:=`(family_id = as.character(family_id), role_id = as.character(role_id))]
fwrite(quality[order(-n)], file.path(PATHS$out, "role_quality.csv"))

# ---- 7. Taxonomy dictionary -------------------------------------------------------------
tax <- merge(ROLES, FAMILIES, by = "family_id")
tax[, indeed_sectors := vapply(family_id, function(f) paste(FAMILY_INDEED_SECTORS[[f]], collapse = "; "), "")]
setorder(tax, family_order, role_order)
fwrite(tax, file.path(PATHS$out, "taxonomy.csv"))
fwrite(rbind(SENIORITY_LEVELS[, basis := "title"], DOL_LEVELS[, basis := "dol"]),
       file.path(PATHS$out, "levels.csv"))

# ---- 8. Build metadata ------------------------------------------------------------------
meta_path <- file.path(PATHS$out, "metadata.json")
meta <- if (file.exists(meta_path)) jsonlite::read_json(meta_path) else list()
last_day <- max(lca$received)
meta$lca <- list(
  filings          = nrow(lca),
  first_received   = as.character(min(lca$received)),
  last_received    = as.character(last_day),
  latest_full_year = as.integer(if (format(last_day, "%m-%d") >= "12-01") year(last_day) else year(last_day) - 1L),
  partial_year     = as.integer(year(last_day)),
  partial_year_months = as.integer(format(last_day, "%m")),
  source_files     = sort(unique(list.files(PATHS$raw_lca, pattern = "\\.xlsx$"))),
  min_n_publish    = MIN_N
)
meta$built_at <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
jsonlite::write_json(meta, meta_path, auto_unbox = TRUE, pretty = TRUE)
cli_alert_success("All tables written to {PATHS$out}/")
