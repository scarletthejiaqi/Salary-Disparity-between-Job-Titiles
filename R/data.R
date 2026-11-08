# -----------------------------------------------------------------------------
# Loading the published tables in data/ and small lookup helpers.
# -----------------------------------------------------------------------------

STAT_COLS <- c("p10", "p25", "p50", "p75", "p90", "mean")

load_app_data <- function(dir = "data") {
  read <- function(file, ...) {
    data.table::fread(file.path(dir, file), data.table = FALSE, showProgress = FALSE, ...)
  }
  meta <- jsonlite::read_json(file.path(dir, "metadata.json"))
  taxonomy <- read("taxonomy.csv")
  taxonomy <- taxonomy[order(taxonomy$family_order, taxonomy$role_order), ]

  trends <- read("salary_trends.csv", colClasses = list(character = "period"))
  trends$period_date <- period_to_date(trends$period_type, trends$period)

  list(
    meta       = meta,
    taxonomy   = taxonomy,
    families   = unique(taxonomy[, c("family_id", "family_name", "family_order")]),
    levels     = read("levels.csv"),
    trends     = trends,
    states     = read("salary_by_state.csv"),
    employers  = read("top_employers.csv"),
    cities     = read("top_cities.csv"),
    titles     = read("top_titles.csv"),
    volume     = read("volume_monthly.csv"),
    crosswalk  = read("level_crosswalk.csv"),
    quality    = read("role_quality.csv"),
    funnel     = read("data_quality.csv"),
    postings   = read("indeed_postings.csv"),
    wages      = read("indeed_wage_growth.csv"),
    jolts      = read("jolts.csv"),
    cpi        = read("cpi.csv", colClasses = list(character = "period"))
  )
}

#' Plot positions for periods: years at mid-year, halves at mid-half.
period_to_date <- function(period_type, period) {
  out <- rep(as.Date(NA), length(period))
  y <- period_type == "year"
  out[y] <- as.Date(paste0(period[y], "-07-01"))
  h <- period_type == "half"
  yr <- substr(period[h], 1, 4)
  out[h] <- as.Date(ifelse(substr(period[h], 7, 7) == "1", paste0(yr, "-04-01"), paste0(yr, "-10-01")))
  out
}

# ---- Subjects: a role, a whole family, or all roles ---------------------------

subject_key <- function(family_id, role_id) paste(family_id, role_id, sep = "|")

parse_subject <- function(key) {
  parts <- strsplit(key, "|", fixed = TRUE)
  list(family_id = vapply(parts, `[`, "", 1), role_id = vapply(parts, `[`, "", 2))
}

subject_choices <- function(taxonomy, include_families = TRUE) {
  fams <- unique(taxonomy[, c("family_id", "family_name")])
  out <- list("Overall" = c("All roles" = subject_key("all", "all")))
  for (i in seq_len(nrow(fams))) {
    roles <- taxonomy[taxonomy$family_id == fams$family_id[i], ]
    ch <- stats::setNames(subject_key(roles$family_id, roles$role_id), roles$role_name)
    if (include_families) {
      ch <- c(stats::setNames(subject_key(fams$family_id[i], "all"), paste("All", fams$family_name[i])), ch)
    }
    out[[fams$family_name[i]]] <- ch
  }
  out
}

subject_label <- function(key, taxonomy) {
  s <- parse_subject(key)
  vapply(seq_along(key), function(i) {
    if (s$family_id[i] == "all") return("All roles")
    if (s$role_id[i] == "all") {
      return(paste("All", taxonomy$family_name[match(s$family_id[i], taxonomy$family_id)]))
    }
    taxonomy$role_name[match(s$role_id[i], taxonomy$role_id)]
  }, character(1))
}

level_label <- function(level_id, levels) {
  out <- levels$level_name[match(level_id, levels$level_id)]
  out[level_id == "all"] <- "All levels"
  out
}

#' Filter a table to one subject (role / family / all roles).
filter_subject <- function(df, key) {
  s <- parse_subject(key)
  df[df$family_id == s$family_id & df$role_id == s$role_id, , drop = FALSE]
}

# ---- Dollars ----------------------------------------------------------------------

#' Convert wage columns to constant (CPI-U) dollars when requested.
apply_dollars <- function(df, cpi, mode = c("nominal", "real"), period_type = NULL,
                          cols = intersect(STAT_COLS, names(df))) {
  mode <- match.arg(mode)
  if (mode == "nominal" || !nrow(df)) return(df)
  ptype <- if (!is.null(period_type)) rep(period_type, nrow(df)) else df$period_type
  period <- if ("period" %in% names(df)) df$period else as.character(df$year)
  f <- cpi$real_factor[match(paste(ptype, period), paste(cpi$period_type, cpi$period))]
  for (c in cols) df[[c]] <- df[[c]] * f
  df
}

dollar_note <- function(mode, meta) {
  if (identical(mode, "real")) {
    sprintf("Inflation-adjusted to %s dollars (CPI-U).", meta$market$cpi_base_year)
  } else {
    "Nominal dollars (not adjusted for inflation)."
  }
}

#' Human label for a year, marking the partial (year-to-date) year.
year_label <- function(year, meta) {
  partial <- as.integer(meta$lca$partial_year)
  months <- as.integer(meta$lca$partial_year_months)
  ifelse(year == partial & months < 12,
         paste0(year, " (Jan–", month.abb[months], ")"), as.character(year))
}
