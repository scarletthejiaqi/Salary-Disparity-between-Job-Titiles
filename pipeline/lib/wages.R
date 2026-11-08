# -----------------------------------------------------------------------------
# Wage parsing and annualisation helpers (pure functions, unit-tested).
# -----------------------------------------------------------------------------

#' Parse money-like strings ("$85,000.00", "85000", " 42.5 ") into numbers.
parse_money <- function(x) {
  x <- gsub("[$,[:space:]]", "", as.character(x))
  x[x %in% c("", "NA", "N/A", "None")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

#' Multipliers that convert a pay rate into an annual amount
#' (2,080 hours = 40 hours x 52 weeks, the DOL convention).
WAGE_UNIT_MULTIPLIER <- c(
  "year"      = 1,
  "month"     = 12,
  "bi-weekly" = 26,
  "week"      = 52,
  "hour"      = 2080
)

#' Standardise the unit labels used across DOL file vintages.
normalize_wage_unit <- function(unit) {
  u <- tolower(trimws(as.character(unit)))
  u <- gsub("[[:space:]]+", " ", u)
  out <- rep(NA_character_, length(u))
  out[u %in% c("year", "yr", "annual", "yearly")]              <- "year"
  out[u %in% c("month", "mth", "monthly")]                      <- "month"
  out[u %in% c("bi-weekly", "biweekly", "bi weekly", "2 weeks")] <- "bi-weekly"
  out[u %in% c("week", "wk", "weekly")]                         <- "week"
  out[u %in% c("hour", "hr", "hourly")]                         <- "hour"
  out
}

#' Convert a wage amount and unit to an annual amount.
annualize_wage <- function(amount, unit) {
  unit <- normalize_wage_unit(unit)
  as.numeric(amount) * unname(WAGE_UNIT_MULTIPLIER[unit])
}

#' Flag annual wages that fall inside plausible bounds.
plausible_wage <- function(annual, min_wage = 25000, max_wage = 1000000) {
  !is.na(annual) & annual >= min_wage & annual <= max_wage
}
