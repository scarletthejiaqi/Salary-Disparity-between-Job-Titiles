# -----------------------------------------------------------------------------
# U.S. state helpers (pure functions, unit-tested).
# -----------------------------------------------------------------------------

US_STATES <- data.table::data.table(
  state_abb  = c(datasets::state.abb, "DC", "PR", "GU", "VI", "MP", "AS"),
  state_name = c(datasets::state.name, "District of Columbia", "Puerto Rico",
                 "Guam", "U.S. Virgin Islands", "Northern Mariana Islands",
                 "American Samoa")
)

#' Normalise worksite state values (abbreviations or full names) to
#' two-letter postal abbreviations. Unknown values become NA.
normalize_state <- function(x) {
  s <- toupper(trimws(as.character(x)))
  out <- ifelse(s %in% US_STATES$state_abb, s, NA_character_)
  by_name <- match(s, toupper(US_STATES$state_name))
  out[is.na(out) & !is.na(by_name)] <- US_STATES$state_abb[by_name[is.na(out) & !is.na(by_name)]]
  out
}
