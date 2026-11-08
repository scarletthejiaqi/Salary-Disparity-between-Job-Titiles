# -----------------------------------------------------------------------------
# Date helpers (pure functions, unit-tested).
# -----------------------------------------------------------------------------

#' Parse dates read from DOL spreadsheets as text.
#'
#' Cells can arrive as Excel serial numbers ("45930"), ISO strings
#' ("2025-09-30" or "2025-09-30 00:00:00") or US-style strings ("9/30/2025").
parse_excel_date <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))

  serial <- grepl("^[0-9]+(\\.[0-9]+)?$", x)
  out[serial] <- as.Date(floor(as.numeric(x[serial])), origin = "1899-12-30")

  iso <- !serial & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  out[iso] <- as.Date(substr(x[iso], 1, 10))

  us <- !serial & !iso & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)
  out[us] <- as.Date(sub("^([0-9]{1,2}/[0-9]{1,2}/[0-9]{4}).*$", "\\1", x[us]), format = "%m/%d/%Y")

  out
}

#' Calendar half-year label, e.g. "2024-H1".
half_year_label <- function(date) {
  date <- as.Date(date)
  paste0(format(date, "%Y"), "-H", ifelse(as.integer(format(date, "%m")) <= 6, 1L, 2L))
}
