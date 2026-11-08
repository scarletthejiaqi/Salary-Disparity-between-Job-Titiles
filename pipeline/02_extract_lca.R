# -----------------------------------------------------------------------------
# Step 2 - Extract the columns we need from each DOL LCA spreadsheet into a
# slim, standardised parquet file (one per spreadsheet, cached).
#
# DOL changed the file layout over time (FY2019 has up to 10 worksites per row
# with "_1" suffixes; FY2020+ has one worksite per row and renamed columns).
# COLUMN_MAP lists, for every standard field, the source column names in order
# of preference. Personal data (contact names, e-mails, phone numbers) is never
# read.
#
# Spreadsheets are read with a streaming parser (pipeline/xlsx_stream.py, needs
# python3 + lxml) that keeps memory flat (~200 MB). readxl is used as a
# fallback, but it loads the whole sheet and needs 10-20+ GB of RAM for the
# largest DOL files - avoid it on machines with less than 32 GB.
#
#   Rscript pipeline/02_extract_lca.R
#   PIPELINE_CORES=2 Rscript pipeline/02_extract_lca.R   # parallel (streaming only)
# -----------------------------------------------------------------------------

source("pipeline/00_config.R")
suppressPackageStartupMessages({
  library(nanoparquet)
  library(parallel)
})

COLUMN_MAP <- list(
  case_number      = "CASE_NUMBER",
  case_status      = "CASE_STATUS",
  received_date    = c("RECEIVED_DATE", "CASE_SUBMITTED"),
  decision_date    = "DECISION_DATE",
  visa_class       = "VISA_CLASS",
  job_title        = "JOB_TITLE",
  soc_code         = "SOC_CODE",
  soc_title        = "SOC_TITLE",
  full_time        = "FULL_TIME_POSITION",
  begin_date       = c("BEGIN_DATE", "PERIOD_OF_EMPLOYMENT_START_DATE"),
  total_workers    = "TOTAL_WORKER_POSITIONS",
  new_employment   = "NEW_EMPLOYMENT",
  continued_employment = "CONTINUED_EMPLOYMENT",
  change_previous  = "CHANGE_PREVIOUS_EMPLOYMENT",
  new_concurrent   = "NEW_CONCURRENT_EMPLOYMENT",
  change_employer  = "CHANGE_EMPLOYER",
  amended_petition = "AMENDED_PETITION",
  employer_name    = "EMPLOYER_NAME",
  naics_code       = "NAICS_CODE",
  worksite_city    = c("WORKSITE_CITY", "WORKSITE_CITY_1"),
  worksite_county  = c("WORKSITE_COUNTY", "WORKSITE_COUNTY_1"),
  worksite_state   = c("WORKSITE_STATE", "WORKSITE_STATE_1"),
  worksite_postal  = c("WORKSITE_POSTAL_CODE", "WORKSITE_POSTAL_CODE_1"),
  secondary_entity = c("SECONDARY_ENTITY", "SECONDARY_ENTITY_1"),
  wage_from        = c("WAGE_RATE_OF_PAY_FROM", "WAGE_RATE_OF_PAY_FROM_1"),
  wage_to          = c("WAGE_RATE_OF_PAY_TO", "WAGE_RATE_OF_PAY_TO_1"),
  wage_unit        = c("WAGE_UNIT_OF_PAY", "WAGE_UNIT_OF_PAY_1"),
  pw_wage          = c("PREVAILING_WAGE", "PREVAILING_WAGE_1"),
  pw_unit          = c("PW_UNIT_OF_PAY", "PW_UNIT_OF_PAY_1"),
  pw_level         = c("PW_WAGE_LEVEL", "PW_WAGE_LEVEL_1"),
  h1b_dependent    = c("H_1B_DEPENDENT", "H-1B_DEPENDENT", "H1B_DEPENDENT")
)
REQUIRED <- c("case_number", "case_status", "received_date", "job_title", "soc_code",
              "wage_from", "wage_unit", "worksite_state")
STREAMER <- file.path("pipeline", "xlsx_stream.py")

streaming_available <- function() {
  py <- Sys.which("python3")
  nzchar(py) && suppressWarnings(system2(py, c("-I", "-c", shQuote("import lxml")),
                                         stdout = FALSE, stderr = FALSE)) == 0
}

#' Read the candidate source columns of one spreadsheet as character columns.
read_sheet <- function(xlsx, columns, streaming) {
  if (streaming) {
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    status <- system2(Sys.which("python3"),
                      c("-I", shQuote(STREAMER), shQuote(xlsx), shQuote(tmp),
                        shQuote(paste(columns, collapse = ","))), stderr = FALSE)
    if (status != 0) stop("streaming reader failed for ", basename(xlsx))
    dt <- fread(tmp, colClasses = "character", na.strings = NULL, showProgress = FALSE)
    # Columns that are absent in this file come back entirely empty
    present <- names(dt)[vapply(dt, function(v) any(nzchar(v)), logical(1))]
    return(list(data = dt, present = present))
  }
  header <- names(readxl::read_excel(xlsx, n_max = 0))
  col_types <- ifelse(header %in% columns, "text", "skip")
  dt <- as.data.table(readxl::read_excel(xlsx, col_types = col_types, guess_max = 0, progress = FALSE))
  list(data = dt, present = intersect(columns, header))
}

extract_one <- function(xlsx, out_dir, streaming) {
  out <- file.path(out_dir, sub("\\.xlsx$", ".parquet", basename(xlsx)))
  if (file.exists(out) && file.mtime(out) > file.mtime(xlsx)) return(out)

  sheet <- read_sheet(xlsx, unique(unlist(COLUMN_MAP)), streaming)
  src <- vapply(COLUMN_MAP, function(cands) {
    hit <- cands[cands %in% sheet$present]
    if (length(hit)) hit[1] else NA_character_
  }, character(1))
  missing <- names(src)[is.na(src)]
  if (any(REQUIRED %in% missing)) {
    stop(sprintf("%s is missing required columns: %s", basename(xlsx),
                 paste(intersect(REQUIRED, missing), collapse = ", ")))
  }
  keep <- src[!is.na(src)]
  dt <- sheet$data[, unname(keep), with = FALSE]
  setnames(dt, unname(keep), names(keep))
  for (m in missing) set(dt, j = m, value = NA_character_)
  for (j in names(dt)) {
    set(dt, j = j, value = trimws(dt[[j]]))   # match readxl's default trim_ws
    set(dt, i = which(dt[[j]] == ""), j = j, value = NA_character_)
  }
  setcolorder(dt, names(COLUMN_MAP))
  dt[, source_file := basename(xlsx)]

  write_parquet(dt, out)
  out
}

ensure_dirs()
out_dir <- file.path(PATHS$interim, "lca_extract")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(PATHS$raw_lca, pattern = "\\.xlsx$", full.names = TRUE)
if (!length(files)) cli_abort("No LCA spreadsheets found in {PATHS$raw_lca}. Run 01_download.R first.")

streaming <- streaming_available()
cores <- as.integer(Sys.getenv("PIPELINE_CORES", "1"))
if (!streaming) {
  cli_alert_warning("python3 + lxml not found: falling back to readxl, which needs 10-20+ GB RAM per large file.")
  cores <- 1L
}
cli_h1("Extracting {length(files)} LCA spreadsheets ({if (streaming) 'streaming' else 'readxl'}, {cores} worker{?s})")
t0 <- Sys.time()
res <- mclapply(files, function(f) {
  tryCatch(extract_one(f, out_dir, streaming), error = function(e) paste("ERROR:", conditionMessage(e)))
}, mc.cores = cores, mc.preschedule = FALSE)

errors <- grep("^ERROR", unlist(res), value = TRUE)
if (length(errors)) cli_abort(c("Extraction failed:", errors))
cli_alert_success("Extracted {length(res)} files in {round(difftime(Sys.time(), t0, units = 'mins'), 1)} min")
