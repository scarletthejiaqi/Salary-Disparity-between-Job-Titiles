# -----------------------------------------------------------------------------
# Pipeline configuration: paths, source URLs and analysis constants.
# Every other pipeline script sources this file first.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(cli)
})

PATHS <- list(
  raw        = "data-raw",
  raw_lca    = file.path("data-raw", "lca"),
  raw_indeed = file.path("data-raw", "indeed"),
  raw_fred   = file.path("data-raw", "fred"),
  interim    = file.path("data-raw", "interim"),
  out        = "data"
)

SOURCES <- list(
  # DOL Office of Foreign Labor Certification - public disclosure files page.
  # LCA disclosure links are discovered from this page so new quarters are
  # picked up automatically; the fallback manifest below is used if the page
  # layout changes.
  dol_page = "https://www.dol.gov/agencies/eta/foreign-labor/performance",
  dol_base = "https://www.dol.gov",
  dol_fallback = c(
    "/sites/dolgov/files/ETA/oflc/pdfs/H-1B_Disclosure_Data_FY2019.xlsx",
    sprintf("/sites/dolgov/files/ETA/oflc/pdfs/LCA_Disclosure_Data_FY%d_Q%d.xlsx",
            rep(2020:2025, each = 4), rep(1:4, times = 6)),
    "/media/LCA_Disclosure_Data_FY2026_Q3.xlsx"
  ),

  # Indeed Hiring Lab (CC BY 4.0)
  indeed = c(
    sector_postings = "https://raw.githubusercontent.com/hiring-lab/job_postings_tracker/master/US/job_postings_by_sector_US.csv",
    total_postings  = "https://raw.githubusercontent.com/hiring-lab/job_postings_tracker/master/US/aggregate_job_postings_US.csv",
    sector_wages    = "https://raw.githubusercontent.com/hiring-lab/indeed-wage-tracker/main/posted-wage-growth-by-sector.csv",
    country_wages   = "https://raw.githubusercontent.com/hiring-lab/indeed-wage-tracker/main/posted-wage-growth-by-country.csv",
    sector_examples = "https://raw.githubusercontent.com/hiring-lab/job_postings_tracker/master/sector-job-title-examples.csv"
  ),

  # FRED (Federal Reserve Bank of St. Louis) - BLS series, public domain
  fred = c(
    CPIAUCSL = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL",
    JTSJOL   = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=JTSJOL"
  )
)

ANALYSIS <- list(
  first_fiscal_year = 2019L,            # earliest DOL fiscal year downloaded
  start_date        = as.IDate("2019-01-01"),
  # Annualised wage sanity bounds (USD). Values outside are treated as data
  # entry errors (e.g. hourly rates entered with a yearly unit).
  min_annual_wage   = 25000,
  max_annual_wage   = 1000000,
  # Minimum number of filings required to publish a statistic.
  min_n_publish     = 30L,
  # Base year for inflation-adjusted ("real") dollars.
  cpi_base_year     = 2025L,
  # Number of employers kept per role and year in the employer table.
  top_employers     = 25L
)

# Download timeout for the large DOL spreadsheets (seconds)
options(timeout = max(1800, getOption("timeout")))

ensure_dirs <- function() {
  for (p in PATHS) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}
