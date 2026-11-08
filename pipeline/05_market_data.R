# -----------------------------------------------------------------------------
# Step 5 - Market-wide context: Indeed job postings & posted-wage growth by
# occupational sector, JOLTS job openings and CPI-U (for real wages).
#
# Output: data/indeed_postings.csv     monthly postings index (Feb 1 2020 = 100)
#         data/indeed_wage_growth.csv  monthly YoY growth in posted wages
#         data/jolts.csv               monthly total job openings (thousands)
#         data/cpi.csv                 CPI-U by year / half-year + real-dollar factors
#
#   Rscript pipeline/05_market_data.R
# -----------------------------------------------------------------------------

source("pipeline/00_config.R")
for (f in list.files("pipeline/lib", pattern = "\\.R$", full.names = TRUE)) source(f)

sector_family <- rbindlist(lapply(names(FAMILY_INDEED_SECTORS), function(f) {
  data.table(sector = FAMILY_INDEED_SECTORS[[f]], family_id = f)
}))

# ---- Indeed job postings index -------------------------------------------------
cli_h1("Indeed job postings")
sec <- fread(file.path(PATHS$raw_indeed, "sector_postings.csv"))
tot <- fread(file.path(PATHS$raw_indeed, "total_postings.csv"))
sec <- sec[variable == "total postings", .(date = as.IDate(date), sector = display_name,
                                           index = indeed_job_postings_index)]
tot <- tot[variable == "total postings", .(date = as.IDate(date), sector = "All postings (US)",
                                           index = indeed_job_postings_index_SA)]
postings <- rbind(tot, sec)[, .(index = round(mean(index), 2), days = .N),
                            by = .(month = format(date, "%Y-%m"), sector)]
postings <- sector_family[postings, on = "sector"]
postings[sector == "All postings (US)", family_id := "all"]
setorder(postings, sector, month)
fwrite(postings, file.path(PATHS$out, "indeed_postings.csv"))
cli_alert_success("indeed_postings.csv: {uniqueN(postings$sector)} series, {min(postings$month)} to {max(postings$month)}")

# ---- Indeed posted wage growth ----------------------------------------------------
cli_h1("Indeed posted wage growth")
parse_month <- function(x) as.IDate(paste0("01-", x), format = "%d-%b-%y")
wg <- fread(file.path(PATHS$raw_indeed, "sector_wages.csv"))[jobcountry == "US"]
wg <- wg[, .(month = format(parse_month(month), "%Y-%m"), sector, n_obs,
             yoy = posted_wage_growth_yoy, yoy_3mo = posted_wage_growth_yoy_3moavg)]
country_file <- file.path(PATHS$raw_indeed, "country_wages.csv")
if (file.exists(country_file)) {
  us <- fread(country_file)[jobcountry == "US"]
  us <- us[, .(month = format(parse_month(month), "%Y-%m"), sector = "All postings (US)", n_obs,
               yoy = posted_wage_growth_yoy, yoy_3mo = posted_wage_growth_yoy_3moavg)]
  wg <- rbind(us, wg)
} else {
  cli_alert_warning("country_wages.csv missing - re-run 01_download.R to add the US total")
}
wg <- sector_family[wg, on = "sector"]
wg[sector == "All postings (US)", family_id := "all"]
setorder(wg, sector, month)
fwrite(wg, file.path(PATHS$out, "indeed_wage_growth.csv"))
cli_alert_success("indeed_wage_growth.csv: {uniqueN(wg$sector)} series, {min(wg$month)} to {max(wg$month)}")

# ---- JOLTS --------------------------------------------------------------------------
jolts <- fread(file.path(PATHS$raw_fred, "JTSJOL.csv"))
setnames(jolts, c("date", "openings_k"))
jolts <- jolts[as.IDate(date) >= as.IDate("2018-01-01") & !is.na(openings_k),
               .(month = format(as.IDate(date), "%Y-%m"), openings_k)]
fwrite(jolts, file.path(PATHS$out, "jolts.csv"))

# ---- CPI-U ---------------------------------------------------------------------------
cpi <- fread(file.path(PATHS$raw_fred, "CPIAUCSL.csv"))
setnames(cpi, c("date", "cpi"))
cpi[, date := as.IDate(date)]
cpi <- cpi[date >= as.IDate("2019-01-01")]
lca_meta <- jsonlite::read_json(file.path(PATHS$out, "metadata.json"))$lca
last_lca_month <- as.IDate(paste0(substr(lca_meta$last_received, 1, 7), "-01"))
# Only average over months covered by the LCA data so partial periods match.
# Months BLS did not publish (October 2025, after the federal shutdown) are
# skipped; `months` records how many months each average uses.
cpi_cov <- cpi[date <= last_lca_month]
by_year <- cpi_cov[, .(cpi = mean(cpi, na.rm = TRUE), months = sum(!is.na(cpi))),
                   by = .(period = as.character(year(date)))]
by_half <- cpi_cov[, .(cpi = mean(cpi, na.rm = TRUE), months = sum(!is.na(cpi))),
                   by = .(period = half_year_label(date))]
cpi_out <- rbind(by_year[, period_type := "year"], by_half[, period_type := "half"])
base <- by_year[period == as.character(ANALYSIS$cpi_base_year), cpi]
if (length(base) != 1 || is.na(base)) cli_abort("CPI base year {ANALYSIS$cpi_base_year} is missing")
cpi_out[, real_factor := round(base / cpi, 5)]
cpi_out[, cpi := round(cpi, 3)]
setcolorder(cpi_out, c("period_type", "period"))
fwrite(cpi_out, file.path(PATHS$out, "cpi.csv"))

meta_path <- file.path(PATHS$out, "metadata.json")
meta <- jsonlite::read_json(meta_path)
meta$market <- list(
  indeed_postings_last_month = max(postings$month),
  indeed_wages_last_month    = max(wg$month),
  jolts_last_month           = max(jolts$month),
  cpi_last_month             = format(max(cpi$date[!is.na(cpi$cpi)]), "%Y-%m"),
  cpi_base_year              = ANALYSIS$cpi_base_year
)
jsonlite::write_json(meta, meta_path, auto_unbox = TRUE, pretty = TRUE)
cli_alert_success("Market context tables written")
