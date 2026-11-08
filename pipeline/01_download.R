# -----------------------------------------------------------------------------
# Step 1 - Download raw source files into data-raw/ (cached: files that are
# already complete are skipped). Writes data-raw/download_manifest.csv.
#
#   Rscript pipeline/01_download.R
# -----------------------------------------------------------------------------

source("pipeline/00_config.R")

ensure_dirs()

# All HTTP requests go through the system `curl` command-line tool (available on
# macOS, Linux and GitHub Actions runners). If a source refuses the request,
# download the file manually in a browser and place it in the folder printed in
# the error message - the step will then treat it as cached.
if (!nzchar(Sys.which("curl"))) cli_abort("The `curl` command-line tool is required.")

curl_text <- function(args) {
  out <- suppressWarnings(system2("curl", c("-sS", "--retry", "3", args), stdout = TRUE, stderr = TRUE))
  paste(out, collapse = "\n")
}

remote_size <- function(url) {
  hdr <- curl_text(c("-I", "-L", shQuote(url)))
  len <- regmatches(hdr, gregexpr("(?i)content-length:\\s*\\d+", hdr, perl = TRUE))[[1]]
  if (!length(len)) return(NA_real_)
  as.numeric(sub("\\D*(\\d+)$", "\\1", len[length(len)]))
}

download_cached <- function(url, dest, check_size = TRUE) {
  expected <- if (check_size) remote_size(url) else NA_real_
  if (file.exists(dest) && (!check_size || is.na(expected) || file.size(dest) == expected)) {
    cli_alert_success("cached  {basename(dest)}")
    return(invisible(dest))
  }
  tmp <- paste0(dest, ".part")
  cli_alert_info("fetch   {basename(dest)}{if (!is.na(expected)) paste0(' (', round(expected / 1e6), ' MB)') else ''}")
  status <- tryCatch(
    utils::download.file(url, tmp, method = "curl", quiet = TRUE,
                         extra = c("-f", "-L", "--retry", "3")),
    error = function(e) 1L
  )
  if (status != 0 || !file.exists(tmp) || (!is.na(expected) && file.size(tmp) != expected)) {
    unlink(tmp)
    cli_abort(c("Download failed: {url}",
                "i" = "Download it in a browser and save it as {.path {dest}}, then re-run."))
  }
  file.rename(tmp, dest)
  invisible(dest)
}

# ---- DOL LCA disclosure files -------------------------------------------------
discover_lca_links <- function() {
  page <- curl_text(c("-L", "-f", shQuote(SOURCES$dol_page)))
  hrefs <- regmatches(page, gregexpr('href="[^"]+\\.xlsx"', page))[[1]]
  hrefs <- gsub('^href="|"$', "", hrefs)
  pattern <- "(LCA_Disclosure_Data_FY(\\d{4})_Q(\\d)|H-1B_Disclosure_Data_FY(\\d{4}))\\.xlsx$"
  hrefs <- unique(hrefs[grepl(pattern, hrefs)])
  if (!length(hrefs)) {
    cli_alert_warning("Could not parse DOL page - using fallback manifest")
    hrefs <- SOURCES$dol_fallback
  }
  fy <- as.integer(sub(".*FY(\\d{4}).*", "\\1", hrefs))
  hrefs <- hrefs[fy >= ANALYSIS$first_fiscal_year]
  # Normalise to absolute URLs (the page mixes relative and absolute links)
  urls <- ifelse(grepl("^https?://", hrefs), hrefs, paste0(SOURCES$dol_base, hrefs))
  urls <- sub("(https?://[^/]+)//+", "\\1/", urls)
  unique(urls)
}

lca_urls <- discover_lca_links()
cli_h1("DOL LCA disclosure files ({length(lca_urls)})")
manifest <- rbindlist(lapply(lca_urls, function(u) {
  dest <- file.path(PATHS$raw_lca, basename(u))
  download_cached(u, dest)
  data.table(source = "dol_lca", url = u, file = dest, bytes = file.size(dest))
}))

# ---- Indeed Hiring Lab -------------------------------------------------------
cli_h1("Indeed Hiring Lab")
manifest <- rbind(manifest, rbindlist(lapply(names(SOURCES$indeed), function(nm) {
  dest <- file.path(PATHS$raw_indeed, paste0(nm, ".csv"))
  # Small files that change weekly: always refresh
  unlink(dest)
  download_cached(SOURCES$indeed[[nm]], dest, check_size = FALSE)
  data.table(source = "indeed", url = SOURCES$indeed[[nm]], file = dest, bytes = file.size(dest))
})))

# ---- FRED ------------------------------------------------------------------
cli_h1("FRED")
manifest <- rbind(manifest, rbindlist(lapply(names(SOURCES$fred), function(nm) {
  dest <- file.path(PATHS$raw_fred, paste0(nm, ".csv"))
  unlink(dest)
  download_cached(SOURCES$fred[[nm]], dest, check_size = FALSE)
  data.table(source = "fred", url = SOURCES$fred[[nm]], file = dest, bytes = file.size(dest))
})))

manifest[, downloaded_at := format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")]
fwrite(manifest, file.path(PATHS$raw, "download_manifest.csv"))
cli_alert_success("Downloaded {nrow(manifest)} files ({round(sum(manifest$bytes) / 1e9, 2)} GB)")
