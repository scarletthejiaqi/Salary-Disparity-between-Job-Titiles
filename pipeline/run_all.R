# -----------------------------------------------------------------------------
# Run the full data pipeline. Each step runs in its own R process so memory is
# released between steps. Steps cache their outputs, so re-running is cheap.
#
#   Rscript pipeline/run_all.R            # all steps
#   Rscript pipeline/run_all.R 3 4 5      # only the listed steps
#
# The extraction step is memory-hungry (readxl parses each spreadsheet fully in
# memory, ~10-20 GB for the largest DOL files). It runs one file at a time by
# default; set PIPELINE_CORES=2+ only on machines with plenty of RAM.
# -----------------------------------------------------------------------------

steps <- c(
  "1" = "01_download.R",
  "2" = "02_extract_lca.R",
  "3" = "03_clean_lca.R",
  "4" = "04_build_tables.R",
  "5" = "05_market_data.R"
)
args <- commandArgs(trailingOnly = TRUE)
selected <- if (length(args)) steps[args] else steps
if (anyNA(selected)) stop("Unknown step; choose from ", paste(names(steps), collapse = ", "))

rscript <- file.path(R.home("bin"), "Rscript")
for (s in selected) {
  message("\n==> ", s)
  t0 <- Sys.time()
  status <- system2(rscript, file.path("pipeline", s))
  if (status != 0) stop("Pipeline step failed: ", s)
  message(sprintf("<== %s finished in %.1f min", s, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
message("\nPipeline complete. App data is in data/.")
