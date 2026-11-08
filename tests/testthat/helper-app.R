# Load the app's R/ helpers (Shiny sources these automatically when running).
local({
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  for (f in sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))) {
    sys.source(f, envir = globalenv())
  }
})

app_root <- function() normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
has_app_data <- function() file.exists(file.path(app_root(), "data", "salary_trends.csv"))
