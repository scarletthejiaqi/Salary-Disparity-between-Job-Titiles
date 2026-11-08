# Load the pipeline's pure helper functions for unit testing.
local({
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  lib  <- file.path(root, "pipeline", "lib")
  for (f in sort(list.files(lib, pattern = "\\.R$", full.names = TRUE))) {
    sys.source(f, envir = globalenv())
  }
})

classify_titles <- function(titles, soc) {
  classify_role(normalize_title(titles), normalize_soc(soc))$role_id
}
