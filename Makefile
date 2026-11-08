.PHONY: help setup data data-tables test lint run manifest deploy

help:          ## Show available commands
	@grep -E '^[a-z-]+:.*##' $(MAKEFILE_LIST) | awk -F':.*## ' '{printf "  %-12s %s\n", $$1, $$2}'

setup:         ## Install the locked R package versions (renv)
	Rscript -e 'renv::restore(prompt = FALSE)'

data:          ## Download sources and rebuild every table in data/ (slow, ~3 GB download)
	nice -n 10 Rscript pipeline/run_all.R

data-tables:   ## Rebuild data/ from already-extracted files (steps 3-5)
	nice -n 10 Rscript pipeline/run_all.R 3 4 5

test:          ## Run unit and app tests
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

lint:          ## Static checks with lintr
	Rscript -e 'lints <- c(lintr::lint_dir("R"), lintr::lint_dir("pipeline"), lintr::lint("app.R")); print(lints); quit(status = length(lints) > 0)'

run:           ## Start the dashboard locally
	Rscript -e 'shiny::runApp(".", port = 8080)'

manifest:      ## Refresh manifest.json for Posit Connect Cloud (run after changing packages or data)
	Rscript -e 'rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")'

deploy:        ## Deploy to shinyapps.io (requires rsconnect account setup)
	Rscript -e 'rsconnect::deployApp(appName = "Salary_Disparity_between_Job_Titles")'
