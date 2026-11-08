# Render every page's outputs with the real published data (no browser needed).
skip_if_not(has_app_data(), "data/ tables not built yet")

d <- withr::with_dir(app_root(), load_app_data("data"))

expect_renders <- function(server, outputs, inputs = list()) {
  withr::with_dir(app_root(), shiny::testServer(server, args = list(d = d), {
    if (length(inputs)) do.call(session$setInputs, inputs)
    for (o in outputs) expect_no_error(output[[o]], message = o)
  }))
}

test_that("published tables have the expected shape", {
  expect_true(all(c("period_type", "period", "filing", "family_id", "role_id", "level_basis", "level_id",
                    "n", "p50") %in% names(d$trends)))
  expect_true(all(d$trends$n > 0))
  expect_true(all(d$trends$p50[d$trends$n < 30] %in% NA))
  expect_true(all(d$taxonomy$role_id %in% c(unique(d$trends$role_id), "other")))
})

test_that("overview renders", {
  expect_renders(overview_server, c("headline", "insights", "family_plot", "postings_plot", "real_plot",
                                    "entry_plot", "family_table"))
})

test_that("salary explorer renders in both comparison modes", {
  expect_renders(explorer_server, c("trend_plot", "tiles", "summary", "titles", "trend_table"),
                 list(compare = "levels", subject = "software|swe", basis = "title", filing = "all",
                      dollars = "real", grain = "year", band = TRUE))
  expect_renders(explorer_server, c("trend_plot", "tiles", "summary", "titles"),
                 list(compare = "roles", roles = c("software|swe", "data_ai|data_scientist"),
                      level = "title|senior", filing = "new_hire", dollars = "nominal", grain = "half",
                      band = FALSE))
})

test_that("seniority ladder renders", {
  expect_renders(ladder_server, c("ladder_plot", "premium_plot", "mix_plot", "insight", "ladder_table"),
                 list(subject = "data_ai|all", year = d$meta$lca$latest_full_year, basis = "dol",
                      filing = "all", dollars = "nominal"))
})

test_that("compare roles renders", {
  expect_renders(compare_server, c("dumbbell_plot", "scatter_plot", "ranking"),
                 list(family = "all", level = "all|all", year = d$meta$lca$latest_full_year,
                      filing = "all", dollars = "real", min_n = 100))
})

test_that("hiring demand renders", {
  expect_renders(market_server, c("tiles", "postings_plot", "wages_plot", "volume_plot", "jolts_plot"),
                 list(family = "data_ai", range = "2019-01"))
})

test_that("employers & locations renders", {
  expect_renders(places_server, c("emp_plot", "state_plot", "city_plot", "emp_table", "state_table"),
                 list(subject = "software|swe", year = d$meta$lca$latest_full_year, level = "senior",
                      dollars = "nominal"))
})

test_that("methodology renders", {
  expect_renders(methodology_server, c("vintage", "funnel", "taxonomy", "crosswalk"))
})
