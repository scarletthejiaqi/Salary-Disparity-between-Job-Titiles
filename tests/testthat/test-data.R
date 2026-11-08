# Data-quality checks on the published tables in data/ (run after `make data`).
skip_if_not(has_app_data(), "data/ tables not built yet")

d <- withr::with_dir(app_root(), load_app_data("data"))

test_that("salary statistics are ordered and suppressed below the publication threshold", {
  t <- d$trends[!is.na(d$trends$p50), ]
  expect_true(all(t$n >= d$meta$lca$min_n_publish))
  expect_true(all(t$p10 <= t$p25 & t$p25 <= t$p50 & t$p50 <= t$p75 & t$p75 <= t$p90))
  expect_true(all(t$p50 >= 25000 & t$p50 <= 1e6))
})

test_that("every year from 2019 to the latest is covered", {
  yrs <- as.integer(unique(d$trends$period[d$trends$period_type == "year"]))
  expect_equal(sort(yrs), 2019:as.integer(d$meta$lca$partial_year))
  all_roles <- d$trends[d$trends$family_id == "all" & d$trends$filing == "all" &
                          d$trends$level_basis == "all" & d$trends$period_type == "year", ]
  expect_true(all(all_roles$n > 100000))
})

test_that("roles, families and levels match the taxonomy", {
  t <- d$trends
  expect_true(all(t$role_id %in% c("all", d$taxonomy$role_id)))
  expect_true(all(t$family_id %in% c("all", d$families$family_id)))
  expect_true(all(t$level_id %in% c("all", d$levels$level_id)))
})

test_that("state, employer and city tables respect their limits", {
  expect_true(all(d$states$n >= 30))
  expect_true(all(d$states$state %in% c(datasets::state.abb, "DC", "PR", "GU", "VI", "MP", "AS")))
  per_group <- stats::aggregate(n ~ year + family_id + role_id, data = d$employers, FUN = length)
  expect_true(all(per_group$n <= 25))
  expect_true(all(d$employers$n >= 10))
})

test_that("market context tables are complete", {
  expect_true(all(d$postings$index > 0))
  expect_true("All postings (US)" %in% d$postings$sector)
  expect_true("All postings (US)" %in% d$wages$sector)
  base <- d$cpi[d$cpi$period_type == "year" & d$cpi$period == as.character(d$meta$market$cpi_base_year), ]
  expect_equal(base$real_factor, 1)
})

test_that("seniority medians rise with seniority overall", {
  t <- d$trends[d$trends$family_id == "all" & d$trends$filing == "all" & d$trends$period_type == "year" &
                  d$trends$period == as.character(d$meta$lca$latest_full_year), ]
  title <- t[t$level_basis == "title", ]
  med <- title$p50[match(c("entry", "mid", "senior", "lead", "director"), title$level_id)]
  expect_true(all(diff(med) > 0))
  dol <- t[t$level_basis == "dol", ]
  expect_true(all(diff(dol$p50[match(c("dol_1", "dol_2", "dol_3", "dol_4"), dol$level_id)]) > 0))
})
