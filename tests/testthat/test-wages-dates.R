test_that("parse_money handles currency formatting and blanks", {
  expect_equal(parse_money(c("$85,000.00", "85000", " 42.5 ", "", NA, "N/A")),
               c(85000, 85000, 42.5, NA, NA, NA))
})

test_that("annualize_wage applies DOL unit conventions", {
  expect_equal(annualize_wage(c(100000, 50, 2000, 4000, 8000), c("Year", "Hour", "Week", "Bi-Weekly", "Month")),
               c(100000, 104000, 104000, 104000, 96000))
  expect_true(is.na(annualize_wage(100, "Fortnight")))
})

test_that("plausible_wage drops obvious unit errors", {
  expect_equal(plausible_wage(c(10000, 60000, 950000, 5e6, NA)), c(FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("parse_excel_date reads serials, ISO and US formats", {
  expect_equal(parse_excel_date(c("45930", "2025-09-30", "2025-09-30 00:00:00", "9/30/2025", "", NA)),
               as.Date(c("2025-09-30", "2025-09-30", "2025-09-30", "2025-09-30", NA, NA)))
})

test_that("half_year_label splits the calendar year", {
  expect_equal(half_year_label(as.Date(c("2024-01-01", "2024-06-30", "2024-07-01"))),
               c("2024-H1", "2024-H1", "2024-H2"))
})

test_that("normalize_state accepts abbreviations and names", {
  expect_equal(normalize_state(c("ca", "New York", " DC ", "ZZ", NA)), c("CA", "NY", "DC", NA, NA))
})
