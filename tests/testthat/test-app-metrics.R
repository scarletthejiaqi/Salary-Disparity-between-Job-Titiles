test_that("formatters produce readable, signed output", {
  expect_equal(fmt_dollar(c(128400, 1250000, NA)), c("$128K", "$1.25M", "—"))
  expect_equal(fmt_dollar(128400.4, compact = FALSE), "$128,400")
  expect_equal(fmt_pct(c(0.0421, -0.05, 0, NA)), c("+4.2%", "−5.0%", "0.0%", "—"))
  expect_equal(fmt_pct(0.25, signed = FALSE), "25.0%")
  expect_equal(fmt_compact_int(c(950, 12500, 4.2e6)), c("950", "12K", "4.2M"))
})

test_that("pct_change and cagr handle edge cases", {
  expect_equal(pct_change(110, 100), 0.1)
  expect_true(is.na(pct_change(100, 0)))
  expect_equal(round(cagr(121, 100, 2), 6), 0.1)
  expect_true(is.na(cagr(100, 90, 0)))
})

test_that("series_headline compares full years and matching half-years", {
  df <- data.frame(
    period_type = c("year", "year", "year", "half", "half"),
    period = c("2019", "2024", "2025", "2025-H1", "2026-H1"),
    p50 = c(100, 110, 121, 118, 124), n = c(50, 60, 70, 30, 35),
    stringsAsFactors = FALSE
  )
  h <- series_headline(df, 2019, 2025, 2026)
  expect_equal(h$latest, 121)
  expect_equal(h$yoy, 0.1)
  expect_equal(h$since_base, 0.21)
  expect_equal(round(h$cagr, 4), round(1.21^(1 / 6) - 1, 4))
  expect_equal(round(h$ytd_yoy, 4), round(124 / 118 - 1, 4))
  expect_equal(h$n_latest, 70)
})

test_that("level_premium and level_mix are relative to the reference level", {
  df <- data.frame(period = "2025", level_id = c("entry", "mid", "senior"), p50 = c(100, 120, 150),
                   n = c(10, 30, 60), stringsAsFactors = FALSE)
  expect_equal(level_premium(df), c(0, 0.2, 0.5))
  expect_equal(level_mix(df)$share, c(0.1, 0.3, 0.6))
})

test_that("rolling_12 sums the trailing year", {
  x <- rep(1, 14)
  r <- rolling_12(x)
  expect_true(all(is.na(r[1:11])))
  expect_equal(r[12:14], c(12, 12, 12))
})

test_that("subjects round-trip between keys and labels", {
  tax <- data.frame(family_id = c("software", "software"), family_name = "Software Engineering",
                    role_id = c("swe", "qa"), role_name = c("Software Engineer", "QA / Test Engineer"),
                    stringsAsFactors = FALSE)
  keys <- c(subject_key("all", "all"), subject_key("software", "all"), subject_key("software", "qa"))
  expect_equal(subject_label(keys, tax), c("All roles", "All Software Engineering", "QA / Test Engineer"))
  ch <- subject_choices(tax)
  expect_equal(unname(ch[["Software Engineering"]]), c("software|all", "software|swe", "software|qa"))
})

test_that("apply_dollars rescales only in real mode", {
  df <- data.frame(period_type = "year", period = c("2019", "2025"), p50 = c(100, 100))
  cpi <- data.frame(period_type = "year", period = c("2019", "2025"), real_factor = c(1.25, 1))
  expect_equal(apply_dollars(df, cpi, "nominal")$p50, c(100, 100))
  expect_equal(apply_dollars(df, cpi, "real")$p50, c(125, 100))
})

test_that("charts build without error on minimal input", {
  df <- data.frame(series = "a", series_label = "A", x = 2019:2021, p25 = c(90, 95, 100),
                   p50 = c(100, 105, 110), p75 = c(110, 115, 120), n = 50, is_partial = c(FALSE, FALSE, TRUE))
  expect_s3_class(chart_trend(df, c(a = CATEGORICAL[1]), band = TRUE), "plotly")
  expect_s3_class(chart_dumbbell(data.frame(label = c("x", "y"), from = c(1, 2), to = c(2, 3)), "2019", "2025"), "plotly")
  expect_s3_class(chart_state_tiles(data.frame(state = "CA", n = 40, p25 = 1, p50 = 2, p75 = 3), c(CA = "California")), "plotly")
  expect_s3_class(chart_trend(df[0, ], c(a = "#000")), "plotly")
})
