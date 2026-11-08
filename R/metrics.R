# -----------------------------------------------------------------------------
# Derived metrics shared by the dashboard pages (pure functions, unit-tested).
# -----------------------------------------------------------------------------

pct_change <- function(new, old) {
  out <- new / old - 1
  out[!is.finite(out)] <- NA_real_
  out
}

cagr <- function(new, old, years) {
  out <- (new / old)^(1 / years) - 1
  out[!is.finite(out) | years <= 0] <- NA_real_
  out
}

#' Pull one statistic for a period from a trend table (NA when absent).
value_at <- function(df, period, stat = "p50", ptype = "year") {
  v <- df[[stat]][df$period_type == ptype & df$period == as.character(period)]
  if (length(v)) v[1] else NA_real_
}

#' Headline comparison for one series.
#'
#' @param df Trend rows for a single subject / level / filing type (both
#'   period types).
#' @param base_year First year of the comparison window (2019).
#' @param full_year Latest complete calendar year.
#' @param partial_year Latest (possibly partial) year; compared half-on-half.
#' @return Named list of values and changes.
series_headline <- function(df, base_year, full_year, partial_year, stat = "p50") {
  latest  <- value_at(df, full_year, stat)
  prev    <- value_at(df, full_year - 1, stat)
  base    <- value_at(df, base_year, stat)
  ytd     <- value_at(df, paste0(partial_year, "-H1"), stat, "half")
  ytd_ref <- value_at(df, paste0(partial_year - 1, "-H1"), stat, "half")
  list(
    latest = latest, prev = prev, base = base,
    yoy = pct_change(latest, prev),
    since_base = pct_change(latest, base),
    cagr = cagr(latest, base, full_year - base_year),
    ytd = ytd, ytd_yoy = pct_change(ytd, ytd_ref),
    n_latest = value_at(df, full_year, "n"),
    n_base = value_at(df, base_year, "n")
  )
}

#' Pay premium of each seniority level over a reference level.
level_premium <- function(df, ref = "entry", stat = "p50") {
  ref_v <- df[[stat]][df$level_id == ref]
  ref_v <- if (length(ref_v)) ref_v[1] else NA_real_
  pct_change(df[[stat]], ref_v)
}

#' Share of filings by level within each period.
level_mix <- function(df) {
  if (!nrow(df)) return(df)
  totals <- stats::aggregate(n ~ period, data = df, FUN = sum)
  df$share <- df$n / totals$n[match(df$period, totals$period)]
  df
}

#' Compare every role (or family) between two years for ranking tables.
role_changes <- function(trends, from_year, to_year, filing = "all", level = "all",
                         basis = "all", view = c("role", "family")) {
  view <- match.arg(view)
  t <- trends[trends$period_type == "year" & trends$filing == filing &
                trends$level_basis == basis & trends$level_id == level, ]
  t <- if (view == "role") t[t$role_id != "all" & t$family_id != "all", ] else
    t[t$role_id == "all" & t$family_id != "all", ]
  a <- t[t$period == as.character(from_year), c("family_id", "role_id", "n", "p50")]
  b <- t[t$period == as.character(to_year), c("family_id", "role_id", "n", "p25", "p50", "p75")]
  prev <- t[t$period == as.character(to_year - 1), c("family_id", "role_id", "p50")]
  names(a)[3:4] <- c("n_from", "p50_from")
  names(prev)[3] <- "p50_prev"
  out <- merge(b, a, by = c("family_id", "role_id"), all.x = TRUE)
  out <- merge(out, prev, by = c("family_id", "role_id"), all.x = TRUE)
  out$change <- pct_change(out$p50, out$p50_from)
  out$yoy <- pct_change(out$p50, out$p50_prev)
  out$cagr <- cagr(out$p50, out$p50_from, to_year - from_year)
  out$volume_change <- pct_change(out$n, out$n_from)
  out
}

#' Trailing 12-month sums for a monthly count series.
rolling_12 <- function(x) {
  if (length(x) < 12) return(rep(NA_real_, length(x)))
  out <- as.numeric(stats::filter(x, rep(1, 12), sides = 1))
  out
}
