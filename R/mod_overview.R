# -----------------------------------------------------------------------------
# Overview: headline numbers and the biggest trends, no filters.
# -----------------------------------------------------------------------------

overview_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    page_intro(
      "Pay & hiring trends, 2019 to now",
      paste("Offered salaries from U.S. Department of Labor H-1B/LCA filings - real, certified job offers",
            "with job title, salary, location and employer - next to market-wide job-postings data from",
            "Indeed Hiring Lab and BLS. Every number below links to an explorer page with full detail.")
    ),
    shiny::uiOutput(ns("headline")),
    bslib::card(
      bslib::card_header("What the numbers say"),
      shiny::uiOutput(ns("insights"))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("Pay change by role family", ns("family_plot"), ns("family_table"),
                 subtitle = shiny::textOutput(ns("family_sub"), inline = TRUE), height = "520px",
                 note = "Median offered salary, all seniority levels, nominal dollars."),
      chart_card("Hiring demand vs. before the pandemic", ns("postings_plot"), ns("postings_table"),
                 subtitle = shiny::textOutput(ns("postings_sub"), inline = TRUE), height = "520px",
                 note = "Indeed job postings index, monthly average, seasonally adjusted. 100 = level on Feb 1, 2020.")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("Median salary, all roles", ns("real_plot"), ns("real_table"),
                 subtitle = "Nominal vs. inflation-adjusted", height = "360px",
                 note = shiny::textOutput(ns("real_note"), inline = TRUE)),
      chart_card("Share of filings for entry-level jobs", ns("entry_plot"), ns("entry_table"),
                 subtitle = "By job title and by DOL wage level", height = "360px",
                 note = "Job-title basis: Intern, Junior, Associate, Graduate, level I titles. DOL basis: prevailing-wage Level I.")
    )
  )
}

overview_server <- function(id, d) {
  shiny::moduleServer(id, function(input, output, session) {
    meta <- d$meta
    full_year <- as.integer(meta$lca$latest_full_year)
    partial_year <- as.integer(meta$lca$partial_year)
    base_year <- 2019L
    tr <- d$trends

    all_rows <- tr[tr$family_id == "all" & tr$role_id == "all" & tr$filing == "all" &
                     tr$level_basis == "all", ]
    hl <- series_headline(all_rows, base_year, full_year, partial_year)
    real_rows <- apply_dollars(all_rows, d$cpi, "real")
    hl_real <- series_headline(real_rows, base_year, full_year, partial_year)

    # Entry-level shares (title basis and DOL basis)
    share_of <- function(basis, level) {
      t <- tr[tr$family_id == "all" & tr$role_id == "all" & tr$filing == "all" &
                tr$level_basis == basis & tr$period_type == "year", ]
      tot <- stats::aggregate(n ~ period, t, sum)
      lv <- t[t$level_id == level, c("period", "n")]
      out <- merge(lv, tot, by = "period", suffixes = c("", "_total"))
      out$share <- out$n / out$n_total
      out
    }
    entry_title <- share_of("title", "entry")
    entry_dol <- share_of("dol", "dol_1")
    share_at <- function(df, y) df$share[df$period == as.character(y)][1]

    # Market context
    pst <- d$postings
    us_post <- pst[pst$sector == "All postings (US)", ]
    us_post <- us_post[order(us_post$month), ]
    last_month <- utils::tail(us_post$month, 1)
    yr_ago <- format(as.Date(paste0(last_month, "-01")) - 365, "%Y-%m")
    post_now <- utils::tail(us_post$index, 1)
    post_yr_ago <- us_post$index[us_post$month == yr_ago][1]
    wg <- d$wages[d$wages$sector == "All postings (US)", ]
    wg <- wg[order(wg$month), ]
    wg_last <- utils::tail(wg[!is.na(wg$yoy_3mo), ], 1)
    jl <- d$jolts[order(d$jolts$month), ]
    jl_last <- utils::tail(jl, 1)
    jl_2019 <- mean(jl$openings_k[substr(jl$month, 1, 4) == "2019"])

    month_label <- function(m) format(as.Date(paste0(m, "-01")), "%b %Y")

    output$headline <- shiny::renderUI({
      htmltools::div(
        class = "headline-grid",
        hero_figure(
          sprintf("Median offered salary, all roles, %d", full_year),
          fmt_dollar(hl$latest, compact = FALSE),
          htmltools::HTML(sprintf(
            "%s vs %d &middot; %s since %d &middot; <b>%s</b> after inflation",
            fmt_pct(hl$yoy), full_year - 1, fmt_pct(hl$since_base), base_year,
            fmt_pct(hl_real$since_base)))
        ),
        stat_row(
          stat_tile(sprintf("%s so far", year_label(partial_year, meta)), fmt_dollar(hl$ytd),
                    delta = hl$ytd_yoy, delta_label = sprintf("vs same months of %d", partial_year - 1)),
          stat_tile(sprintf("Certified filings, %d", full_year), fmt_compact_int(hl$n_latest),
                    delta = pct_change(hl$n_latest, hl$n_base), delta_label = sprintf("vs %d", base_year),
                    neutral = TRUE),
          stat_tile(sprintf("Entry-level share of filings, %d", full_year),
                    fmt_pct(share_at(entry_title, full_year), signed = FALSE),
                    delta = share_at(entry_title, full_year) - share_at(entry_title, base_year),
                    delta_text = sprintf("%+.1f pts", 100 * (share_at(entry_title, full_year) - share_at(entry_title, base_year))),
                    delta_label = sprintf("vs %d", base_year), neutral = TRUE),
          stat_tile(sprintf("Indeed job postings, %s", month_label(last_month)),
                    formatC(post_now, format = "f", digits = 1),
                    delta = pct_change(post_now, post_yr_ago), delta_label = "vs a year earlier",
                    help = "Index, Feb 1 2020 = 100"),
          if (nrow(wg_last)) stat_tile(sprintf("Posted wage growth, %s", month_label(wg_last$month)),
                                       fmt_pct(wg_last$yoy_3mo), help = "Year over year, 3-month average (Indeed)"),
          stat_tile(sprintf("U.S. job openings, %s", month_label(jl_last$month)),
                    paste0(formatC(jl_last$openings_k / 1000, format = "f", digits = 1), "M"),
                    delta = pct_change(jl_last$openings_k, jl_2019), delta_label = "vs 2019 average",
                    neutral = TRUE, help = "BLS JOLTS, seasonally adjusted")
        )
      )
    })

    # ---- Insights ----------------------------------------------------------------
    roles_change <- role_changes(tr, base_year, full_year)
    roles_change <- roles_change[!is.na(roles_change$change) & roles_change$n >= 500 &
                                   roles_change$n_from >= 500 & roles_change$role_id != "other", ]
    roles_change$label <- d$taxonomy$role_name[match(roles_change$role_id, d$taxonomy$role_id)]
    top_up <- utils::head(roles_change[order(-roles_change$change), ], 3)
    top_down <- utils::head(roles_change[order(roles_change$change), ], 3)
    fmt_roles <- function(x) paste(sprintf("%s (%s)", x$label, fmt_pct(x$change)), collapse = ", ")

    ladder_gap <- function(y) {
      t <- tr[tr$family_id == "all" & tr$role_id == "all" & tr$filing == "all" & tr$level_basis == "title" &
                tr$period_type == "year" & tr$period == as.character(y), ]
      pct_change(t$p50[t$level_id == "senior"][1], t$p50[t$level_id == "entry"][1])
    }
    sector_idx <- function(s) {
      x <- pst[pst$sector == s, ]
      utils::tail(x$index[order(x$month)], 1)
    }

    output$insights <- shiny::renderUI({
      insight_list(list(
        sprintf(paste("Across all roles, the median offered salary went from <b>%s</b> in %d to <b>%s</b> in %d:",
                      "%s in nominal terms, <b>%s</b> after inflation."),
                fmt_dollar(hl$base, FALSE), base_year, fmt_dollar(hl$latest, FALSE), full_year,
                fmt_pct(hl$since_base), fmt_pct(hl_real$since_base)),
        if (nrow(top_up)) sprintf("Fastest pay growth since %d (roles with 500+ filings): %s.", base_year, fmt_roles(top_up)),
        if (nrow(top_down)) sprintf("Slowest: %s.", fmt_roles(top_down)),
        sprintf("Entry-level job titles made up <b>%s</b> of filings in %d versus %s in %d; DOL Level I (entry) wage filings went from %s to %s.",
                fmt_pct(share_at(entry_title, full_year), signed = FALSE), full_year,
                fmt_pct(share_at(entry_title, base_year), signed = FALSE), base_year,
                fmt_pct(share_at(entry_dol, base_year), signed = FALSE),
                fmt_pct(share_at(entry_dol, full_year), signed = FALSE)),
        sprintf("Senior titles were offered <b>%s</b> more than entry-level titles in %d (median vs median), compared with %s in %d.",
                fmt_pct(ladder_gap(full_year), signed = FALSE), full_year,
                fmt_pct(ladder_gap(base_year), signed = FALSE), base_year),
        sprintf(paste("Indeed software-development postings are at <b>%.0f</b> and data & analytics at",
                      "<b>%.0f</b> (Feb 2020 = 100), versus %.0f for all U.S. postings (%s)."),
                sector_idx("Software Development"), sector_idx("Data & Analytics"), post_now, month_label(last_month)),
        if (!is.na(hl$ytd_yoy)) sprintf("%s: median offered salary %s, %s versus the same months of %d.",
                year_label(partial_year, meta), fmt_dollar(hl$ytd, FALSE), fmt_pct(hl$ytd_yoy), partial_year - 1)
      ))
    })

    # ---- Family dumbbell ----------------------------------------------------------
    fam <- role_changes(tr, base_year, full_year, view = "family")
    fam$label <- d$families$family_name[match(fam$family_id, d$families$family_id)]
    fam_df <- data.frame(label = fam$label, from = fam$p50_from, to = fam$p50, n = fam$n,
                         change = fam$change, stringsAsFactors = FALSE)
    output$family_sub <- shiny::renderText(sprintf("Median offered salary, %d vs %d", base_year, full_year))
    output$family_plot <- plotly::renderPlotly(chart_dumbbell(fam_df, as.character(base_year), as.character(full_year)))
    output$family_table <- reactable::renderReactable(data_table(
      fam_df[order(-fam_df$to), c("label", "from", "to", "change", "n")],
      columns = list(label = reactable::colDef(name = "Role family", minWidth = 200),
                     from = col_dollar(paste("Median", base_year)), to = col_dollar(paste("Median", full_year)),
                     change = col_pct("Change"), n = col_int(paste("Filings", full_year)))
    ))

    # ---- Postings diverging bars -----------------------------------------------------
    last <- pst[pst$month == last_month & !is.na(pst$family_id) & pst$family_id != "all", ]
    output$postings_sub <- shiny::renderText(sprintf("Indeed postings by occupational sector, %s", month_label(last_month)))
    output$postings_plot <- plotly::renderPlotly(chart_diverging_bars(
      last$sector, last$index, 100,
      sprintf("<b>%.1f</b>  %s<br>%s vs Feb 2020", last$index, htmltools::htmlEscape(last$sector),
              fmt_pct(last$index / 100 - 1)),
      x_title = "Index (Feb 1, 2020 = 100)"))
    output$postings_table <- reactable::renderReactable(data_table(
      transform(last[order(-last$index), c("sector", "index")], vs_2020 = index / 100 - 1),
      columns = list(sector = reactable::colDef(name = "Sector", minWidth = 220),
                     index = reactable::colDef(name = "Index", format = reactable::colFormat(digits = 1)),
                     vs_2020 = col_pct("vs Feb 2020"))
    ))

    # ---- Nominal vs real ---------------------------------------------------------------
    yr <- all_rows[all_rows$period_type == "year", ]
    yr_real <- real_rows[real_rows$period_type == "year", ]
    trend_df <- rbind(
      data.frame(series = "nominal", series_label = "Nominal", x = as.integer(yr$period),
                 p25 = yr$p25, p50 = yr$p50, p75 = yr$p75, n = yr$n),
      data.frame(series = "real", series_label = sprintf("In %s dollars", meta$market$cpi_base_year),
                 x = as.integer(yr_real$period), p25 = yr_real$p25, p50 = yr_real$p50, p75 = yr_real$p75, n = yr_real$n)
    )
    trend_df$is_partial <- trend_df$x == partial_year & as.integer(meta$lca$partial_year_months) < 12
    ticks <- list(values = sort(unique(trend_df$x)), labels = year_label(sort(unique(trend_df$x)), meta))
    output$real_note <- shiny::renderText(sprintf(
      "Hollow marker: %s is year to date. Inflation adjustment uses CPI-U averaged over the same months.",
      partial_year))
    output$real_plot <- plotly::renderPlotly(chart_trend(
      trend_df, c(nominal = CATEGORICAL[1], real = CATEGORICAL[2]), x_ticks = ticks))
    output$real_table <- reactable::renderReactable(data_table(
      data.frame(year = year_label(as.integer(yr$period), meta), nominal = yr$p50,
                 real = yr_real$p50[match(yr$period, yr_real$period)], n = yr$n),
      columns = list(year = reactable::colDef(name = "Year"), nominal = col_dollar("Nominal median"),
                     real = col_dollar(sprintf("Median in %s dollars", meta$market$cpi_base_year)),
                     n = col_int("Filings"))
    ))

    # ---- Entry share ---------------------------------------------------------------------
    es <- rbind(
      data.frame(series = "title", series_label = "Entry-level job title", x = as.integer(entry_title$period),
                 value = entry_title$share, n = entry_title$n),
      data.frame(series = "dol", series_label = "DOL wage Level I", x = as.integer(entry_dol$period),
                 value = entry_dol$share, n = entry_dol$n)
    )
    output$entry_plot <- plotly::renderPlotly({
      p <- chart_lines(es, c(title = CATEGORICAL[1], dol = CATEGORICAL[2]), "Share of filings",
                       y_format = ".0%", hover_fmt = function(v) fmt_pct(v, signed = FALSE))
      plotly::layout(p, xaxis = list(tickvals = ticks$values, ticktext = ticks$labels))
    })
    output$entry_table <- reactable::renderReactable(data_table(
      es[, c("series_label", "x", "value", "n")],
      columns = list(series_label = reactable::colDef(name = "Basis"), x = reactable::colDef(name = "Year"),
                     value = col_pct("Share", signed = FALSE), n = col_int("Filings"))
    ))
  })
}
