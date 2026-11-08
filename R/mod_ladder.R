# -----------------------------------------------------------------------------
# Seniority ladder: the pay gap between levels within a role, how the gap
# moved since 2019, and how the seniority mix of filings shifted.
# -----------------------------------------------------------------------------

ladder_ui <- function(id, d) {
  ns <- shiny::NS(id)
  years <- sort(unique(as.integer(d$trends$period[d$trends$period_type == "year"])), decreasing = TRUE)
  htmltools::tagList(
    page_intro("Seniority ladder",
               "How much more each step up the ladder pays, and whether those gaps are widening or narrowing."),
    filter_row(
      shiny::selectizeInput(ns("subject"), "Role", choices = subject_choices(d$taxonomy),
                            selected = subject_key("software", "swe"), width = "300px"),
      shiny::selectInput(ns("year"), "Year", stats::setNames(years, year_label(years, d$meta)),
                         selected = d$meta$lca$latest_full_year, width = "170px"),
      shiny::radioButtons(ns("basis"), "Seniority from", c("Job title" = "title", "DOL wage level" = "dol"),
                          inline = TRUE),
      shiny::selectInput(ns("filing"), "Filings", c("All certified" = "all", "New hires only" = "new_hire"),
                         width = "160px"),
      shiny::selectInput(ns("dollars"), "Dollars", c("Nominal" = "nominal", "Inflation-adjusted" = "real"),
                         width = "170px")
    ),
    bslib::card(bslib::card_body(shiny::uiOutput(ns("insight")))),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card(shiny::textOutput(ns("ladder_title"), inline = TRUE), ns("ladder_plot"), ns("ladder_table"),
                 subtitle = "Bar: middle 50% of offers · dot: median", height = "400px",
                 note = shiny::textOutput(ns("ladder_note"), inline = TRUE)),
      chart_card("Premium over the entry level", ns("premium_plot"), ns("premium_table"),
                 subtitle = "Median of each level vs. the entry-level median, by year", height = "400px",
                 note = "A rising line means that level's pay pulled further ahead of entry-level pay.")
    ),
    chart_card("Seniority mix of filings", ns("mix_plot"), ns("mix_table"),
               subtitle = "Share of certified filings at each level", height = "380px",
               note = "Titles without a seniority keyword count as Mid / Unspecified.")
  )
}

ladder_server <- function(id, d) {
  shiny::moduleServer(id, function(input, output, session) {
    meta <- d$meta
    tr <- d$trends

    rows <- shiny::reactive({
      shiny::req(input$subject)
      r <- filter_subject(tr, input$subject)
      r <- r[r$period_type == "year" & r$filing == input$filing & r$level_basis == input$basis, ]
      lv <- d$levels[d$levels$basis == input$basis, ]
      r$level_name <- lv$level_name[match(r$level_id, lv$level_id)]
      r$level_order <- lv$level_order[match(r$level_id, lv$level_id)]
      apply_dollars(r, d$cpi, input$dollars)
    })
    ref_level <- shiny::reactive(if (input$basis == "title") "entry" else "dol_1")

    year_rows <- shiny::reactive({
      r <- rows()
      r <- r[r$period == as.character(input$year), ]
      r <- r[order(r$level_order), ]
      r$premium <- level_premium(r, ref_level())
      r
    })

    output$ladder_title <- shiny::renderText(sprintf("Pay ladder, %s", year_label(as.integer(input$year), meta)))
    output$ladder_note <- shiny::renderText(dollar_note(input$dollars, meta))
    output$ladder_plot <- plotly::renderPlotly(chart_ladder(year_rows()))
    output$ladder_table <- reactable::renderReactable(data_table(
      year_rows()[, c("level_name", "n", "p25", "p50", "p75", "premium")],
      columns = list(level_name = reactable::colDef(name = "Level", minWidth = 180), n = col_int("Filings"),
                     p25 = col_dollar("25th pct"), p50 = col_dollar("Median"), p75 = col_dollar("75th pct"),
                     premium = col_pct("vs entry")),
      pagination = FALSE
    ))

    premium_rows <- shiny::reactive({
      r <- rows()
      out <- do.call(rbind, lapply(split(r, r$period), function(x) {
        x$premium <- level_premium(x, ref_level())
        x
      }))
      out <- out[out$level_id != ref_level() & !is.na(out$premium), ]
      out$x <- as.integer(out$period)
      out
    })
    output$premium_plot <- plotly::renderPlotly({
      r <- premium_rows()
      if (!nrow(r)) return(empty_plot())
      r$series <- r$level_id
      r$series_label <- r$level_name
      r$value <- r$premium
      yrs <- sort(unique(r$x))
      p <- chart_lines(r[order(r$level_order), ], LEVEL_COLORS, "Premium over entry level", y_format = "+.0%",
                       hover_fmt = function(v) fmt_pct(v))
      plotly::layout(p, xaxis = list(tickvals = yrs, ticktext = year_label(yrs, meta)))
    })
    output$premium_table <- reactable::renderReactable({
      r <- premium_rows()
      data_table(r[order(r$level_order, r$x), c("level_name", "period", "p50", "premium")],
                 columns = list(level_name = reactable::colDef(name = "Level"), period = reactable::colDef(name = "Year"),
                                p50 = col_dollar("Median"), premium = col_pct("vs entry")))
    })

    mix_rows <- shiny::reactive({
      r <- level_mix(rows())
      r$x_label <- year_label(as.integer(r$period), meta)
      r[order(r$period, r$level_order), ]
    })
    output$mix_plot <- plotly::renderPlotly({
      lv <- d$levels[d$levels$basis == input$basis, ]
      r <- mix_rows()
      p <- chart_level_mix(r, lv)
      plotly::layout(p, xaxis = list(categoryorder = "array", categoryarray = unique(r$x_label)))
    })
    output$mix_table <- reactable::renderReactable(data_table(
      mix_rows()[, c("x_label", "level_name", "n", "share")],
      columns = list(x_label = reactable::colDef(name = "Year"), level_name = reactable::colDef(name = "Level"),
                     n = col_int("Filings"), share = col_pct("Share", signed = FALSE))
    ))

    output$insight <- shiny::renderUI({
      r <- rows()
      y <- as.character(input$year)
      top <- if (input$basis == "title") "senior" else "dol_3"
      gap <- function(yy) {
        x <- r[r$period == yy, ]
        pct_change(x$p50[x$level_id == top][1], x$p50[x$level_id == ref_level()][1])
      }
      nm <- subject_label(input$subject, d$taxonomy)
      top_name <- d$levels$level_name[d$levels$level_id == top]
      ref_name <- d$levels$level_name[d$levels$level_id == ref_level()]
      g_now <- gap(y)
      g_base <- gap("2019")
      mix <- mix_rows()
      entry_share <- function(yy) mix$share[mix$period == yy & mix$level_id == ref_level()][1]
      insight_list(list(
        if (!is.na(g_now)) sprintf("In %s, %s filings at <b>%s</b> were offered a median <b>%s</b> more than at <b>%s</b>%s.",
                                   year_label(as.integer(y), meta), htmltools::htmlEscape(nm), top_name, fmt_pct(g_now, signed = FALSE), ref_name,
                                   if (!is.na(g_base) && y != "2019") sprintf(" (the gap was %s in 2019)", fmt_pct(g_base, signed = FALSE)) else ""),
        if (!is.na(entry_share(y))) sprintf("%s made up <b>%s</b> of these filings in %s%s.", ref_name,
                                            fmt_pct(entry_share(y), signed = FALSE), year_label(as.integer(y), meta),
                                            if (!is.na(entry_share("2019")) && y != "2019")
                                              sprintf(", versus %s in 2019", fmt_pct(entry_share("2019"), signed = FALSE)) else "")
      ))
    })
  })
}
