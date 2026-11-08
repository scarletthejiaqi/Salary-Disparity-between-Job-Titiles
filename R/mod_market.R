# -----------------------------------------------------------------------------
# Hiring demand: job postings and posted-wage growth (Indeed Hiring Lab),
# DOL filing volume and BLS JOLTS job openings.
# -----------------------------------------------------------------------------

market_ui <- function(id, d) {
  ns <- shiny::NS(id)
  fams <- d$families[d$families$family_id != "other", ]
  htmltools::tagList(
    page_intro("Hiring demand",
               paste("Are employers posting more or fewer jobs, and are advertised wages still rising?",
                     "Market-wide postings from Indeed sit next to DOL filings and BLS job openings.")),
    filter_row(
      shiny::selectInput(ns("family"), "Role family", stats::setNames(fams$family_id, fams$family_name),
                         selected = "software", width = "260px"),
      shiny::selectInput(ns("range"), "Time range",
                         c("Since 2019" = "2019-01", "Since 2022" = "2022-01", "Last 24 months" = "last24"),
                         width = "170px")
    ),
    shiny::uiOutput(ns("tiles")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("Job postings on Indeed", ns("postings_plot"), ns("postings_table"),
                 subtitle = "Index, Feb 1 2020 = 100, seasonally adjusted (monthly average)", height = "400px",
                 note = "Gray line: all U.S. postings. Sectors are Indeed's occupational categories matched to this role family."),
      chart_card("Advertised wage growth", ns("wages_plot"), ns("wages_table"),
                 subtitle = "Year-over-year change in posted wages, 3-month average", height = "400px",
                 note = paste("Indeed Wage Tracker: median growth of advertised pay for the same job titles a year",
                              "apart, not seasonally adjusted. Indeed publishes sector detail for 19 sectors only;",
                              "if this family's sectors are not among them, only the U.S. total (gray) is shown."))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("DOL filings for this role family", ns("volume_plot"), ns("volume_table"),
                 subtitle = "Certified H-1B/LCA filings, trailing 12 months", height = "360px",
                 note = "New hires: new employment or a change of employer. Trailing sums remove the March cap-season spike."),
      chart_card("U.S. job openings (JOLTS)", ns("jolts_plot"), ns("jolts_table"),
                 subtitle = "All nonfarm industries, millions, seasonally adjusted", height = "360px",
                 note = "Source: BLS Job Openings and Labor Turnover Survey via FRED.")
    )
  )
}

market_server <- function(id, d) {
  shiny::moduleServer(id, function(input, output, session) {
    start_month <- shiny::reactive({
      if (input$range == "last24") {
        last <- max(d$postings$month)
        format(seq(as.Date(paste0(last, "-01")), by = "-23 months", length.out = 2)[2], "%Y-%m")
      } else {
        input$range
      }
    })
    sectors <- shiny::reactive({
      s <- unique(d$postings$sector[d$postings$family_id %in% input$family])
      utils::head(s, length(CATEGORICAL))
    })
    series_colors <- shiny::reactive({
      s <- sectors()
      c(stats::setNames(CATEGORICAL[seq_along(s)], s), "All postings (US)" = VIZ$context)
    })
    to_date <- function(m) as.Date(paste0(m, "-01"))

    postings <- shiny::reactive({
      p <- d$postings[d$postings$sector %in% c("All postings (US)", sectors()) & d$postings$month >= start_month(), ]
      data.frame(series = p$sector, series_label = p$sector, x = to_date(p$month), value = p$index,
                 month = p$month, stringsAsFactors = FALSE)
    })
    wages <- shiny::reactive({
      w <- d$wages[d$wages$sector %in% c("All postings (US)", sectors()) & d$wages$month >= start_month(), ]
      data.frame(series = w$sector, series_label = w$sector, x = to_date(w$month), value = w$yoy_3mo,
                 month = w$month, n_obs = w$n_obs, stringsAsFactors = FALSE)
    })

    output$tiles <- shiny::renderUI({
      p <- d$postings
      last <- max(p$month)
      yr_ago <- format(seq(to_date(last), by = "-12 months", length.out = 2)[2], "%Y-%m")
      idx <- function(s, m) p$index[p$sector == s & p$month == m][1]
      w <- d$wages[!is.na(d$wages$yoy_3mo), ]
      wlast <- max(w$month)
      tiles <- lapply(sectors(), function(s) {
        wg_s <- w$yoy_3mo[w$sector == s & w$month == wlast][1]
        stat_tile(sprintf("%s postings", s), formatC(idx(s, last), format = "f", digits = 1),
                  delta = pct_change(idx(s, last), idx(s, yr_ago)), delta_label = "vs a year earlier",
                  help = if (!is.na(wg_s)) sprintf("Feb 2020 = 100 · advertised wages %s YoY", fmt_pct(wg_s))
                         else "Feb 2020 = 100")
      })
      us <- stat_tile("All U.S. postings", formatC(idx("All postings (US)", last), format = "f", digits = 1),
                      delta = pct_change(idx("All postings (US)", last), idx("All postings (US)", yr_ago)),
                      delta_label = "vs a year earlier",
                      help = format(to_date(last), "Feb 2020 = 100 · %b %Y"))
      do.call(stat_row, c(tiles, list(us)))
    })

    output$postings_plot <- plotly::renderPlotly(
      chart_lines(postings(), series_colors(), "Index (Feb 2020 = 100)", ref = 100))
    output$postings_table <- reactable::renderReactable(data_table(
      postings()[order(postings()$series, postings()$month, decreasing = c(FALSE, TRUE), method = "radix"), c("series", "month", "value")],
      columns = list(series = reactable::colDef(name = "Sector", minWidth = 200), month = reactable::colDef(name = "Month"),
                     value = reactable::colDef(name = "Index", format = reactable::colFormat(digits = 1)))
    ))
    output$wages_plot <- plotly::renderPlotly(
      chart_lines(wages(), series_colors(), "Year-over-year growth", ref = 0, y_format = ".1%",
                  hover_fmt = function(v) fmt_pct(v)))
    output$wages_table <- reactable::renderReactable(data_table(
      wages()[order(wages()$series, wages()$month, decreasing = c(FALSE, TRUE), method = "radix"), c("series", "month", "value", "n_obs")],
      columns = list(series = reactable::colDef(name = "Sector", minWidth = 200), month = reactable::colDef(name = "Month"),
                     value = col_pct("YoY (3-mo avg)"), n_obs = col_int("Postings sampled"))
    ))

    volume <- shiny::reactive({
      v <- d$volume[d$volume$family_id == input$family, ]
      v <- v[order(v$month), ]
      v$all_12 <- rolling_12(v$n)
      v$new_12 <- rolling_12(v$n_new_hire)
      v <- v[v$month >= max(start_month(), "2019-12"), ]
      v
    })
    output$volume_plot <- plotly::renderPlotly({
      v <- volume()
      df <- rbind(
        data.frame(series = "all", series_label = "All certified filings", x = to_date(v$month), value = v$all_12),
        data.frame(series = "new", series_label = "New hires", x = to_date(v$month), value = v$new_12)
      )
      chart_lines(df, c(all = CATEGORICAL[1], new = CATEGORICAL[2]), "Filings, trailing 12 months",
                  hover_fmt = fmt_int)
    })
    output$volume_table <- reactable::renderReactable({
      v <- volume()
      data_table(v[order(v$month, decreasing = TRUE), c("month", "n", "n_new_hire", "all_12", "new_12")],
                 columns = list(month = reactable::colDef(name = "Month"), n = col_int("Filings"),
                                n_new_hire = col_int("New hires"), all_12 = col_int("Filings, 12 mo"),
                                new_12 = col_int("New hires, 12 mo")))
    })

    jolts <- shiny::reactive({
      j <- d$jolts[d$jolts$month >= start_month(), ]
      data.frame(series = "jolts", series_label = "Job openings", x = to_date(j$month),
                 value = j$openings_k / 1000, month = j$month)
    })
    output$jolts_plot <- plotly::renderPlotly(
      chart_lines(jolts(), c(jolts = CATEGORICAL[1]), "Openings (millions)", y_suffix = "M",
                  hover_fmt = function(v) formatC(v, format = "f", digits = 2)))
    output$jolts_table <- reactable::renderReactable(data_table(
      jolts()[order(jolts()$month, decreasing = TRUE), c("month", "value")],
      columns = list(month = reactable::colDef(name = "Month"),
                     value = reactable::colDef(name = "Openings (millions)", format = reactable::colFormat(digits = 2)))
    ))
  })
}
