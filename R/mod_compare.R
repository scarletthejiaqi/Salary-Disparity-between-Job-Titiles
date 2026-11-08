# -----------------------------------------------------------------------------
# Compare roles: which roles pay most, which grew fastest, and how pay growth
# relates to the change in hiring volume.
# -----------------------------------------------------------------------------

compare_ui <- function(id, d) {
  ns <- shiny::NS(id)
  years <- sort(unique(as.integer(d$trends$period[d$trends$period_type == "year"])), decreasing = TRUE)
  years <- years[years > 2019]
  fams <- d$families[d$families$family_id != "other", ]
  htmltools::tagList(
    page_intro("Compare roles",
               "Rank roles by pay and by how much pay changed since 2019. Use the seniority filter to compare like with like."),
    filter_row(
      shiny::selectInput(ns("family"), "Role family",
                         c("All families" = "all", stats::setNames(fams$family_id, fams$family_name)), width = "260px"),
      shiny::selectInput(ns("level"), "Seniority", choices = level_choices(d$levels), width = "220px"),
      shiny::selectInput(ns("year"), "Compare 2019 with", stats::setNames(years, year_label(years, d$meta)),
                         selected = d$meta$lca$latest_full_year, width = "170px"),
      shiny::selectInput(ns("filing"), "Filings", c("All certified" = "all", "New hires only" = "new_hire"),
                         width = "160px"),
      shiny::selectInput(ns("dollars"), "Dollars", c("Nominal" = "nominal", "Inflation-adjusted" = "real"),
                         width = "170px"),
      shiny::numericInput(ns("min_n"), "Min. filings per year", value = 100, min = 30, step = 50, width = "160px")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      chart_card(shiny::textOutput(ns("dumbbell_title"), inline = TRUE), ns("dumbbell_plot"), ns("dumbbell_table"),
                 subtitle = "Median offered salary per role, sorted by the latest value", height = "720px",
                 note = shiny::textOutput(ns("note"), inline = TRUE)),
      chart_card("Pay growth vs. hiring volume", ns("scatter_plot"), ns("scatter_table"),
                 subtitle = "Change since 2019 in median salary (up) and number of filings (right)", height = "720px",
                 note = "Upper right: more filings and higher pay. Lower left: fewer filings and slower pay growth.")
    ),
    bslib::card(bslib::card_header("All roles"), reactable::reactableOutput(ns("ranking")),
                bslib::card_footer(shiny::downloadButton(ns("download"), "Download CSV",
                                                         class = "btn-sm btn-outline-secondary")))
  )
}

compare_server <- function(id, d) {
  shiny::moduleServer(id, function(input, output, session) {
    meta <- d$meta

    ranked <- shiny::reactive({
      lvl <- strsplit(input$level, "|", fixed = TRUE)[[1]]
      to <- as.integer(input$year)
      tr <- d$trends
      tr <- tr[tr$period_type == "year" & tr$period %in% as.character(c(2019, to - 1, to)), ]
      tr <- apply_dollars(tr, d$cpi, input$dollars)
      r <- role_changes(tr, 2019L, to, filing = input$filing, level = lvl[2], basis = lvl[1])
      if (input$family != "all") r <- r[r$family_id == input$family, ]
      min_n <- max(30, input$min_n %||% 100)
      r <- r[!is.na(r$p50) & r$n >= min_n & (is.na(r$n_from) | r$n_from >= min_n), ]
      r$label <- d$taxonomy$role_name[match(r$role_id, d$taxonomy$role_id)]
      r$family <- d$families$family_name[match(r$family_id, d$families$family_id)]
      r[order(-r$p50), ]
    })

    output$dumbbell_title <- shiny::renderText(sprintf("2019 vs %s", year_label(as.integer(input$year), meta)))
    output$note <- shiny::renderText(paste(dollar_note(input$dollars, meta),
                                           sprintf("Roles shown have at least %s filings in both years.", max(30, input$min_n %||% 100))))
    output$dumbbell_plot <- plotly::renderPlotly({
      r <- ranked()
      chart_dumbbell(data.frame(label = r$label, from = r$p50_from, to = r$p50, stringsAsFactors = FALSE),
                     "2019", year_label(as.integer(input$year), meta))
    })
    output$dumbbell_table <- reactable::renderReactable({
      r <- ranked()
      data_table(r[, c("label", "p50_from", "p50", "change")],
                 columns = list(label = reactable::colDef(name = "Role", minWidth = 200),
                                p50_from = col_dollar("Median 2019"), p50 = col_dollar("Median"),
                                change = col_pct("Change")))
    })
    output$scatter_plot <- plotly::renderPlotly(chart_growth_scatter(ranked()))
    output$scatter_table <- reactable::renderReactable({
      r <- ranked()
      data_table(r[, c("label", "change", "volume_change", "n_from", "n")],
                 columns = list(label = reactable::colDef(name = "Role", minWidth = 200),
                                change = col_pct("Pay change"), volume_change = col_pct("Filings change"),
                                n_from = col_int("Filings 2019"), n = col_int("Filings")))
    })

    ranking_df <- shiny::reactive({
      r <- ranked()
      r[, c("label", "family", "p25", "p50", "p75", "yoy", "change", "cagr", "n", "volume_change")]
    })
    output$ranking <- reactable::renderReactable(data_table(
      ranking_df(),
      columns = list(label = reactable::colDef(name = "Role", minWidth = 200),
                     family = reactable::colDef(name = "Family", minWidth = 170),
                     p25 = col_dollar("25th pct"), p50 = col_dollar("Median"), p75 = col_dollar("75th pct"),
                     yoy = col_pct("1-yr change"), change = col_pct("Since 2019"), cagr = col_pct("Per year"),
                     n = col_int("Filings"), volume_change = col_pct("Filings vs 2019")),
      searchable = TRUE, defaultPageSize = 20
    ))
    output$download <- csv_download(ranking_df, "role-comparison")
  })
}
