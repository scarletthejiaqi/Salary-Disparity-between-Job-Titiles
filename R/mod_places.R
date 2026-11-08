# -----------------------------------------------------------------------------
# Employers & locations: who files the most for a role, what they offer, and
# how pay differs by state and city.
# -----------------------------------------------------------------------------

places_ui <- function(id, d) {
  ns <- shiny::NS(id)
  years <- sort(unique(d$employers$year), decreasing = TRUE)
  t <- d$levels[d$levels$basis == "title", ]
  htmltools::tagList(
    page_intro("Employers & locations",
               "Who sponsors the most positions for a role, what they offer, and how pay differs across states and cities."),
    filter_row(
      shiny::selectizeInput(ns("subject"), "Role", choices = subject_choices(d$taxonomy),
                            selected = subject_key("data_ai", "data_scientist"), width = "300px"),
      shiny::selectInput(ns("year"), "Year", stats::setNames(years, year_label(years, d$meta)),
                         selected = d$meta$lca$latest_full_year, width = "170px"),
      shiny::selectInput(ns("level"), "Seniority (map only)",
                         c("All levels" = "all", stats::setNames(t$level_id, t$level_name)), width = "220px"),
      shiny::selectInput(ns("dollars"), "Dollars", c("Nominal" = "nominal", "Inflation-adjusted" = "real"),
                         width = "170px")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("Top sponsoring employers", ns("emp_plot"), ns("emp_table"),
                 subtitle = "By number of certified filings; hover for median offered salary", height = "560px",
                 note = "Company names are grouped across legal entities for the largest sponsors (e.g. all Amazon entities).",
                 download_id = ns("emp_download")),
      chart_card("Median offered salary by state", ns("state_plot"), ns("state_table"),
                 subtitle = shiny::textOutput(ns("state_sub"), inline = TRUE), height = "560px",
                 note = "Gray tiles: fewer than 30 filings. Pay is not adjusted for local cost of living.")
    ),
    chart_card("Top cities", ns("city_plot"), ns("city_table"),
               subtitle = "The 20 cities with the most filings, ranked by median offered salary", height = "520px",
               note = "City is the primary worksite on the filing.")
  )
}

places_server <- function(id, d) {
  shiny::moduleServer(id, function(input, output, session) {
    state_names <- stats::setNames(c(datasets::state.name, "District of Columbia", "Puerto Rico"),
                                   c(datasets::state.abb, "DC", "PR"))
    by_subject <- function(df) {
      s <- parse_subject(input$subject)
      df[df$family_id == s$family_id & df$role_id == s$role_id & df$year == as.integer(input$year), , drop = FALSE]
    }
    real <- function(df) apply_dollars(df, d$cpi, input$dollars, period_type = "year", cols = c("p25", "p50", "p75"))

    emp <- shiny::reactive(real(by_subject(d$employers)))
    output$emp_plot <- plotly::renderPlotly({
      e <- utils::head(emp()[order(-emp()$n), ], 15)
      if (!nrow(e)) return(empty_plot())
      chart_ranked_bars(e$employer, e$n,
                        sprintf("<b>%s</b> filings  %s<br>Median %s · middle 50%% %s–%s<br>New hires: %s",
                                fmt_int(e$n), htmltools::htmlEscape(e$employer), fmt_dollar(e$p50),
                                fmt_dollar(e$p25), fmt_dollar(e$p75), fmt_pct(e$new_hire_share, signed = FALSE)),
                        x_title = "Certified filings", tickformat = ",")
    })
    emp_table_df <- shiny::reactive({
      e <- emp()
      e[order(-e$n), c("employer", "n", "p25", "p50", "p75", "new_hire_share")]
    })
    output$emp_table <- reactable::renderReactable(data_table(
      emp_table_df(),
      columns = list(employer = reactable::colDef(name = "Employer", minWidth = 220), n = col_int("Filings"),
                     p25 = col_dollar("25th pct"), p50 = col_dollar("Median"), p75 = col_dollar("75th pct"),
                     new_hire_share = col_pct("New hires", signed = FALSE)),
      searchable = TRUE
    ))
    output$emp_download <- csv_download(emp_table_df, "top-employers")

    states <- shiny::reactive({
      s <- by_subject(d$states)
      real(s[s$level_id == input$level, ])
    })
    output$state_sub <- shiny::renderText({
      lv <- if (input$level == "all") "all levels" else tolower(d$levels$level_name[d$levels$level_id == input$level])
      sprintf("%s · %s · %s", subject_label(input$subject, d$taxonomy), lv, year_label(as.integer(input$year), d$meta))
    })
    output$state_plot <- plotly::renderPlotly(chart_state_tiles(states(), state_names))
    output$state_table <- reactable::renderReactable({
      s <- states()
      s$name <- state_names[s$state]
      data_table(s[order(-s$p50), c("name", "n", "p25", "p50", "p75")],
                 columns = list(name = reactable::colDef(name = "State", minWidth = 180), n = col_int("Filings"),
                                p25 = col_dollar("25th pct"), p50 = col_dollar("Median"), p75 = col_dollar("75th pct")))
    })

    cities <- shiny::reactive(real(by_subject(d$cities)))
    output$city_plot <- plotly::renderPlotly({
      c <- cities()
      if (!nrow(c)) return(empty_plot())
      chart_ranked_bars(c$city, c$p50,
                        sprintf("<b>%s</b>  median, %s<br>Middle 50%%: %s–%s · %s filings",
                                fmt_dollar(c$p50), htmltools::htmlEscape(c$city), fmt_dollar(c$p25),
                                fmt_dollar(c$p75), fmt_int(c$n)),
                        x_title = "Median offered salary", tickprefix = "$")
    })
    output$city_table <- reactable::renderReactable({
      c <- cities()
      data_table(c[order(-c$p50), c("city", "n", "p25", "p50", "p75")],
                 columns = list(city = reactable::colDef(name = "City", minWidth = 200), n = col_int("Filings"),
                                p25 = col_dollar("25th pct"), p50 = col_dollar("Median"), p75 = col_dollar("75th pct")))
    })
  })
}
