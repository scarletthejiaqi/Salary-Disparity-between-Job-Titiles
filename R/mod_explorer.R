# -----------------------------------------------------------------------------
# Salary explorer: salary over time for seniority levels of one role, or for
# up to six roles side by side.
# -----------------------------------------------------------------------------

level_choices <- function(levels) {
  t <- levels[levels$basis == "title", ]
  g <- levels[levels$basis == "dol", ]
  list(
    "Overall" = c("All levels" = "all|all"),
    "By job title" = stats::setNames(paste("title", t$level_id, sep = "|"), t$level_name),
    "By DOL wage level" = stats::setNames(paste("dol", g$level_id, sep = "|"), g$level_name)
  )
}

explorer_ui <- function(id, d) {
  ns <- shiny::NS(id)
  is_mode <- function(m) sprintf("input['%s'] == '%s'", ns("compare"), m)
  htmltools::tagList(
    page_intro("Salary explorer",
               "How offered salaries changed from 2019 to now. Compare the seniority ladder within a role, or compare roles at the same level."),
    filter_row(
      shiny::radioButtons(ns("compare"), "Compare", c("Seniority levels" = "levels", "Roles" = "roles"),
                          inline = TRUE),
      shiny::conditionalPanel(is_mode("levels"),
        shiny::selectizeInput(ns("subject"), "Role", choices = subject_choices(d$taxonomy),
                              selected = subject_key("software", "swe"), width = "300px")),
      shiny::conditionalPanel(is_mode("levels"),
        shiny::radioButtons(ns("basis"), "Seniority from", c("Job title" = "title", "DOL wage level" = "dol"),
                            inline = TRUE)),
      shiny::conditionalPanel(is_mode("roles"),
        shiny::selectizeInput(ns("roles"), "Roles (up to 6)", choices = subject_choices(d$taxonomy),
                              multiple = TRUE, width = "420px",
                              selected = subject_key(c("software", "data_ai", "data_ai", "product"),
                                                     c("swe", "data_scientist", "data_engineer", "product_manager")),
                              options = list(maxItems = 6, plugins = list("remove_button")))),
      shiny::conditionalPanel(is_mode("roles"),
        shiny::selectInput(ns("level"), "Seniority", choices = level_choices(d$levels), width = "220px")),
      shiny::selectInput(ns("filing"), "Filings", c("All certified" = "all", "New hires only" = "new_hire"),
                         width = "160px"),
      shiny::selectInput(ns("dollars"), "Dollars", c("Nominal" = "nominal", "Inflation-adjusted" = "real"),
                         width = "170px"),
      shiny::selectInput(ns("grain"), "Time", c("Yearly" = "year", "Half-yearly" = "half"), width = "130px"),
      shiny::checkboxInput(ns("band"), "Middle-50% band", FALSE)
    ),
    shiny::uiOutput(ns("tiles")),
    chart_card(shiny::textOutput(ns("chart_title"), inline = TRUE), ns("trend_plot"), ns("trend_table"),
               subtitle = shiny::textOutput(ns("chart_sub"), inline = TRUE),
               note = shiny::textOutput(ns("chart_note"), inline = TRUE), height = "460px",
               download_id = ns("download")),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(bslib::card_header("Summary"), reactable::reactableOutput(ns("summary"))),
      bslib::card(bslib::card_header(shiny::textOutput(ns("titles_header"), inline = TRUE)),
                  reactable::reactableOutput(ns("titles")))
    )
  )
}

explorer_server <- function(id, d) {
  shiny::moduleServer(id, function(input, output, session) {
    meta <- d$meta
    full_year <- as.integer(meta$lca$latest_full_year)
    partial_year <- as.integer(meta$lca$partial_year)
    partial_months <- as.integer(meta$lca$partial_year_months)
    tr <- d$trends

    # Colour slots follow each role while it stays selected
    slots <- shiny::reactiveVal(stats::setNames(integer(0), character(0)))
    shiny::observeEvent(input$roles, {
      cur <- slots()
      sel <- input$roles
      cur <- cur[names(cur) %in% sel]
      for (k in setdiff(sel, names(cur))) cur[k] <- setdiff(seq_along(CATEGORICAL), cur)[1]
      slots(cur)
    }, ignoreNULL = FALSE)

    # All rows (both period types) for the current selection, one 'series' each
    selection <- shiny::reactive({
      if (identical(input$compare, "levels")) {
        shiny::req(input$subject)
        rows <- filter_subject(tr, input$subject)
        rows <- rows[rows$filing == input$filing & rows$level_basis == input$basis, ]
        lv <- d$levels[d$levels$basis == input$basis, ]
        rows$series <- rows$level_id
        rows$series_label <- lv$level_name[match(rows$level_id, lv$level_id)]
        rows$order <- lv$level_order[match(rows$level_id, lv$level_id)]
        colors <- LEVEL_COLORS[lv$level_id]
      } else {
        shiny::req(length(input$roles) > 0)
        lvl <- strsplit(input$level, "|", fixed = TRUE)[[1]]
        rows <- do.call(rbind, lapply(input$roles, function(k) {
          r <- filter_subject(tr, k)
          r <- r[r$filing == input$filing & r$level_basis == lvl[1] & r$level_id == lvl[2], ]
          if (!nrow(r)) return(NULL)
          r$series <- k
          r$series_label <- subject_label(k, d$taxonomy)
          r$order <- match(k, input$roles)
          r
        }))
        shiny::req(!is.null(rows))
        sl <- slots()
        colors <- stats::setNames(CATEGORICAL[sl[input$roles]], input$roles)
        colors[is.na(colors)] <- CATEGORICAL[1]
      }
      rows <- apply_dollars(rows, d$cpi, input$dollars)
      list(rows = rows, colors = colors)
    })

    plot_rows <- shiny::reactive({
      s <- selection()
      r <- s$rows[s$rows$period_type == input$grain, ]
      r <- r[order(r$order, r$period), ]
      if (input$grain == "year") {
        r$x <- as.integer(r$period)
        r$is_partial <- r$x == partial_year & partial_months < 12
      } else {
        r$x <- r$period_date
        r$is_partial <- FALSE
      }
      r
    })

    output$chart_title <- shiny::renderText({
      if (identical(input$compare, "levels")) {
        sprintf("%s: offered salary by seniority", subject_label(input$subject, d$taxonomy))
      } else {
        sprintf("Offered salary by role, %s", tolower(level_label(strsplit(input$level, "|", fixed = TRUE)[[1]][2], d$levels)))
      }
    })
    output$chart_sub <- shiny::renderText({
      paste0("Median offered annual salary", if (isTRUE(input$band)) ", with the middle 50% shaded" else "",
             if (identical(input$filing, "new_hire")) " · new hires only" else "")
    })
    output$chart_note <- shiny::renderText({
      paste(dollar_note(input$dollars, meta),
            if (input$grain == "year" && partial_months < 12)
              sprintf("Hollow marker: %s.", year_label(partial_year, meta)) else "",
            sprintf("Points with fewer than %s filings are not shown.", meta$lca$min_n_publish))
    })

    output$trend_plot <- plotly::renderPlotly({
      r <- plot_rows()
      s <- selection()
      ticks <- NULL
      if (input$grain == "year") {
        yrs <- sort(unique(r$x))
        ticks <- list(values = yrs, labels = year_label(yrs, meta))
      }
      band <- isTRUE(input$band) && length(unique(r$series[!is.na(r$p50)])) <= 2
      chart_trend(r, s$colors, band = band, x_ticks = ticks)
    })

    table_rows <- shiny::reactive({
      r <- plot_rows()
      out <- data.frame(series = r$series_label, period = r$period, n = r$n, p25 = r$p25,
                        median = r$p50, p75 = r$p75, mean = r$mean, stringsAsFactors = FALSE)
      out
    })
    output$trend_table <- reactable::renderReactable(data_table(
      table_rows(),
      columns = list(series = reactable::colDef(name = "Series", minWidth = 180),
                     period = reactable::colDef(name = "Period"), n = col_int("Filings"),
                     p25 = col_dollar("25th pct"), median = col_dollar("Median"),
                     p75 = col_dollar("75th pct"), mean = col_dollar("Mean"))
    ))
    output$download <- csv_download(table_rows, "salary-trend")

    summary_rows <- shiny::reactive({
      s <- selection()
      do.call(rbind, lapply(unique(s$rows$series), function(k) {
        r <- s$rows[s$rows$series == k, ]
        h <- series_headline(r, 2019L, full_year, partial_year)
        data.frame(series = r$series_label[1], order = r$order[1], latest = h$latest, yoy = h$yoy,
                   since = h$since_base, cagr = h$cagr, ytd = h$ytd, ytd_yoy = h$ytd_yoy,
                   n = h$n_latest, stringsAsFactors = FALSE)
      }))
    })

    output$tiles <- shiny::renderUI({
      sr <- summary_rows()
      shiny::req(nrow(sr) > 0)
      sr <- sr[order(sr$order), ]
      first <- if (identical(input$compare, "levels")) {
        r <- filter_subject(tr, input$subject)
        r <- apply_dollars(r[r$filing == input$filing & r$level_basis == "all", ], d$cpi, input$dollars)
        h <- series_headline(r, 2019L, full_year, partial_year)
        list(label = paste(subject_label(input$subject, d$taxonomy), "· all levels"), h = h)
      } else {
        r <- selection()$rows
        r <- r[r$series == input$roles[1], ]
        list(label = subject_label(input$roles[1], d$taxonomy),
             h = series_headline(r, 2019L, full_year, partial_year))
      }
      h <- first$h
      stat_row(
        stat_tile(sprintf("%s, median %d", first$label, full_year), fmt_dollar(h$latest),
                  delta = h$yoy, delta_label = sprintf("vs %d", full_year - 1)),
        stat_tile(sprintf("Change since 2019"), fmt_pct(h$since_base),
                  help = sprintf("%s a year (compound)", fmt_pct(h$cagr))),
        stat_tile(year_label(partial_year, meta), fmt_dollar(h$ytd), delta = h$ytd_yoy,
                  delta_label = sprintf("vs same months of %d", partial_year - 1)),
        stat_tile(sprintf("Filings, %d", full_year), fmt_compact_int(h$n_latest),
                  delta = pct_change(h$n_latest, h$n_base), delta_label = "vs 2019", neutral = TRUE)
      )
    })

    output$summary <- reactable::renderReactable({
      sr <- summary_rows()
      sr <- sr[order(sr$order), setdiff(names(sr), "order")]
      data_table(sr, columns = list(
        series = reactable::colDef(name = "Series", minWidth = 170),
        latest = col_dollar(paste("Median", full_year)), yoy = col_pct("1-yr change"),
        since = col_pct("Since 2019"), cagr = col_pct("Per year"),
        ytd = col_dollar(year_label(partial_year, meta)), ytd_yoy = col_pct("vs prior-year months"),
        n = col_int(paste("Filings", full_year))
      ), pagination = FALSE)
    })

    focus_subject <- shiny::reactive({
      if (identical(input$compare, "levels")) input$subject else input$roles[1]
    })
    output$titles_header <- shiny::renderText({
      s <- parse_subject(focus_subject())
      if (s$role_id == "all") "Largest roles in this group" else
        sprintf("Most common job titles · %s", subject_label(focus_subject(), d$taxonomy))
    })
    output$titles <- reactable::renderReactable({
      s <- parse_subject(focus_subject())
      if (s$role_id != "all") {
        t <- d$titles[d$titles$role_id == s$role_id, c("title", "n", "share", "p50")]
        data_table(t, columns = list(title = reactable::colDef(name = "Job title", minWidth = 200),
                                     n = col_int("Filings"), share = col_pct("Share of role", signed = FALSE),
                                     p50 = col_dollar("Median")), defaultPageSize = 10)
      } else {
        r <- tr[tr$period_type == "year" & tr$period == as.character(full_year) & tr$filing == "all" &
                  tr$level_basis == "all" & tr$role_id != "all" & tr$family_id != "all", ]
        if (s$family_id != "all") r <- r[r$family_id == s$family_id, ]
        r$role <- d$taxonomy$role_name[match(r$role_id, d$taxonomy$role_id)]
        r <- r[order(-r$n), c("role", "n", "p50")]
        data_table(r, columns = list(role = reactable::colDef(name = "Role", minWidth = 200),
                                     n = col_int(paste("Filings", full_year)), p50 = col_dollar("Median")),
                   defaultPageSize = 10)
      }
    })
  })
}
