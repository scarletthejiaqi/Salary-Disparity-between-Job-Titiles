# -----------------------------------------------------------------------------
# Methodology & data: sources, definitions, cleaning steps, taxonomy and
# validation tables.
# -----------------------------------------------------------------------------

methodology_ui <- function(id, d) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(class = "prose", bslib::card_body(shiny::includeMarkdown("content/methodology.md"))),
      htmltools::div(
        bslib::card(bslib::card_header("Data vintage"), bslib::card_body(shiny::uiOutput(ns("vintage")))),
        bslib::card(bslib::card_header("Download the published tables"),
                    bslib::card_body(shiny::uiOutput(ns("downloads"))))
      )
    ),
    bslib::card(bslib::card_header("From raw filings to published numbers"), reactable::reactableOutput(ns("funnel"))),
    bslib::card(bslib::card_header("Role taxonomy (52 roles in 14 families)"),
                bslib::card_body(htmltools::p(class = "card-note",
                  "Share by title rule: filings whose role was decided by a job-title rule rather than the occupation (SOC) code.")),
                reactable::reactableOutput(ns("taxonomy"))),
    bslib::card(bslib::card_header("Check: seniority from job titles vs. DOL wage levels"),
                bslib::card_body(htmltools::p(class = "card-note",
                  paste("Each row shows how filings with a given title-based level are distributed across DOL",
                        "prevailing-wage levels (all years). Higher title levels should lean toward higher",
                        "wage levels and higher medians."))),
                reactable::reactableOutput(ns("crosswalk")))
  )
}

methodology_server <- function(id, d) {
  shiny::moduleServer(id, function(input, output, session) {
    m <- d$meta
    output$vintage <- shiny::renderUI({
      htmltools::tags$dl(class = "vintage",
        htmltools::tags$dt("DOL filings"),
        htmltools::tags$dd(sprintf("%s certified filings received %s to %s", fmt_int(m$lca$filings),
                                   m$lca$first_received, m$lca$last_received)),
        htmltools::tags$dt("Indeed job postings"), htmltools::tags$dd(sprintf("through %s", m$market$indeed_postings_last_month)),
        htmltools::tags$dt("Indeed posted wages"), htmltools::tags$dd(sprintf("through %s", m$market$indeed_wages_last_month)),
        htmltools::tags$dt("JOLTS job openings"), htmltools::tags$dd(sprintf("through %s", m$market$jolts_last_month)),
        htmltools::tags$dt("CPI-U"), htmltools::tags$dd(sprintf("through %s (real dollars = %s)", m$market$cpi_last_month,
                                                                 m$market$cpi_base_year)),
        htmltools::tags$dt("Tables built"), htmltools::tags$dd(m$built_at)
      )
    })
    files <- c("salary_trends.csv", "salary_by_state.csv", "top_employers.csv", "top_cities.csv",
               "top_titles.csv", "volume_monthly.csv", "taxonomy.csv")
    output$downloads <- shiny::renderUI({
      htmltools::tagList(
        htmltools::p(class = "card-note", "Aggregated tables only; no individual filings are published here."),
        lapply(files, function(f) htmltools::div(shiny::downloadLink(session$ns(paste0("dl_", sub("\\.csv$", "", f))), f)))
      )
    })
    for (f in files) local({
      file <- f
      output[[paste0("dl_", sub("\\.csv$", "", file))]] <- shiny::downloadHandler(
        filename = function() file,
        content = function(path) file.copy(file.path("data", file), path)
      )
    })

    output$funnel <- reactable::renderReactable(data_table(
      d$funnel,
      columns = list(step = reactable::colDef(name = "Step", minWidth = 320), n = col_int("Records"),
                     pct_of_raw = reactable::colDef(name = "% of raw", align = "right",
                                                    format = reactable::colFormat(suffix = "%", digits = 1))),
      pagination = FALSE, sortable = FALSE
    ))

    output$taxonomy <- reactable::renderReactable({
      t <- merge(d$taxonomy[, c("family_name", "role_name", "role_id", "family_order", "role_order", "indeed_sectors")],
                 d$quality[, c("role_id", "n", "share_title_rule")], by = "role_id", all.x = TRUE)
      ex <- vapply(t$role_id, function(r) {
        x <- utils::head(d$titles$title[d$titles$role_id == r], 4)
        paste(x, collapse = "; ")
      }, "")
      t$examples <- ex
      t <- t[order(t$family_order, t$role_order), c("family_name", "role_name", "examples", "n", "share_title_rule", "indeed_sectors")]
      data_table(t, columns = list(
        family_name = reactable::colDef(name = "Family", minWidth = 170),
        role_name = reactable::colDef(name = "Role", minWidth = 190),
        examples = reactable::colDef(name = "Most common titles", minWidth = 320),
        n = col_int("Filings 2019–2026"),
        share_title_rule = col_pct("Share by title rule", signed = FALSE),
        indeed_sectors = reactable::colDef(name = "Matching Indeed sectors", minWidth = 200)
      ), searchable = TRUE, defaultPageSize = 52, pagination = FALSE)
    })

    output$crosswalk <- reactable::renderReactable({
      x <- d$crosswalk
      lv <- d$levels
      x$title_name <- lv$level_name[match(x$level_title, lv$level_id)]
      x$title_order <- lv$level_order[match(x$level_title, lv$level_id)]
      x$dol_name <- lv$level_name[match(x$level_dol, lv$level_id)]
      wide <- stats::reshape(x[, c("title_name", "title_order", "dol_name", "share_of_title_level")],
                             idvar = c("title_name", "title_order"), timevar = "dol_name", direction = "wide")
      names(wide) <- sub("^share_of_title_level\\.", "", names(wide))
      med <- stats::aggregate(n ~ title_name, data = x, FUN = sum)
      wide$filings <- med$n[match(wide$title_name, med$title_name)]
      wide <- wide[order(wide$title_order), setdiff(names(wide), "title_order")]
      dol_cols <- intersect(lv$level_name[lv$basis == "dol"], names(wide))
      cols <- c(list(title_name = reactable::colDef(name = "Title-based level", minWidth = 190),
                     filings = col_int("Filings")),
                stats::setNames(lapply(dol_cols, function(n) col_pct(n, signed = FALSE)), dol_cols))
      data_table(wide[, c("title_name", "filings", dol_cols)], columns = cols, pagination = FALSE)
    })
  })
}
