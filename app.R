# -----------------------------------------------------------------------------
# U.S. Pay & Hiring Trends, 2019 to now - Shiny dashboard.
#
# The app only reads the small aggregated tables in data/ (built by the
# pipeline in pipeline/), so it starts quickly and deploys as-is to
# shinyapps.io. Files in R/ are sourced automatically by Shiny.
#
# Run locally with `make run` (or shiny::runApp() from this folder).
# -----------------------------------------------------------------------------

library(shiny)

app_data <- load_app_data("data")

ui <- bslib::page_navbar(
  title = htmltools::span(class = "brand", "Pay & Hiring Trends"),
  id = "nav",
  theme = app_theme(),
  fillable = FALSE,
  header = htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", href = "styles.css"),
    htmltools::tags$meta(name = "description",
                         content = "U.S. offered salaries by role, seniority, employer and location, 2019 to now, with hiring-demand trends.")
  ),
  bslib::nav_panel("Overview", overview_ui("overview")),
  bslib::nav_panel("Salary explorer", explorer_ui("explorer", app_data)),
  bslib::nav_panel("Seniority ladder", ladder_ui("ladder", app_data)),
  bslib::nav_panel("Compare roles", compare_ui("compare", app_data)),
  bslib::nav_panel("Hiring demand", market_ui("market", app_data)),
  bslib::nav_panel("Employers & locations", places_ui("places", app_data)),
  bslib::nav_menu(
    "About",
    bslib::nav_panel("Methodology & data", methodology_ui("methodology", app_data)),
    bslib::nav_panel("About this project",
                     bslib::card(class = "prose", bslib::card_body(shiny::includeMarkdown("content/about.md"))))
  ),
  footer = htmltools::div(
    class = "app-footer",
    sprintf("Data: U.S. DOL OFLC LCA disclosures (filings through %s); Indeed Hiring Lab (CC BY 4.0); BLS via FRED.",
            app_data$meta$lca$last_received)
  )
)

server <- function(input, output, session) {
  overview_server("overview", app_data)
  explorer_server("explorer", app_data)
  ladder_server("ladder", app_data)
  compare_server("compare", app_data)
  market_server("market", app_data)
  places_server("places", app_data)
  methodology_server("methodology", app_data)
}

shinyApp(ui, server)
