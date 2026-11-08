# -----------------------------------------------------------------------------
# Reusable UI building blocks.
# -----------------------------------------------------------------------------

#' One left-aligned row of filters that scopes everything below it.
filter_row <- function(...) {
  htmltools::div(class = "filter-row", ...)
}

#' Stat tile: label, value and an optional signed delta vs a named period.
stat_tile <- function(label, value, delta = NULL, delta_label = NULL, up_is_good = TRUE,
                      help = NULL, delta_text = NULL, neutral = FALSE) {
  delta_tag <- NULL
  if (!is.null(delta) && length(delta) && !is.na(delta)) {
    dir <- if (delta > 0) "up" else if (delta < 0) "down" else "flat"
    good <- (dir == "up" && up_is_good) || (dir == "down" && !up_is_good)
    cls <- if (dir == "flat" || neutral) "flat" else if (good) "good" else "bad"
    icon <- switch(dir, up = "▲", down = "▼", flat = "▬")
    delta_tag <- htmltools::div(
      class = paste("stat-delta", cls),
      htmltools::span(class = "stat-delta-icon", `aria-hidden` = "true", icon),
      htmltools::span(if (is.null(delta_text)) fmt_pct(delta) else delta_text),
      if (!is.null(delta_label)) htmltools::span(class = "stat-delta-label", delta_label)
    )
  }
  htmltools::div(
    class = "stat-tile",
    htmltools::div(class = "stat-label", label),
    htmltools::div(class = "stat-value", value),
    delta_tag,
    if (!is.null(help)) htmltools::div(class = "stat-help", help)
  )
}

stat_row <- function(...) {
  htmltools::div(class = "stat-row", ...)
}

hero_figure <- function(label, value, sub = NULL) {
  htmltools::div(
    class = "hero",
    htmltools::div(class = "hero-label", label),
    htmltools::div(class = "hero-value", value),
    if (!is.null(sub)) htmltools::div(class = "hero-sub", sub)
  )
}

#' A card with a chart and its table twin, plus an optional footnote.
chart_card <- function(title, plot_id, table_id, subtitle = NULL, note = NULL,
                       height = "420px", download_id = NULL) {
  header <- htmltools::div(
    class = "chart-card-title",
    htmltools::div(class = "chart-title", title),
    if (!is.null(subtitle)) htmltools::div(class = "chart-subtitle", subtitle)
  )
  bslib::navset_card_underline(
    title = header,
    full_screen = TRUE,
    bslib::nav_panel("Chart", plotly::plotlyOutput(plot_id, height = height)),
    bslib::nav_panel(
      "Table",
      reactable::reactableOutput(table_id),
      if (!is.null(download_id)) htmltools::div(class = "mt-2",
        shiny::downloadButton(download_id, "Download CSV", class = "btn-sm btn-outline-secondary"))
    ),
    footer = if (!is.null(note)) htmltools::div(class = "card-note", note)
  )
}

page_intro <- function(title, text) {
  htmltools::div(class = "page-intro", htmltools::h2(title), htmltools::p(text))
}

insight_list <- function(items) {
  items <- Filter(function(x) !is.null(x) && length(x) && nzchar(x[1]), items)
  if (!length(items)) return(NULL)
  htmltools::tags$ul(class = "insights", lapply(items, function(x) htmltools::tags$li(htmltools::HTML(x))))
}

#' Shared reactable styling (tabular figures for aligned numbers).
table_theme <- function() {
  reactable::reactableTheme(
    color = VIZ$ink, backgroundColor = VIZ$surface, borderColor = VIZ$grid,
    stripedColor = "#f5f5f2", highlightColor = "#f0efec",
    cellPadding = "6px 10px",
    style = list(fontFamily = VIZ$font, fontSize = "0.875rem"),
    headerStyle = list(color = VIZ$ink_2, fontWeight = 600, borderBottomColor = VIZ$axis),
    searchInputStyle = list(width = "100%")
  )
}

data_table <- function(df, columns = NULL, ...) {
  defaults <- list(
    theme = table_theme(), compact = TRUE, striped = TRUE, highlight = TRUE,
    defaultPageSize = 15, sortable = TRUE, resizable = TRUE,
    defaultColDef = reactable::colDef(headerClass = "rt-header", class = "rt-num-cell")
  )
  args <- utils::modifyList(defaults, list(...))  # caller arguments win
  do.call(reactable::reactable, c(list(data = df, columns = columns), args))
}

col_dollar <- function(name, ...) {
  reactable::colDef(name = name, align = "right", cell = function(v) fmt_dollar(v, compact = FALSE), ...)
}
col_pct <- function(name, signed = TRUE, ...) {
  reactable::colDef(name = name, align = "right", cell = function(v) fmt_pct(v, signed = signed), ...)
}
col_int <- function(name, ...) {
  reactable::colDef(name = name, align = "right", cell = function(v) fmt_int(v), ...)
}

csv_download <- function(data_fun, prefix) {
  shiny::downloadHandler(
    filename = function() paste0(prefix, "-", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) utils::write.csv(data_fun(), file, row.names = FALSE, na = "")
  )
}
