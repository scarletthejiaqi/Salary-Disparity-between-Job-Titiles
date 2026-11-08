# -----------------------------------------------------------------------------
# Visual system: palette, bslib theme, plotly defaults and number formatting.
#
# Colours follow a validated chart palette (categorical slots checked for
# colour-vision-deficiency separation; seniority ramps checked as ordinal
# one-hue ramps). Colour always follows the entity, never its rank.
# -----------------------------------------------------------------------------

VIZ <- list(
  surface   = "#fcfcfb",
  page      = "#f9f9f7",
  ink       = "#0b0b0b",
  ink_2     = "#52514e",
  muted     = "#898781",
  grid      = "#e1e0d9",
  axis      = "#c3c2b7",
  context   = "#b4b2aa",   # de-emphasised comparison series
  good      = "#006300",   # positive delta text
  bad       = "#d03b3b",   # negative delta text
  font      = 'system-ui, -apple-system, "Segoe UI", Roboto, sans-serif'
)

# Categorical slots in fixed order (identity). Max 6 series per chart.
CATEGORICAL <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")

# Seniority is ordered: the individual-contributor ladder is a blue ordinal
# ramp, the management track a second (orange) ordinal ramp.
LEVEL_COLORS <- c(
  entry    = "#86b6ef",
  mid      = "#3987e5",
  senior   = "#184f95",
  lead     = "#0d366b",
  manager  = "#eb6834",
  director = "#a33f17",
  dol_1    = "#86b6ef",
  dol_2    = "#3987e5",
  dol_3    = "#184f95",
  dol_4    = "#0d366b",
  all      = "#2a78d6"
)

# Sequential ramp (magnitude, e.g. the state map), light -> dark.
SEQUENTIAL <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

# Diverging poles for above / below a baseline.
DIVERGING <- c(neg = "#e34948", mid = "#f0efec", pos = "#2a78d6")

app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = VIZ$page, fg = VIZ$ink,
    primary = "#2a78d6", secondary = VIZ$ink_2,
    success = VIZ$good, danger = VIZ$bad,
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
    heading_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
    "border-radius" = "0.6rem",
    "card-border-color" = "rgba(11,11,11,0.10)",
    "card-cap-bg" = VIZ$surface,
    "card-bg" = VIZ$surface,
    "navbar-bg" = VIZ$surface
  )
}

# ---- Number formatting --------------------------------------------------------

fmt_dollar <- function(x, compact = TRUE, digits = NULL) {
  out <- rep("—", length(x))
  ok <- !is.na(x)
  if (compact) {
    big <- ok & abs(x) >= 1e6
    k <- ok & !big
    out[k] <- paste0("$", formatC(x[k] / 1e3, format = "f", digits = if (is.null(digits)) 0 else digits,
                                  big.mark = ","), "K")
    out[big] <- paste0("$", formatC(x[big] / 1e6, format = "f", digits = 2), "M")
  } else {
    out[ok] <- paste0("$", formatC(round(x[ok]), format = "d", big.mark = ","))
  }
  out
}

fmt_pct <- function(x, digits = 1, signed = TRUE) {
  out <- rep("—", length(x))
  ok <- !is.na(x) & is.finite(x)
  v <- formatC(100 * x[ok], format = "f", digits = digits)
  if (signed) v <- ifelse(x[ok] > 0, paste0("+", v), v)
  v <- sub("^-", "−", v)
  out[ok] <- paste0(v, "%")
  out
}

fmt_int <- function(x) {
  out <- rep("—", length(x))
  ok <- !is.na(x)
  out[ok] <- formatC(round(x[ok]), format = "d", big.mark = ",")
  out
}

fmt_compact_int <- function(x) {
  out <- rep("—", length(x))
  ok <- !is.na(x)
  big <- ok & abs(x) >= 1e6
  k <- ok & !big & abs(x) >= 1e4
  small <- ok & !big & !k
  out[big] <- paste0(formatC(x[big] / 1e6, format = "f", digits = 1), "M")
  out[k] <- paste0(formatC(x[k] / 1e3, format = "f", digits = 0), "K")
  out[small] <- formatC(round(x[small]), format = "d", big.mark = ",")
  out
}

# ---- Plotly defaults ------------------------------------------------------------

viz_axis <- function(title = NULL, ...) {
  utils::modifyList(list(
    title = list(text = title, font = list(size = 12, color = VIZ$ink_2)),
    gridcolor = VIZ$grid, gridwidth = 1, zeroline = FALSE,
    linecolor = VIZ$axis, linewidth = 1, showline = TRUE,
    tickfont = list(size = 11, color = VIZ$ink_2),
    ticks = "", automargin = TRUE
  ), list(...))
}

viz_layout <- function(p, x = list(), y = list(), legend = TRUE, hovermode = "x unified",
                       margin = list(l = 8, r = 24, t = 36, b = 8), ...) {
  p <- plotly::layout(
    p,
    font = list(family = VIZ$font, color = VIZ$ink, size = 12),
    paper_bgcolor = VIZ$surface, plot_bgcolor = VIZ$surface,
    xaxis = do.call(viz_axis, x), yaxis = do.call(viz_axis, y),
    hovermode = hovermode,
    hoverlabel = list(bgcolor = "#ffffff", bordercolor = VIZ$axis,
                      font = list(family = VIZ$font, color = VIZ$ink, size = 12)),
    showlegend = legend,
    legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom",
                  font = list(size = 12, color = VIZ$ink_2), bgcolor = "rgba(0,0,0,0)"),
    margin = margin,
    ...
  )
  plotly::config(p, displaylogo = FALSE, responsive = TRUE,
                 modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                            "hoverClosestCartesian", "hoverCompareCartesian",
                                            "toggleSpikelines"),
                 toImageButtonOptions = list(format = "png", scale = 2))
}

# Crosshair that snaps to the nearest x position (line charts).
crosshair <- function() {
  list(showspikes = TRUE, spikemode = "across", spikesnap = "cursor",
       spikethickness = 1, spikecolor = VIZ$axis, spikedash = "solid")
}

empty_plot <- function(message = "Not enough filings to show this view.") {
  plotly::plot_ly(type = "scatter", mode = "markers") |>
    plotly::layout(
      paper_bgcolor = VIZ$surface, plot_bgcolor = VIZ$surface,
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(text = message, showarrow = FALSE, xref = "paper", yref = "paper",
                              x = 0.5, y = 0.5, font = list(color = VIZ$ink_2, size = 14)))
    ) |>
    plotly::config(displayModeBar = FALSE)
}
