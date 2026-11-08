# -----------------------------------------------------------------------------
# Chart builders (plotly). Each takes tidy data and returns a plotly object.
# Mark specs: 2px lines, 8px markers with a 2px surface ring, 10% opacity
# bands, hairline solid grids, one y-axis, unified hover with a crosshair.
# -----------------------------------------------------------------------------

hex_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
}

#' Pick white or near-black text for a label sitting inside a fill.
ink_on <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(lum > 0.55, VIZ$ink, "#ffffff")
}

#' Salary trend lines.
#'
#' @param df Rows with series, series_label, x (numeric year or Date), p25, p50,
#'   p75, n and is_partial.
#' @param colors Named vector series -> colour.
#' @param band Draw the p25-p75 band (only sensible for <= 2 series).
chart_trend <- function(df, colors, band = FALSE, y_title = "Median offered salary",
                        x_ticks = NULL, stat_label = "Median") {
  df <- df[!is.na(df$p50), , drop = FALSE]
  if (!nrow(df)) return(empty_plot())
  series <- unique(df$series)
  p <- plotly::plot_ly()
  for (s in series) {
    d <- df[df$series == s, , drop = FALSE]
    d <- d[order(d$x), , drop = FALSE]
    col <- colors[[s]]
    lab <- d$series_label[1]
    if (band && all(c("p25", "p75") %in% names(d)) && any(!is.na(d$p25))) {
      p <- plotly::add_ribbons(p, x = d$x, ymin = d$p25, ymax = d$p75,
                               fillcolor = hex_alpha(col, 0.10), line = list(width = 0),
                               hoverinfo = "skip", showlegend = FALSE, name = paste(lab, "IQR"))
    }
    hover <- sprintf("<b>%s</b>  %s<br><span style='color:%s'>Middle 50%%: %s–%s · %s filings</span>",
                     fmt_dollar(d$p50), htmltools::htmlEscape(lab), VIZ$ink_2,
                     fmt_dollar(d$p25), fmt_dollar(d$p75), fmt_int(d$n))
    symbols <- ifelse(d$is_partial, "circle-open", "circle")
    p <- plotly::add_trace(
      p, type = "scatter", mode = "lines+markers", x = d$x, y = d$p50, name = lab,
      line = list(color = col, width = 2, shape = "linear"),
      marker = list(color = col, size = 8, symbol = symbols, line = list(color = VIZ$surface, width = 2)),
      text = hover, hovertemplate = "%{text}<extra></extra>"
    )
  }
  # Direct end labels when series separate (<= 4 series)
  ann <- list()
  if (length(series) >= 2 && length(series) <= 4) {
    ends <- do.call(rbind, lapply(series, function(s) {
      d <- df[df$series == s, , drop = FALSE]
      d[which.max(d$x), , drop = FALSE]
    }))
    ann <- lapply(seq_len(nrow(ends)), function(i) {
      list(x = ends$x[i], y = ends$p50[i], text = htmltools::htmlEscape(ends$series_label[i]),
           xanchor = "left", xshift = 10, showarrow = FALSE,
           font = list(size = 11, color = VIZ$ink_2))
    })
  }
  xaxis <- c(list(title = NULL), crosshair())
  if (!is.null(x_ticks)) xaxis <- c(xaxis, list(tickvals = x_ticks$values, ticktext = x_ticks$labels))
  viz_layout(p, x = xaxis,
             y = list(title = y_title, tickprefix = "$", tickformat = "~s", rangemode = "normal"),
             legend = length(series) > 1, annotations = ann,
             margin = list(l = 8, r = if (length(ann)) 150 else 24, t = 36, b = 8))
}

#' Dumbbell: value in a base period vs a later period, per item.
chart_dumbbell <- function(df, from_label, to_label, value_prefix = "$") {
  df <- df[!is.na(df$from) & !is.na(df$to), , drop = FALSE]
  if (!nrow(df)) return(empty_plot())
  df <- df[order(df$to), , drop = FALSE]
  df$label <- factor(df$label, levels = df$label)
  chg <- pct_change(df$to, df$from)
  hover_from <- sprintf("<b>%s</b>  %s %s", fmt_dollar(df$from), from_label, htmltools::htmlEscape(as.character(df$label)))
  hover_to <- sprintf("<b>%s</b>  %s %s<br>%s since %s", fmt_dollar(df$to), to_label,
                      htmltools::htmlEscape(as.character(df$label)), fmt_pct(chg), from_label)
  p <- plotly::plot_ly() |>
    plotly::add_segments(x = df$from, xend = df$to, y = df$label, yend = df$label,
                         line = list(color = VIZ$axis, width = 2), hoverinfo = "skip",
                         showlegend = FALSE) |>
    plotly::add_trace(type = "scatter", mode = "markers", x = df$from, y = df$label, name = from_label,
                      marker = list(color = "#86b6ef", size = 9, line = list(color = VIZ$surface, width = 2)),
                      text = hover_from, hovertemplate = "%{text}<extra></extra>") |>
    plotly::add_trace(type = "scatter", mode = "markers", x = df$to, y = df$label, name = to_label,
                      marker = list(color = "#184f95", size = 9, line = list(color = VIZ$surface, width = 2)),
                      text = hover_to, hovertemplate = "%{text}<extra></extra>")
  ann <- lapply(seq_len(nrow(df)), function(i) {
    list(x = max(df$from[i], df$to[i]), y = as.character(df$label[i]), text = fmt_pct(chg[i]),
         xanchor = "left", xshift = 10, showarrow = FALSE, font = list(size = 11, color = VIZ$ink_2))
  })
  viz_layout(p, x = list(title = NULL, tickprefix = value_prefix, tickformat = "~s"),
             y = list(title = NULL, showgrid = FALSE, showline = FALSE),
             hovermode = "closest", annotations = ann,
             margin = list(l = 8, r = 56, t = 36, b = 8))
}

#' Seniority pay ladder: interquartile range bar with the median marked.
chart_ladder <- function(df) {
  df <- df[!is.na(df$p50), , drop = FALSE]
  if (!nrow(df)) return(empty_plot())
  df <- df[order(df$level_order), , drop = FALSE]
  df$label <- factor(df$level_name, levels = rev(df$level_name))
  cols <- unname(LEVEL_COLORS[df$level_id])
  hover <- sprintf("<b>%s</b>  median, %s<br>Middle 50%%: %s–%s<br>%s filings",
                   fmt_dollar(df$p50), htmltools::htmlEscape(df$level_name),
                   fmt_dollar(df$p25), fmt_dollar(df$p75), fmt_int(df$n))
  p <- plotly::plot_ly() |>
    plotly::add_bars(x = df$p75 - df$p25, base = df$p25, y = df$label, orientation = "h",
                     marker = list(color = hex_alpha(cols, 0.28), line = list(width = 0)),
                     text = hover, hovertemplate = "%{text}<extra></extra>", showlegend = FALSE,
                     textposition = "none") |>
    plotly::add_trace(type = "scatter", mode = "markers+text", x = df$p50, y = df$label,
                      marker = list(color = cols, size = 12, line = list(color = VIZ$surface, width = 2)),
                      text = fmt_dollar(df$p50), textposition = "top center",
                      textfont = list(size = 11, color = VIZ$ink), hoverinfo = "skip", showlegend = FALSE)
  viz_layout(p, x = list(title = "Offered salary (middle 50% and median)", tickprefix = "$", tickformat = "~s"),
             y = list(title = NULL, showgrid = FALSE, showline = FALSE),
             hovermode = "closest", legend = FALSE, bargap = 0.55)
}

#' 100% stacked columns of the seniority mix per year.
chart_level_mix <- function(df, levels) {
  if (!nrow(df)) return(empty_plot())
  p <- plotly::plot_ly()
  for (lv in levels$level_id) {
    d <- df[df$level_id == lv, , drop = FALSE]
    if (!nrow(d)) next
    lab <- levels$level_name[levels$level_id == lv]
    p <- plotly::add_bars(p, x = d$x_label, y = d$share, name = lab,
                          marker = list(color = LEVEL_COLORS[[lv]], line = list(color = VIZ$surface, width = 2)),
                          text = sprintf("<b>%s</b>  %s<br>%s filings", fmt_pct(d$share, signed = FALSE),
                                         lab, fmt_int(d$n)),
                          hovertemplate = "%{text}<extra></extra>", textposition = "none")
  }
  viz_layout(p, x = list(title = NULL, type = "category"),
             y = list(title = "Share of filings", tickformat = ".0%", range = c(0, 1)),
             barmode = "stack", bargap = 0.55, hovermode = "closest")
}

#' Horizontal ranked bars (one hue).
chart_ranked_bars <- function(labels, values, hover, color = CATEGORICAL[1], x_title = NULL,
                              tickprefix = "", tickformat = "~s") {
  if (!length(values)) return(empty_plot())
  o <- order(values)
  lab <- factor(labels[o], levels = labels[o])
  p <- plotly::plot_ly() |>
    plotly::add_bars(x = values[o], y = lab, orientation = "h",
                     marker = list(color = color, line = list(width = 0)),
                     text = hover[o], hovertemplate = "%{text}<extra></extra>", textposition = "none")
  viz_layout(p, x = list(title = x_title, tickprefix = tickprefix, tickformat = tickformat),
             y = list(title = NULL, showgrid = FALSE, showline = FALSE),
             legend = FALSE, hovermode = "closest", bargap = 0.4)
}

#' Diverging bars around a baseline (e.g. postings index vs 100).
chart_diverging_bars <- function(labels, values, baseline, hover, x_title = NULL) {
  if (!length(values)) return(empty_plot())
  o <- order(values)
  lab <- factor(labels[o], levels = labels[o])
  v <- values[o]
  cols <- ifelse(v >= baseline, DIVERGING[["pos"]], DIVERGING[["neg"]])
  p <- plotly::plot_ly() |>
    plotly::add_bars(x = v - baseline, base = baseline, y = lab, orientation = "h",
                     marker = list(color = cols, line = list(width = 0)),
                     text = hover[o], hovertemplate = "%{text}<extra></extra>", textposition = "none")
  viz_layout(p, x = list(title = x_title, zeroline = FALSE),
             y = list(title = NULL, showgrid = FALSE, showline = FALSE),
             legend = FALSE, hovermode = "closest", bargap = 0.4,
             shapes = list(list(type = "line", x0 = baseline, x1 = baseline, y0 = 0, y1 = 1,
                                yref = "paper", line = list(color = VIZ$muted, width = 1))))
}

#' Generic multi-series monthly line chart with an optional reference line.
chart_lines <- function(df, colors, y_title, ref = NULL, y_format = NULL, y_suffix = "",
                        hover_fmt = function(v) formatC(v, format = "f", digits = 1)) {
  df <- df[!is.na(df$value), , drop = FALSE]
  if (!nrow(df)) return(empty_plot())
  p <- plotly::plot_ly()
  for (s in unique(df$series)) {
    d <- df[df$series == s, , drop = FALSE]
    d <- d[order(d$x), , drop = FALSE]
    lab <- d$series_label[1]
    p <- plotly::add_trace(
      p, type = "scatter", mode = "lines", x = d$x, y = d$value, name = lab,
      line = list(color = colors[[s]], width = 2),
      text = sprintf("<b>%s%s</b>  %s", hover_fmt(d$value), y_suffix, htmltools::htmlEscape(lab)),
      hovertemplate = "%{text}<extra></extra>"
    )
  }
  shapes <- if (!is.null(ref)) list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = ref, y1 = ref,
                                         line = list(color = VIZ$muted, width = 1))) else list()
  viz_layout(p, x = c(list(title = NULL), crosshair()),
             y = list(title = y_title, tickformat = y_format, ticksuffix = if (is.null(y_format)) y_suffix else ""),
             legend = length(unique(df$series)) > 1, shapes = shapes)
}

#' Scatter of pay growth vs volume growth per role.
chart_growth_scatter <- function(df) {
  df <- df[!is.na(df$change) & !is.na(df$volume_change), , drop = FALSE]
  if (!nrow(df)) return(empty_plot())
  hover <- sprintf("<b>%s</b><br>Pay %s · Filings %s<br>Median now %s",
                   htmltools::htmlEscape(df$label), fmt_pct(df$change), fmt_pct(df$volume_change),
                   fmt_dollar(df$p50))
  p <- plotly::plot_ly() |>
    plotly::add_trace(type = "scatter", mode = "markers", x = df$volume_change, y = df$change,
                      marker = list(color = CATEGORICAL[1], size = 10, opacity = 0.85,
                                    line = list(color = VIZ$surface, width = 2)),
                      text = hover, hovertemplate = "%{text}<extra></extra>", showlegend = FALSE)
  zero <- list(
    list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper", line = list(color = VIZ$muted, width = 1)),
    list(type = "line", y0 = 0, y1 = 0, x0 = 0, x1 = 1, xref = "paper", line = list(color = VIZ$muted, width = 1))
  )
  viz_layout(p, x = list(title = "Change in number of filings", tickformat = "+.0%"),
             y = list(title = "Change in median salary", tickformat = "+.0%"),
             legend = FALSE, hovermode = "closest", shapes = zero)
}

# Tile-grid map of U.S. states (NPR-style layout) - equal-area tiles avoid the
# visual bias of large, sparsely populated states and need no map download.
STATE_TILES <- data.frame(
  state = c("AK", "ME", "VT", "NH",
            "WA", "ID", "MT", "ND", "MN", "IL", "WI", "MI", "NY", "RI", "MA",
            "OR", "NV", "WY", "SD", "IA", "IN", "OH", "PA", "NJ", "CT",
            "CA", "UT", "CO", "NE", "MO", "KY", "WV", "VA", "MD", "DE",
            "AZ", "NM", "KS", "AR", "TN", "NC", "SC", "DC",
            "OK", "LA", "MS", "AL", "GA",
            "HI", "TX", "FL", "PR"),
  col = c(0, 10, 9, 10,
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
          0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
          1, 2, 3, 4, 5, 6, 7, 8,
          3, 4, 5, 6, 7,
          0, 3, 8, 10),
  row = c(0, 0, 1, 1,
          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
          3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
          4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
          5, 5, 5, 5, 5, 5, 5, 5,
          6, 6, 6, 6, 6,
          7, 7, 7, 7),
  stringsAsFactors = FALSE
)

chart_state_tiles <- function(df, state_names) {
  tiles <- merge(STATE_TILES, df, by = "state", all.x = TRUE)
  has <- !is.na(tiles$p50)
  if (!any(has)) return(empty_plot())
  rng <- range(tiles$p50[has])
  pos <- if (diff(rng) > 0) (tiles$p50 - rng[1]) / diff(rng) else rep(0.5, nrow(tiles))
  ramp <- grDevices::colorRamp(SEQUENTIAL[2:7])
  fill <- rep("#ecebe6", nrow(tiles))
  fill[has] <- grDevices::rgb(ramp(pos[has]), maxColorValue = 255)
  text_col <- ifelse(has, ink_on(fill), VIZ$muted)
  nm <- state_names[match(tiles$state, names(state_names))]
  hover <- ifelse(has,
    sprintf("<b>%s</b>  median, %s<br>Middle 50%%: %s–%s<br>%s filings", fmt_dollar(tiles$p50),
            nm, fmt_dollar(tiles$p25), fmt_dollar(tiles$p75), fmt_int(tiles$n)),
    sprintf("%s<br>Fewer than %d filings", nm, 30L))
  p <- plotly::plot_ly() |>
    plotly::add_trace(type = "scatter", mode = "markers+text", x = tiles$col, y = -tiles$row,
                      marker = list(symbol = "square", size = 40, color = fill,
                                    line = list(color = VIZ$surface, width = 2)),
                      text = tiles$state, textfont = list(color = text_col, size = 11),
                      hovertext = hover, hovertemplate = "%{hovertext}<extra></extra>",
                      showlegend = FALSE)
  # Legend for the sequential scale: low / high swatches as annotations
  viz_layout(p, x = list(visible = FALSE, range = c(-0.7, 10.7)),
             y = list(visible = FALSE, range = c(-7.7, 0.7), scaleanchor = "x"),
             legend = FALSE, hovermode = "closest",
             annotations = list(
               list(x = 0, y = 1.06, xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE,
                    text = sprintf("<span style='color:%s'>■</span> %s  →  <span style='color:%s'>■</span> %s",
                                   SEQUENTIAL[2], fmt_dollar(rng[1]), SEQUENTIAL[7], fmt_dollar(rng[2])),
                    font = list(size = 11, color = VIZ$ink_2))
             ))
}
