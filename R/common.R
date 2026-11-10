# Helpers shared by every page. Each indicator .qmd sources this first, then
# its own R/<slug>.R.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggiraph)
})

# ---- Reading data from the indicator repositories ----------------------------

#' Build the raw.githubusercontent.com URL for a file in an indicator
#' repository's data/ directory.
indicator_url <- function(repo, file) {
  sprintf(
    "https://raw.githubusercontent.com/trackingclimateimpacts/%s/main/data/%s",
    repo, file
  )
}

#' Read one of an indicator repository's generated datasets.
#'
#' Values are stored as text upstream so the source file's precision survives
#' byte for byte. This is where they become numeric, once, for plotting. Rows
#' carrying a suppression flag get NA, which is what makes a chart draw a gap
#' instead of a zero.
read_indicator <- function(repo, file) {
  d <- readr::read_csv(
    indicator_url(repo, file),
    col_types = readr::cols(.default = readr::col_character()),
    na = character(), progress = FALSE
  )
  d$value <- suppressWarnings(as.numeric(d$value))
  if ("year" %in% names(d)) d$year <- as.integer(d$year)
  if ("date" %in% names(d)) d$date <- as.Date(d$date)
  d
}

#' Read an indicator repository's data dictionary.
read_meta <- function(repo) {
  yaml::read_yaml(indicator_url(repo, "meta.yml"))
}

#' Pull one dataset's entry out of an already-read meta.yml, by filename.
meta_for <- function(meta, file) {
  hit <- Filter(function(d) identical(d$file, file), meta$datasets)
  if (length(hit) != 1L) stop("No meta.yml entry for ", file, call. = FALSE)
  hit[[1]]
}

#' The bold title and source line printed above a figure, from meta.yml.
#' `ref` is the reference number(s) EPA attaches to the data source, if any; it
#' is rendered as the same anchor-linked marker the page prose uses. `number`
#' replaces the figure number in meta.yml's title, for a page that orders its
#' figures differently from EPA's.
figure_caption <- function(meta, file, ref = NULL, number = NULL) {
  m <- meta_for(meta, file)
  if (!is.null(number)) {
    m$figure_title <- sub("^Figure [0-9]+[.]", sprintf("Figure %d.", number), m$figure_title)
  }
  marker <- if (is.null(ref)) "" else
    sprintf("^%s^", paste(sprintf("[%d](#ref-%d)", ref, ref), collapse = ","))
  knitr::asis_output(sprintf(
    "**%s**\n\nData source: %s%s <br> Web update: %s\n",
    m$figure_title, m$data_source, marker, m$web_update
  ))
}

#' The link to an indicator's technical documentation PDF, from meta.yml.
technical_documentation_link <- function(meta) {
  knitr::asis_output(sprintf(
    "- [Download related technical information (PDF)](%s)\n",
    meta$indicator$technical_documentation
  ))
}

# ---- Chart colours -----------------------------------------------------------

# The categorical palette: ltc's `expevo`, lifted out of its published order
# into the role order named below. Of the 31 palettes ltc ships, expevo is the
# one that carries a blue, an orange, a teal and a plum at once with no
# near-white slot, so it is the only one that fills all four roles a chart here
# needs. The two spare slots are what a fifth or sixth series would draw in.
INDICATOR_PALETTE <- stats::setNames(
  ltc::ltc("expevo")[c(5, 1, 3, 4, 2, 6)],
  c(
    "base",     # the series everything else is read against
    "focus",    # the series most worth the reader's attention
    "compare",  # a peer series read alongside the baseline
    "other",    # a category that is not a peer of the others
    "extra",    # a fifth series, when a chart has one
    "neutral"   # a series deliberately pushed into the background
  )
)

# A palette that lost slots would index to NA and draw in ggplot's
# missing-value grey without complaint.
stopifnot(
  "ltc's expevo palette no longer has six slots" =
    length(INDICATOR_PALETTE) == 6L && !anyNA(INDICATOR_PALETTE)
)

# Everything that is not a data series: gridlines, axis furniture, annotations.
CHART_GREY <- c(
  ink        = "#1e1e1e",  # facet strip text, tooltip background
  text       = "#464646",  # axis titles, tick labels, the x axis line
  annotation = "#6b6b66",  # in-panel notes
  rule       = "#9a9a94",  # reference and breakpoint lines
  grid       = "#dcdcd7",  # major gridlines
  surface    = "#ffffff"   # marker rings, drawn over the page background
)

#' Assign a figure's series keys to palette slots, in the order given.
#'
#' Errors on more keys than slots rather than recycling: an unmapped series
#' would otherwise draw in ggplot's missing-value grey with no warning.
series_colours <- function(keys) {
  if (length(keys) > length(INDICATOR_PALETTE)) {
    stop(
      "INDICATOR_PALETTE has ", length(INDICATOR_PALETTE), " slots but ",
      length(keys), " series keys were given.",
      call. = FALSE
    )
  }
  stats::setNames(unname(INDICATOR_PALETTE[seq_along(keys)]), keys)
}

#' The same mapping, re-keyed to the label column a legend displays.
#'
#' `keys` fixes which slot each series draws in; `label_order()` fixes the order
#' the legend reads in. They are the same vector on every page but Lyme, whose
#' four case-definition eras are deliberately not coloured chronologically.
label_colours <- function(d, key_col, label_col, keys) {
  stats::setNames(
    unname(series_colours(keys)),
    d[[label_col]][match(keys, d[[key_col]])]
  )
}

label_order <- function(d, key_col, label_col, order) {
  d[[label_col]][match(order, d[[key_col]])]
}

# ---- Chart styling -----------------------------------------------------------

# Font family is left as the theme default: SVG text metrics are resolved at
# build time, so pinning a family that is not installed everywhere makes the
# same chart render differently on a different machine.
theme_indicator <- function(base_size = 13) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4),
      axis.title         = element_text(colour = CHART_GREY[["text"]], size = rel(0.9)),
      axis.text          = element_text(colour = CHART_GREY[["text"]]),
      axis.line.x        = element_line(colour = CHART_GREY[["text"]], linewidth = 0.4),
      plot.title         = element_blank(),   # the caption block carries the title
      plot.margin        = margin(4, 4, 4, 4),
      strip.text         = element_text(colour = CHART_GREY[["ink"]], face = "bold",
                                        hjust = 0, size = rel(0.95)),
      legend.position    = "none"             # each figure sets its own legend
    )
}

legend_top <- function() {
  theme(
    legend.position      = "top",
    legend.title         = element_blank(),
    legend.justification = "left",
    legend.margin        = margin(0, 0, 6, 0),
    legend.text          = element_text(size = rel(0.85)),
    legend.key.width     = unit(1.4, "lines")
  )
}

# ---- Interactive output ------------------------------------------------------

GIRAFE_OPTS <- list(
  opts_hover(css = "stroke-width:3.5;"),
  opts_hover_inv(css = "opacity:0.3;"),
  opts_tooltip(css = paste0(
    "background:", CHART_GREY[["ink"]], ";color:#fff;padding:6px 10px;",
    "border-radius:4px;font-family:sans-serif;font-size:12px;max-width:320px;"
  )),
  opts_toolbar(position = "topright", saveaspng = TRUE, fixed = TRUE, pngname = "climate-indicator"),
  opts_sizing(rescale = TRUE, width = 1)
)

#' Wrap a ggplot as a girafe htmlwidget: build-time SVG, hover tooltips, no CDN
#' dependency.
girafe_indicator <- function(p, height = 4.2) {
  ggiraph::girafe(
    ggobj = p, width_svg = 8, height_svg = height,
    options = GIRAFE_OPTS
  )
}

#' A multi-line chart whose legend is the series selector.
#'
#' Clicking a legend entry (marker and name together) hides or shows that line,
#' which is how a reader strips a busy figure down to the series they want to
#' compare; every series starts visible. This is an echarts4r chart rather than
#' a ggiraph one because ggiraph's legend keys are 1-6px glyphs whose labels
#' cannot be linked to the plotted lines.
#'
#' `wide` has a `year` column plus one column per series, named by the label the
#' legend shows. `colours` is a named vector, labels to colours, in legend
#' order. `styles` overrides the default marker, width and dash for a label:
#' `list("Label" = list(symbol = "none", width = 3.5, type = "dashed"))`.
#' `value_format` is a JavaScript function body for the tooltip, taking `v`.
echarts_legend_chart <- function(wide, colours, y_title, value_format,
                                 x_min, x_max, x_interval = 20, y_min = NULL,
                                 styles = list()) {
  e <- echarts4r::e_charts_(wide, "year", width = "100%", height = "440px")
  for (label in names(colours)) {
    st <- utils::modifyList(
      list(symbol = "circle", width = 2, type = "solid"), as.list(styles[[label]])
    )
    e <- echarts4r::e_line_(
      e, label, name = label,
      symbol = st$symbol, symbolSize = 5,
      lineStyle = list(color = colours[[label]], width = st$width, type = st$type),
      itemStyle = list(color = colours[[label]])
    )
  }

  axis_text <- list(color = CHART_GREY[["text"]], fontSize = 13)
  e |>
    echarts4r::e_x_axis(
      type = "value", min = x_min, max = x_max, interval = x_interval,
      splitLine = list(show = FALSE),
      axisLine = list(lineStyle = list(color = CHART_GREY[["text"]])),
      # A plain function: ECharts' number formatting would print 1,900.
      axisLabel = c(axis_text, list(formatter = htmlwidgets::JS("function (v) { return String(v); }")))
    ) |>
    echarts4r::e_y_axis(
      name = y_title, nameLocation = "middle", nameGap = 40,
      min = y_min, nameTextStyle = axis_text, axisLabel = axis_text,
      splitLine = list(lineStyle = list(color = CHART_GREY[["grid"]]))
    ) |>
    echarts4r::e_legend(
      top = 0, left = 0, itemWidth = 28, itemHeight = 12, itemGap = 18,
      textStyle = list(color = CHART_GREY[["ink"]], fontSize = 14)
    ) |>
    echarts4r::e_tooltip(
      trigger = "axis",
      backgroundColor = CHART_GREY[["ink"]], borderWidth = 0,
      textStyle = list(color = CHART_GREY[["surface"]], fontSize = 12),
      valueFormatter = htmlwidgets::JS(
        paste0("function (v) { return v == null ? '' : ", value_format, "; }")
      )
    ) |>
    echarts4r::e_grid(top = 80, left = 56, right = 24, bottom = 36) |>
    echarts4r::e_toolbox_feature("saveAsImage", title = "Download image", pixelRatio = 2)
}

# ---- Misc --------------------------------------------------------------------

print_chunk <- function(url) {
  cat(readLines(url), sep = '\n')
}
