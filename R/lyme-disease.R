# Figures for indicators/vector-related-health/lyme-disease.qmd.

REPO <- "lyme-disease"

# ---- Figure 1: national Lyme disease incidence, 1992-2022 --------------------

# The four case-definition eras are not peers on a shared timeline where any
# one is a natural "first" colour: common.R deliberately keeps Lyme off the
# chronological convention every other multi-series page uses. 2022 is the
# indicator's headline number (EPA's own Key Points cite it as the maximum),
# so it gets focus. 2008-2021 is the longest, most-used era and the one 2022
# is read against, so it is base. The shorter 1996-2007 and 1992-1995 eras
# are comparatively minor context.
FIG1_KEYS <- c("def_2008", "def_2022", "def_1996", "def_1990")

fig_1_plot <- function(d) {
  ggplot(d, aes(x = year, y = value, colour = definition_label, group = definition_key)) +
    geom_line_interactive(linewidth = 0.9) +
    # geom_line_interactive() draws one svg path per group, so a tooltip
    # aesthetic on it takes only the first row's value for the whole line --
    # per-year tooltips come from the points instead.
    geom_point_interactive(
      aes(tooltip = sprintf("%d - %s\n%.1f cases per 100,000 people",
                            year, definition_label, value)),
      size = 1.8
    ) +
    scale_colour_manual(
      values = label_colours(d, "definition_key", "definition_label", FIG1_KEYS),
      breaks = label_order(d, "definition_key", "definition_label", FIG1_KEYS)
    ) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = c(seq(1992, 2017, 5), 2022)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Reported cases per 100,000 people") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 4.6)

fig_1_table <- function(d) {
  out <- d[, c("year", "definition_label", "value")]
  names(out) <- c("Year", "Case definition", "Cases per 100,000 people")
  out
}

# ---- Figure 2: incidence by jurisdiction, 2022 --------------------------------

# CDC's own published threshold for a "high-incidence jurisdiction": EPA's
# narrative describes this figure as showing "the portion of the country
# where Lyme disease is common ... all states not shown had rates below 10
# cases per 100,000 people." Not a value chosen to make the chart look tidy.
FIG2_HIGH_INCIDENCE <- 10

# Ascending so the highest rate lands at the top of a horizontal bar chart.
# The jurisdiction with no report (NA, not a rate of zero) sorts last with
# na.last = TRUE, landing at the very top as an annotation, not a bar.
fig_2_sorted <- function(d) {
  d[order(d$value, decreasing = FALSE, na.last = TRUE), ]
}

fig_2_plot <- function(d) {
  d <- fig_2_sorted(d)
  d$jurisdiction <- factor(d$jurisdiction, levels = d$jurisdiction)
  reported <- !is.na(d$value)

  ggplot(d, aes(y = jurisdiction, x = value)) +
    geom_vline(
      xintercept = FIG2_HIGH_INCIDENCE,
      colour = CHART_GREY[["rule"]], linetype = "22", linewidth = 0.4
    ) +
    geom_col_interactive(
      data = d[reported, ],
      aes(
        data_id = jurisdiction,
        tooltip = sprintf("%s\n%.1f cases per 100,000 people", jurisdiction, value)
      ),
      fill = INDICATOR_PALETTE[["base"]], width = 0.72
    ) +
    geom_text_interactive(
      data = d[!reported, ],
      aes(
        x = 0, label = "no report filed for 2022",
        data_id = jurisdiction,
        tooltip = sprintf("%s\nFiled no report for 2022. Not a rate of zero.", jurisdiction)
      ),
      hjust = 0, nudge_x = 1.5, size = 2.9,
      colour = CHART_GREY[["annotation"]], fontface = "italic"
    ) +
    annotate(
      "text", x = FIG2_HIGH_INCIDENCE, y = 2.2, hjust = -0.08,
      label = paste0("CDC high-incidence threshold: ", FIG2_HIGH_INCIDENCE, " per 100,000"),
      size = 2.9, colour = CHART_GREY[["annotation"]]
    ) +
    scale_y_discrete(limits = levels(d$jurisdiction)) +
    scale_x_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, 0.04)), position = "top"
    ) +
    labs(x = "Reported cases per 100,000 people (2022)", y = NULL) +
    theme_indicator() +
    theme(
      panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      axis.line.x        = element_blank(),
      axis.text.y        = element_text(size = rel(0.72)),
      axis.ticks.length  = unit(0, "pt")
    )
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 9.5)

fig_2_table <- function(d) {
  d <- fig_2_sorted(d)
  d <- d[rev(seq_len(nrow(d))), ]  # highest first, to match the chart
  data.frame(
    Jurisdiction = d$jurisdiction,
    `Cases per 100,000 people` = ifelse(is.na(d$value), "not reported", sprintf("%.1f", d$value)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Figure 3 (the 1996-vs-2022 dot maps) has no figure function: EPA published
# the image and no data at all, so there is nothing to chart. It is wired
# directly into the .qmd as a vendored image.
