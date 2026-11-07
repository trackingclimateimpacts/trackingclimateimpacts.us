# Figures for indicators/ecosystems/black-guillemots-cooper-island.qmd.

REPO <- "black-guillemots-cooper-island"

# ---- Figure 1: breeding pairs and nest sites on Cooper Island, 1972-2023 -----

FIG1_ORDER <- c("breeding_pairs", "nest_sites_installed")
FIG1_LABELS <- c(
  breeding_pairs       = "Breeding pairs",
  nest_sites_installed = "Nest sites installed"
)
# EPA's caption names a solid green line for breeding pairs and a dashed orange
# line for nest sites; the page's caption names these colours instead, the
# palette's nearest to EPA's, so the two have to change together.
FIG1_COLOURS <- stats::setNames(
  unname(INDICATOR_PALETTE[c("compare", "focus")]),
  FIG1_LABELS[FIG1_ORDER]
)

fig_1_plot <- function(d) {
  # Upstream omits the years EPA left blank (breeding pairs, 1973-1974);
  # restoring them as NA makes the line break there instead of bridging them.
  d <- tidyr::complete(d, series, year = seq(min(year), max(year)))
  d$label <- unname(FIG1_LABELS[d$series])
  # Rows draw in order, so nest sites go first: in 1972 both series are 10, and
  # breeding pairs' lone point before the gap would otherwise be hidden.
  d <- d[order(-match(d$series, FIG1_ORDER), d$year), ]

  ggplot(d, aes(x = year, y = value, colour = label, linetype = label, group = series)) +
    geom_line_interactive(aes(data_id = series), linewidth = 0.9, na.rm = TRUE) +
    geom_point_interactive(
      aes(data_id = series, tooltip = sprintf("%d\n%s: %d", year, label, as.integer(value))),
      size = 1.6, na.rm = TRUE
    ) +
    scale_colour_manual(
      values = FIG1_COLOURS,
      breaks = label_order(d, "series", "label", FIG1_ORDER)
    ) +
    scale_linetype_manual(
      values = stats::setNames(c("solid", "dashed"), FIG1_LABELS[FIG1_ORDER]),
      breaks = label_order(d, "series", "label", FIG1_ORDER)
    ) +
    scale_x_continuous(breaks = seq(1975, 2020, 5)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Number of breeding pairs or nest sites") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  d$label <- factor(FIG1_LABELS[d$series], levels = FIG1_LABELS[FIG1_ORDER])
  w <- tidyr::pivot_wider(d[order(d$label), ], id_cols = year, names_from = label, values_from = value)
  w[order(w$year), ]
}
