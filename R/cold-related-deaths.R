# Figures for indicators/temperature-related-health/cold-related-deaths.qmd.

REPO <- "cold-related-deaths"

# ---- Figure 1: annual cold-related death rates -------------------------------

# *_plot() builds the plain ggplot object; fig_*() wraps it for the page. The
# split exists so a plot can be ggsave()'d for a static check without pulling
# in the htmlwidget machinery.
fig_1_plot <- function(d) {
  # icd_revision splits the underlying-cause line at the classification
  # change; series_key alone would draw straight through 1998/1999.
  d$seg <- paste(d$series_key, d$icd_revision, sep = "/")
  # underlying-cause-only is the narrower, longer-running baseline; the broader
  # underlying-or-contributing series is the one the page's text draws to.
  order <- c("underlying", "underlying_or_contributing")

  ggplot(d, aes(x = year, y = value, colour = series_label, group = seg)) +
    geom_line_interactive(linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = series_key,
        tooltip = sprintf(
          "%d — %s\n%.2f deaths per million (%s)",
          year, series_label, value, icd_revision
        )
      ),
      size = 2.2
    ) +
    scale_colour_manual(
      values = label_colours(d, "series_key", "series_label", order),
      breaks = label_order(d, "series_key", "series_label", order)
    ) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(1980, 2015, 5)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Death rate (per million people)") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}
