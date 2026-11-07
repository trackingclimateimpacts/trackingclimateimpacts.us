# Figures for indicators/snow-and-ice/alaskan-rivers.qmd.

REPO <- "alaskan-rivers"

# ---- Figure 1: ice breakup dates for three Alaskan rivers, 1896-2024 ---------

# Rivers in EPA's column order, which is also the order their records begin:
# the Yukon in 1896, the Tanana in 1917, the Kuskokwim in 1924.
FIG1_KEYS <- c("Yukon River", "Tanana River", "Kuskokwim River")

fig_1_plot <- function(d) {
  d$river <- factor(d$river, levels = FIG1_KEYS)

  ggplot(d, aes(x = year, y = value, colour = river, group = river)) +
    geom_line_interactive(aes(data_id = river), linewidth = 0.7) +
    geom_point_interactive(
      aes(
        data_id = river,
        tooltip = sprintf("%s, %d\nBreakup: %.1f days from January 1", river, year, value)
      ),
      size = 0.7
    ) +
    scale_colour_manual(values = series_colours(FIG1_KEYS)) +
    scale_x_continuous(breaks = seq(1900, 2020, 20)) +
    labs(x = NULL, y = "Breakup date (days from January 1)") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = year, names_from = river, values_from = value)
  out <- w[c("year", FIG1_KEYS)]
  names(out) <- c("Year", paste(FIG1_KEYS, "(days from January 1)"))
  out
}
