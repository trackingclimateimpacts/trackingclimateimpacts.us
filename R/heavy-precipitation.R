# Figures for indicators/weather-and-extreme-events/heavy-precipitation.qmd.

REPO <- "heavy-precipitation"

# Both figures share the same shape: one annual series plus its own 9-year
# moving average (upstream gives both figures' moving average the series_key
# "moving_average" for exactly this reason, see that repository's
# R/build_data.R). Declaring the same two-role order for both figures means
# the raw annual series always draws in the "base" slot and the smoothed
# trend always draws in "focus", in both tabs.

# ---- Figure 1: extreme one-day precipitation events, annual ------------------

# EPA's own chart draws the annual series as bars and the moving average as an
# overlaid line ("The bars represent individual years, while the line is a
# nine-year weighted average").
fig_1_plot <- function(d) {
  order <- c("annual", "moving_average")
  bars  <- d[d$series_key == "annual", ]
  line  <- d[d$series_key == "moving_average", ]
  cols  <- label_colours(d, "series_key", "series_label", order)
  brks  <- label_order(d, "series_key", "series_label", order)
  tt    <- function(year, label, value) {
    sprintf("%d\n%s: %s", year, label, scales::percent(value, accuracy = 0.1))
  }

  ggplot() +
    geom_col_interactive(
      data = bars,
      aes(x = year, y = value, fill = series_label,
          data_id = year, tooltip = tt(year, series_label, value)),
      width = 0.8
    ) +
    geom_line_interactive(
      data = line,
      aes(x = year, y = value, colour = series_label, group = series_label,
          data_id = series_label, tooltip = tt(year, series_label, value)),
      linewidth = 1
    ) +
    scale_fill_manual(values = cols, breaks = brks) +
    scale_colour_manual(values = cols, breaks = brks) +
    guides(fill = guide_legend(), colour = "none") +
    scale_x_continuous(breaks = seq(1910, 2020, 20)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Percent of land area") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}

# ---- Figure 2: unusually high annual precipitation ----------------------------

# EPA's own chart draws both series as lines, the moving average thicker
# ("The thicker line shows a nine-year weighted average that smooths out some
# of the year-to-year fluctuations").
fig_2_plot <- function(d) {
  order <- c("annual", "moving_average")
  raw   <- d[d$series_key == "annual", ]
  ma    <- d[d$series_key == "moving_average", ]
  cols  <- label_colours(d, "series_key", "series_label", order)

  ggplot(d, aes(x = year, y = value, colour = series_label, group = series_label)) +
    geom_line_interactive(
      data = raw,
      aes(data_id = series_label,
          tooltip = sprintf("%d\n%s: %s", year, series_label, scales::percent(value, accuracy = 0.1))),
      linewidth = 0.6
    ) +
    geom_line_interactive(
      data = ma,
      aes(data_id = series_label,
          tooltip = sprintf("%d\n%s: %s", year, series_label, scales::percent(value, accuracy = 0.1))),
      linewidth = 1.4
    ) +
    scale_colour_manual(values = cols, breaks = label_order(d, "series_key", "series_label", order)) +
    scale_x_continuous(breaks = seq(1900, 2020, 20)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Percent of land area") +
    theme_indicator() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d))

fig_2_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}
