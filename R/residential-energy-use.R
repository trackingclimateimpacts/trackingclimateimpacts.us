# Figures for indicators/societal-systems/residential-energy-use.qmd.

REPO <- "residential-energy-use"

# Each dataset mixes two units in one long file (a per-capita energy measure
# and degree days), so each gets its own stacked panel with a free y-scale
# rather than a dual-axis chart (house style avoids dual axes; see also
# R/tropical-cyclone-activity.R). Each panel holds exactly one series, so no
# colour legend is needed -- the facet strip alone names the line.

# ---- Figure 1: summer electricity use and cooling degree days, annual -------

FIG1_PANEL_LABELS <- c(
  electricity_per_capita = "Summer electricity use per capita (kilowatt-hours per person)",
  cooling_degree_days    = "Summer cooling degree days"
)
FIG1_ORDER <- c("electricity_per_capita", "cooling_degree_days")

fig_1_plot <- function(d) {
  d$panel <- factor(FIG1_PANEL_LABELS[d$series_key], levels = unname(FIG1_PANEL_LABELS))

  ggplot(d, aes(x = year, y = value, colour = series_key, group = series_key)) +
    # geom_line_interactive() draws one svg path per group, so a tooltip
    # aesthetic on it takes only the first row's value for the whole line --
    # per-year tooltips come from the points instead.
    geom_line_interactive(linewidth = 0.9) +
    geom_point_interactive(
      aes(tooltip = sprintf("%d\n%s: %.1f %s", year, series_label, value, unit)),
      size = 1.8
    ) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = series_colours(FIG1_ORDER), guide = "none") +
    scale_x_continuous(breaks = seq(1975, 2020, 5)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = NULL) +
    theme_indicator()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 5.2)

fig_1_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}

# ---- Figure 2: winter natural gas use and heating degree days, annual -------

FIG2_PANEL_LABELS <- c(
  natural_gas_per_capita = "Winter natural gas use per capita (cubic feet per person)",
  heating_degree_days    = "Winter heating degree days"
)
FIG2_ORDER <- c("natural_gas_per_capita", "heating_degree_days")

fig_2_plot <- function(d) {
  d$panel <- factor(FIG2_PANEL_LABELS[d$series_key], levels = unname(FIG2_PANEL_LABELS))

  ggplot(d, aes(x = year, y = value, colour = series_key, group = series_key)) +
    geom_line_interactive(linewidth = 0.9) +
    geom_point_interactive(
      aes(tooltip = sprintf("%d\n%s: %.1f %s", year, series_label, value, unit)),
      size = 1.8
    ) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = series_colours(FIG2_ORDER), guide = "none") +
    scale_x_continuous(breaks = seq(1975, 2020, 5)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = NULL) +
    theme_indicator()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 5.2)

fig_2_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}
