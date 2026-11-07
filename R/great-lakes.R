# Figures for indicators/freshwater-systems/great-lakes.qmd.

REPO <- "great-lakes"

# Figure 1's values are anomalies against EPA's 1981-2010 baseline, so zero is a
# real reference line, not just a spot on the axis.
BASELINE <- 0

# Lakes Michigan and Huron share one water level, so Figure 1 has four lakes and
# Figure 2 (satellite temperatures) has five.
FIG_1_LAKES <- c("Lake Erie", "Lake Michigan-Huron", "Lake Ontario", "Lake Superior")
FIG_2_LAKES <- c("Lake Erie", "Lake Huron", "Lake Michigan", "Lake Ontario", "Lake Superior")

# EPA compares a recent decade against an early one. The earlier period is the
# `base` the later one is read against; the recent period is the `focus`.
PERIOD_KEYS <- c("1995-2004", "2014-2023")

# First day of each month in a non-leap year, for the Julian-day axis.
MONTH_STARTS <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)

# ---- Figure 1: water levels of the Great Lakes, 1860-2023 --------------------

# The shaded band is the range of monthly average levels each year; the line is
# the annual average. Upstream stores them as three `series` values.
fig_1_plot <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = c(year, lake), names_from = series, values_from = value)
  w$lake <- factor(w$lake, levels = FIG_1_LAKES)
  cols <- series_colours(FIG_1_LAKES)

  ggplot(w, aes(x = year, colour = lake, fill = lake)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.25, colour = NA) +
    geom_line(aes(y = level, group = lake), linewidth = 0.6) +
    geom_point_interactive(
      aes(
        y = level, data_id = paste(lake, year),
        tooltip = sprintf("%d, %s\nAnnual average: %+.2f ft\nMonthly range: %+.2f to %+.2f ft",
                          year, lake, level, lower_bound, upper_bound)
      ),
      size = 0.8
    ) +
    scale_colour_manual(values = cols, guide = "none") +
    scale_fill_manual(values = cols, guide = "none") +
    scale_x_continuous(breaks = seq(1880, 2020, 40)) +
    facet_wrap(~lake, ncol = 2) +
    labs(x = NULL, y = "Water level anomaly (feet, vs. 1981-2010 average)") +
    theme_indicator()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 5.5)

fig_1_table <- function(d) {
  d$lake <- factor(d$lake, levels = FIG_1_LAKES)
  d$series <- factor(d$series, levels = c("level", "lower_bound", "upper_bound"),
                     labels = c("level", "lower bound", "upper bound"))
  tidyr::pivot_wider(d[order(d$lake, d$series), ], id_cols = year,
                     names_from = c(lake, series), names_sep = " ", values_from = value)
}

# ---- Figure 2: surface water temperatures, 1995-2023 -------------------------
#
# EPA draws annual averages (left) and the daily pattern for two periods (right)
# in one figure. They are two upstream datasets with different x axes, so they
# are two charts here, stacked in the same tab.

fig_2_annual_plot <- function(d) {
  d$lake <- factor(d$lake, levels = FIG_2_LAKES)
  cols <- series_colours(FIG_2_LAKES)

  ggplot(d, aes(x = year, y = value, colour = lake, group = lake)) +
    geom_line(linewidth = 0.6) +
    geom_point_interactive(
      aes(data_id = paste(lake, year),
          tooltip = sprintf("%d, %s\n%.1f°F annual average", year, lake, value)),
      size = 1
    ) +
    scale_colour_manual(values = cols, guide = "none") +
    scale_x_continuous(breaks = seq(1995, 2020, 5)) +
    facet_wrap(~lake, ncol = 2, scales = "free_y") +
    labs(x = NULL, y = "Annual average surface water temperature (°F)") +
    theme_indicator()
}

fig_2_annual <- function(d) girafe_indicator(fig_2_annual_plot(d), height = 5.5)

fig_2_annual_table <- function(d) {
  d$lake <- factor(d$lake, levels = FIG_2_LAKES)
  tidyr::pivot_wider(d[order(d$lake), ], id_cols = year, names_from = lake, values_from = value)
}

fig_2_daily_plot <- function(d) {
  d$lake <- factor(d$lake, levels = FIG_2_LAKES)
  d$julian_day <- as.integer(d$julian_day)

  ggplot(d, aes(x = julian_day, y = value, colour = period, group = interaction(lake, period))) +
    geom_line_interactive(
      aes(data_id = paste(lake, period),
          tooltip = sprintf("%s, %s average", lake, period)),
      linewidth = 0.7
    ) +
    scale_colour_manual(values = series_colours(PERIOD_KEYS), breaks = PERIOD_KEYS) +
    scale_x_continuous(breaks = MONTH_STARTS[c(TRUE, FALSE)], labels = month.abb[c(TRUE, FALSE)]) +
    facet_wrap(~lake, ncol = 2) +
    labs(x = NULL, y = "Average daily surface water temperature (°F)") +
    theme_indicator() +
    legend_top()
}

fig_2_daily <- function(d) girafe_indicator(fig_2_daily_plot(d), height = 5.5)

fig_2_daily_table <- function(d) {
  d$lake <- factor(d$lake, levels = FIG_2_LAKES)
  d$julian_day <- as.integer(d$julian_day)
  out <- tidyr::pivot_wider(d[order(d$lake, d$period), ], id_cols = julian_day,
                            names_from = c(lake, period), names_sep = ", ", values_from = value)
  names(out)[1] <- "Julian day"
  out
}
