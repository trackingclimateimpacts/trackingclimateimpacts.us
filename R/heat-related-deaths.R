# Figures for indicators/temperature-related-health/heat-related-deaths.qmd.

REPO <- "heat-related-deaths"

# ---- Figure 1: annual heat-related death rates -------------------------------

# *_plot() builds the plain ggplot object; fig_*() wraps it for the page. The
# split exists so a plot can be ggsave()'d for a static check without pulling
# in the htmlwidget machinery.
fig_1_plot <- function(d) {
  # icd_revision splits the line at the classification change; series_key
  # alone would draw straight through 1998/1999.
  d$seg <- paste(d$series_key, d$icd_revision, sep = "/")
  # Underlying-cause-only is the narrower baseline; the broader
  # underlying-or-contributing series gets the attention colour.
  order <- c("underlying_all_year", "underlying_or_contributing_may_sep")

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
    scale_x_continuous(breaks = seq(1980, 2020, 10)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Death rate (per million people)") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}

# ---- Figure 2: summer heat + cardiovascular disease death rates --------------

fig_2_plot <- function(d) {
  # The general population is the baseline the two higher-risk groups are read
  # against. EPA's caption names these in a different order than the source
  # file lists its columns, so the order is written out rather than inferred.
  order <- c("general", "age_65_plus", "nh_black")
  d$population_key <- factor(d$population_key, levels = order)

  ggplot(d, aes(x = year, y = value, colour = population_label, group = population_key)) +
    geom_line_interactive(linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = population_key,
        tooltip = ifelse(
          flag == "suppressed",
          sprintf("%d — %s\nSuppressed: too few deaths to publish", year, population_label),
          sprintf("%d — %s\n%.2f deaths per million", year, population_label, value)
        )
      ),
      size = 2.2
    ) +
    scale_colour_manual(
      values = label_colours(d, "population_key", "population_label", order),
      breaks = label_order(d, "population_key", "population_label", order)
    ) +
    scale_x_continuous(breaks = seq(2000, 2020, 5)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Death rate (per million people)") +
    theme_indicator() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d))

fig_2_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = population_label, values_from = value)
}

# ---- Example figure: 1995 Chicago heat wave ----------------------------------

EXAMPLE_PANEL_LABELS <- c(
  "deaths"             = "Number of daily deaths",
  "degrees Fahrenheit" = paste0("Daily high temperature (", intToUtf8(0x00B0), "F)")
)

fig_example_plot <- function(d) {
  d$panel <- factor(EXAMPLE_PANEL_LABELS[d$unit], levels = unname(EXAMPLE_PANEL_LABELS))
  # The 1990-2000 average is the baseline 1995 departs from; temperature is a
  # different measure, not a peer series.
  order <- c("deaths_avg_1990_2000", "deaths_1995", "high_temp_f")
  d$measure_key <- factor(d$measure_key, levels = order)

  # Shade the acute event: the contiguous run of days where 1995 deaths ran at
  # least 50 above the 1990-2000 average for that date. Derived from the data,
  # not copied from any published date range.
  wide <- tidyr::pivot_wider(d, id_cols = date, names_from = measure_key, values_from = value)
  excess_days <- wide$date[(wide$deaths_1995 - wide$deaths_avg_1990_2000) >= 50]
  highlight <- data.frame(xmin = min(excess_days), xmax = max(excess_days))

  # group is explicit because the tooltip string below is unique per row; left
  # implicit, ggplot infers grouping from every discrete aesthetic in a layer,
  # including tooltip, which would put each point in its own group of one and
  # silently break every line into isolated dots.
  ggplot(d, aes(x = date, y = value, colour = measure_label, group = measure_key)) +
    geom_rect(
      data = highlight, inherit.aes = FALSE,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = INDICATOR_PALETTE[["base"]], alpha = 0.08
    ) +
    geom_line_interactive(
      aes(
        data_id = measure_key, linetype = measure_key,
        tooltip = sprintf("%s\n%s: %.1f", format(date, "%B %d, %Y"), measure_label, value)
      ),
      linewidth = 0.8
    ) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_colour_manual(
      values = label_colours(d, "measure_key", "measure_label", order),
      breaks = label_order(d, "measure_key", "measure_label", order[1:2])
    ) +
    scale_linetype_manual(
      values = stats::setNames(c("solid", "solid", "22"), order),
      guide = "none"
    ) +
    scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
    labs(x = NULL, y = NULL) +
    theme_indicator() +
    legend_top() +
    theme(panel.spacing = unit(1, "lines"))
}

fig_example <- function(d) girafe_indicator(fig_example_plot(d), height = 5.2)

fig_example_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = date, names_from = measure_label, values_from = value)
}
