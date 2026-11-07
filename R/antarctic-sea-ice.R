# Figures for indicators/coastal-and-marine/antarctic-sea-ice.qmd.

REPO <- "antarctic-sea-ice"

# ---- Figure 1: February and September Antarctic sea ice extent, 1979-2024 ----

# September (the annual maximum) and February (the annual minimum) sit about
# six million square miles apart, so both share one axis starting at zero
# rather than being rescaled into separate panels.
FIG1_KEYS <- c("September", "February")

fig_1_plot <- function(d) {
  d$month <- factor(d$month, levels = FIG1_KEYS)

  ggplot(d, aes(x = year, y = value, colour = month, group = month)) +
    geom_line(linewidth = 0.7) +
    geom_point_interactive(
      aes(
        data_id = paste(year, month),
        tooltip = sprintf("%s %d\n%.2f million square miles", month, year, value)
      ),
      size = 0.9
    ) +
    scale_colour_manual(values = series_colours(FIG1_KEYS)) +
    scale_x_continuous(breaks = seq(1980, 2020, 10)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Extent (million square miles)") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = year, names_from = month, values_from = value)
  data.frame(
    Year                                = w$year,
    `February (million square miles)`  = w$February,
    `September (million square miles)` = w$September,
    check.names = FALSE
  )
}
