# Figures for indicators/freshwater-systems/snake-river.qmd.

REPO <- "snake-river"

# ---- Figure 1: average August temperature in the Snake River, 1960-2022 ------

# One series, so one palette role. Points carry the tooltip; the line joins
# them because the measure is a single site's yearly August mean.
fig_1_plot <- function(d) {
  d$tooltip <- sprintf("%d\n%.1f °F", d$year, d$value)
  col <- INDICATOR_PALETTE[["base"]]

  ggplot(d, aes(x = year, y = value)) +
    geom_line(colour = col, linewidth = 0.7) +
    geom_point_interactive(
      aes(data_id = year, tooltip = tooltip),
      colour = col, size = 1.6
    ) +
    scale_x_continuous(breaks = seq(1960, 2020, 10)) +
    labs(x = NULL, y = "Average August temperature (°F)") +
    theme_indicator()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 4.2)

fig_1_table <- function(d) {
  data.frame(
    Year = d$year,
    `August mean temperature (°F)` = sprintf("%.2f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
