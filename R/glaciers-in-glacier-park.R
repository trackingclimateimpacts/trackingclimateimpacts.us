# Figures for indicators/snow-and-ice/glaciers-in-glacier-park.qmd.

REPO <- "glaciers-in-glacier-park"

# ---- Figure 1: total glacier surface area in Glacier National Park, 1966-2015 ----

# Four survey years, unevenly spaced, so bars sit on a continuous year axis to
# keep the 32-year gap before 1998 visibly longer than the gaps after it.
fig_1_plot <- function(d) {
  ggplot(d, aes(x = year, y = value)) +
    geom_col_interactive(
      aes(
        data_id = year,
        tooltip = sprintf("%d\n%.2f square miles", year, value)
      ),
      fill = INDICATOR_PALETTE[["base"]], width = 4
    ) +
    geom_text(
      aes(label = sprintf("%.1f", value)),
      vjust = -0.5, size = 3.6, colour = CHART_GREY[["text"]]
    ) +
    scale_x_continuous(breaks = d$year) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    labs(x = NULL, y = "Total glacier area (square miles)") +
    theme_indicator()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  data.frame(
    Year                                  = d$year,
    `Total glacier area (square miles)`   = d$value,
    check.names = FALSE
  )
}
