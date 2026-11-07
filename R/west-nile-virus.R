# Figures for indicators/vector-related-health/west-nile-virus.qmd.

REPO <- "west-nile-virus"

# ---- Figure 1: national annual incidence, 2002-2023 ---------------------------

# *_plot() builds the plain ggplot object; fig_*() wraps it for the page. The
# split exists so a plot can be ggsave()'d for a static check without pulling
# in the htmlwidget machinery. A single series, so no colour legend.
fig_1_plot <- function(d) {
  ggplot(d, aes(x = year, y = value)) +
    geom_line_interactive(colour = INDICATOR_PALETTE[["base"]], linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = year,
        tooltip = sprintf("%d\n%.2f cases per 100,000 people", year, value)
      ),
      colour = INDICATOR_PALETTE[["base"]], size = 2.4
    ) +
    # 2002 to 2023 spans 21 years, divisible by 3, so a 3-year step lands
    # exactly on both endpoints without a partial interval at either end.
    scale_x_continuous(breaks = seq(2002, 2023, by = 3)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Reported cases per 100,000 people") +
    theme_indicator()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  data.frame(
    Year = d$year,
    `Cases per 100,000 people` = sprintf("%.3f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ---- Figure 2: average annual incidence by state, 2002-2023 -------------------

# Sorted ascending so the highest rate lands at the top of a horizontal bar
# chart, matching the site's convention for other 50-state figures.
fig_2_sorted <- function(d) {
  d[order(d$value, decreasing = FALSE), ]
}

fig_2_plot <- function(d) {
  d <- fig_2_sorted(d)
  d$state <- factor(d$state, levels = d$state)

  ggplot(d, aes(y = state, x = value)) +
    geom_col_interactive(
      aes(
        data_id = state,
        tooltip = sprintf(
          "%s\n%.2f cases per 100,000 people, averaged annually (2002-2023)",
          state, value
        )
      ),
      fill = INDICATOR_PALETTE[["base"]], width = 0.72
    ) +
    scale_x_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, 0.04)),
      position = "top"
    ) +
    labs(x = "Average annual cases per 100,000 people, 2002-2023", y = NULL) +
    theme_indicator() +
    theme(
      # theme_indicator() is built for a vertical chart: gridlines running
      # across the categories and a baseline under them. Both flip here.
      panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      axis.line.x        = element_blank(),
      axis.text.y        = element_text(size = rel(0.72)),
      axis.ticks.length  = unit(0, "pt")
    )
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 9.5)

fig_2_table <- function(d) {
  d <- fig_2_sorted(d)
  d <- d[rev(seq_len(nrow(d))), ]  # highest first, to match the chart
  data.frame(
    State = d$state,
    `Average annual cases per 100,000 people` = sprintf("%.3f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
