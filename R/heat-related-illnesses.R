# Figures for indicators/temperature-related-health/heat-related-illnesses.qmd.

REPO <- "heat-related-illnesses"

# ---- Figure 1: annual hospitalization rate, 20 states -------------------------

# *_plot() builds the plain ggplot object; fig_*() wraps it for the page. A
# single series, so no colour legend.
fig_1_plot <- function(d) {
  ggplot(d, aes(x = year, y = value)) +
    geom_line_interactive(colour = INDICATOR_PALETTE[["base"]], linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = year,
        tooltip = sprintf("%d\n%.1f hospitalizations per 100,000 people", year, value)
      ),
      colour = INDICATOR_PALETTE[["base"]], size = 2.4
    ) +
    scale_x_continuous(breaks = 2001:2010) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Hospitalization rate (per 100,000 people)") +
    theme_indicator()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  data.frame(
    Year = d$year,
    `Hospitalizations per 100,000 people` = d$value,
    check.names = FALSE
  )
}

# ---- Figure 2: average rate by state, 23 states --------------------------------

# Sorted ascending so the highest rate lands at the top of a horizontal bar
# chart. EPA published this figure as a map; the state-level values are the
# same, only the presentation differs.
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
          "%s\n%.2f hospitalizations per 100,000 people (2001-2010 average)",
          state, value
        )
      ),
      fill = INDICATOR_PALETTE[["base"]], width = 0.72
    ) +
    scale_x_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, 0.04)),
      position = "top"
    ) +
    labs(x = "Average annual hospitalization rate, 2001-2010 (per 100,000 people)", y = NULL) +
    theme_indicator() +
    theme(
      # theme_indicator() is built for a vertical chart: gridlines running
      # across the categories and a baseline under them. Both flip here.
      panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      axis.line.x        = element_blank(),
      axis.text.y        = element_text(size = rel(0.8)),
      axis.ticks.length  = unit(0, "pt")
    )
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 6.5)

fig_2_table <- function(d) {
  d <- fig_2_sorted(d)
  d <- d[rev(seq_len(nrow(d))), ]  # highest first, to match the chart
  data.frame(
    State = d$state,
    `Average rate (per 100,000 people)` = sprintf("%.2f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ---- Figure 3: hospitalizations by sex and by age, 20 states ------------------

# group_type (from the indicator repository's build_data.R) distinguishes
# EPA's two independent breakdowns of the same 20-state total: the sex total
# and the age total differ by 14 hospitalizations in EPA's own published
# figures. Two small panels, not one axis of seven unrelated bars, so a reader
# cannot read "Male" and "0-4" as peers of each other.
FIG3_GROUP_ORDER  <- c("Male", "Female", "0-4", "5-14", "15-34", "35-64", "65+")
FIG3_PANEL_LABELS <- c(sex = "By sex", age = "By age group")

fig_3_plot <- function(d) {
  d$group_label <- factor(d$group_label, levels = FIG3_GROUP_ORDER)
  d$panel        <- factor(FIG3_PANEL_LABELS[d$group_type], levels = unname(FIG3_PANEL_LABELS))

  ggplot(d, aes(x = group_label, y = value)) +
    geom_col_interactive(
      aes(
        data_id = group_key,
        tooltip = sprintf(
          "%s\n%s hospitalizations (2001-2010 total)",
          group_label, format(value, big.mark = ",")
        )
      ),
      fill = INDICATOR_PALETTE[["base"]], width = 0.6
    ) +
    # free_x, not free_y: sex has two categories and age has five, but the
    # counts are directly comparable across panels, so the count axis stays
    # shared.
    facet_wrap(~panel, scales = "free_x") +
    scale_y_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, 0.08)),
      labels = function(x) format(x, big.mark = ",")
    ) +
    labs(x = NULL, y = "Total hospitalizations, 2001-2010") +
    theme_indicator()
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d))

fig_3_table <- function(d) {
  d$group_label <- factor(d$group_label, levels = FIG3_GROUP_ORDER)
  d <- d[order(d$group_label), ]
  data.frame(
    Breakdown = ifelse(d$group_type == "sex", "By sex", "By age group"),
    Group = as.character(d$group_label),
    `Total hospitalizations` = d$value,
    check.names = FALSE
  )
}
