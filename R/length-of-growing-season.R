# Figures for indicators/ecosystems/length-of-growing-season.qmd.

REPO <- "length-of-growing-season"

# Every series on this page is a deviation or a change from a fixed reference
# point (an 1895-2023 average, or 1895 itself), so zero is a real value the
# reader has to be able to find, not just wherever the axis happens to start.
BASELINE <- 0

REGION_LABELS <- c(east = "East", west = "West")
EVENT_LABELS  <- c(last_spring_frost = "Last spring frost", first_fall_frost = "First fall frost")

# ---- Figure 1: national growing season length ---------------------------------

fig_1_plot <- function(d) {
  ggplot(d, aes(x = year, y = value)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_line_interactive(colour = INDICATOR_PALETTE[["base"]], linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = year,
        tooltip = sprintf("%d\n%+.1f days vs. the 1895-2023 average", year, value)
      ),
      colour = INDICATOR_PALETTE[["base"]], size = 1.4
    ) +
    scale_x_continuous(breaks = seq(1900, 2020, 20)) +
    labs(x = NULL, y = "Days shorter or longer than average") +
    theme_indicator()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  data.frame(
    Year = d$year,
    "Days shorter or longer than average" = sprintf("%+.2f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ---- Figure 2: east versus west ------------------------------------------------
#
# EPA's own key point is that the West has gained growing season length about
# twice as fast as the East, so the western series takes `focus` and the
# eastern series the `base` it is read against. The legend still follows the
# figure's own title, "West Versus East", which puts the two in the opposite
# order from their colour roles.
FIG2_COLOUR_KEYS <- c("east", "west")
FIG2_LEGEND_KEYS <- c("west", "east")

fig_2_plot <- function(d) {
  d$series_label <- unname(REGION_LABELS[d$region])

  ggplot(d, aes(x = year, y = value, colour = series_label, group = region)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_line_interactive(linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = region,
        tooltip = sprintf("%d — %s\n%+.1f days vs. that half's 1895-2023 average", year, series_label, value)
      ),
      size = 1.4
    ) +
    scale_colour_manual(
      values = label_colours(d, "region", "series_label", FIG2_COLOUR_KEYS),
      breaks = label_order(d, "region", "series_label", FIG2_LEGEND_KEYS)
    ) +
    scale_x_continuous(breaks = seq(1900, 2020, 20)) +
    labs(x = NULL, y = "Days shorter or longer than average") +
    theme_indicator() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d))

fig_2_table <- function(d) {
  d$series_label <- unname(REGION_LABELS[d$region])
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}

# ---- Figure 3, 5, 6: change by state --------------------------------------------
#
# EPA publishes these three as choropleth maps. They are drawn here as
# horizontal diverging bars, one per state, which carries the same numbers with
# the ranking made explicit and needs no mapping dependency. Bars run from an
# explicit zero rule rather than leaving the reader to find x = 0 on the axis,
# and are coloured by which side of it they fall on so the direction of a
# state's change reads before the axis does.
SIGN_KEYS <- c("negative", "positive")

fig_state_plot <- function(d, x_label) {
  d <- d[order(d$value, decreasing = TRUE), ]
  d$state <- factor(d$state, levels = rev(d$state))
  d$sign_key <- ifelse(d$value < BASELINE, "negative", "positive")

  ggplot(d, aes(y = state, x = value, fill = sign_key)) +
    geom_vline(xintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_col_interactive(
      aes(
        data_id = state,
        tooltip = sprintf("%s\n%+.2f days", state, value)
      ),
      width = 0.72
    ) +
    scale_fill_manual(values = series_colours(SIGN_KEYS)) +
    scale_x_continuous(expand = expansion(mult = 0.06), position = "top") +
    labs(x = x_label, y = NULL) +
    theme_indicator() +
    theme(
      # theme_indicator() is built for a vertical chart: gridlines running
      # across the categories and a baseline under them. Both flip here.
      panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      axis.line.x        = element_blank(),
      axis.text.y        = element_text(size = rel(0.7)),
      axis.ticks.length  = unit(0, "pt")
    )
}

fig_state_table <- function(d, value_label) {
  d <- d[order(d$value, decreasing = TRUE), ]
  out <- data.frame(
    State = d$state,
    sprintf("%+.2f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(out)[2] <- value_label
  out
}

fig_3_plot <- function(d) fig_state_plot(d, "Change in length of growing season, 1895 to 2023 (days)")
fig_3 <- function(d) girafe_indicator(fig_3_plot(d), height = 10)
fig_3_table <- function(d) fig_state_table(d, "Change in growing season (days)")

fig_5_plot <- function(d) fig_state_plot(d, "Change in timing of last spring frost, 1895 to 2023 (days)")
fig_5 <- function(d) girafe_indicator(fig_5_plot(d), height = 10)
fig_5_table <- function(d) fig_state_table(d, "Change in last spring frost (days)")

fig_6_plot <- function(d) fig_state_plot(d, "Change in timing of first fall frost, 1895 to 2023 (days)")
fig_6 <- function(d) girafe_indicator(fig_6_plot(d), height = 10)
fig_6_table <- function(d) fig_state_table(d, "Change in first fall frost (days)")

# ---- Figure 4: national frost timing -------------------------------------------
#
# The two frost series are peers, read in the order the season runs, so they
# take the two colour roles a chart reads first rather than a base/focus split.
FIG4_KEYS <- c("last_spring_frost", "first_fall_frost")

fig_4_plot <- function(d) {
  d$series_label <- unname(EVENT_LABELS[d$event])

  ggplot(d, aes(x = year, y = value, colour = series_label, group = event)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_line_interactive(linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = event,
        tooltip = sprintf("%d — %s\n%+.1f days vs. the 1895-2023 average", year, series_label, value)
      ),
      size = 1.4
    ) +
    scale_colour_manual(
      values = label_colours(d, "event", "series_label", FIG4_KEYS),
      breaks = label_order(d, "event", "series_label", FIG4_KEYS)
    ) +
    scale_x_continuous(breaks = seq(1900, 2020, 20)) +
    labs(x = NULL, y = "Days later or earlier than average") +
    theme_indicator() +
    legend_top()
}

fig_4 <- function(d) girafe_indicator(fig_4_plot(d))

fig_4_table <- function(d) {
  d$series_label <- unname(EVENT_LABELS[d$event])
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}
