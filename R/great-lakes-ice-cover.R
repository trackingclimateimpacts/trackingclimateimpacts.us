# Figures for indicators/snow-and-ice/great-lakes-ice-cover.qmd.

REPO <- "great-lakes-ice-cover"

# Upstream keys are EPA's column headers; panels read in EPA's column order,
# which puts the five-lake "All Lakes" panel last (lower right, as EPA draws it).
LAKE_LABELS <- c(
  "Erie"      = "Lake Erie",
  "Huron"     = "Lake Huron",
  "Michigan"  = "Lake Michigan",
  "Ontario"   = "Lake Ontario",
  "Superior"  = "Lake Superior",
  "All Lakes" = "All Lakes"
)

# Six and five panels are more than the palette is meant to separate, so each
# lake gets its own panel and every panel draws in the same colour.
lake_ice_plot <- function(d, y_label, tooltip_fmt, y_limits) {
  d$lake <- factor(LAKE_LABELS[d$lake], levels = unname(LAKE_LABELS))
  colour <- INDICATOR_PALETTE[["base"]]

  ggplot(d, aes(x = year, y = value, group = lake)) +
    geom_line(colour = colour, linewidth = 0.6) +
    geom_point_interactive(
      aes(data_id = paste(lake, year),
          tooltip = sprintf(tooltip_fmt, year, lake, value)),
      colour = colour, size = 0.8
    ) +
    scale_x_continuous(breaks = seq(1975, 2015, 10)) +
    scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0.04))) +
    facet_wrap(~lake, ncol = 3) +
    labs(x = NULL, y = y_label) +
    theme_indicator(base_size = 12)
}

lake_ice_table <- function(d) {
  d$lake <- factor(LAKE_LABELS[d$lake], levels = unname(LAKE_LABELS))
  out <- tidyr::pivot_wider(d[order(d$year, d$lake), ], id_cols = year,
                            names_from = lake, values_from = value)
  names(out)[1] <- "Year"
  out
}

# ---- Figure 1: maximum ice-covered area --------------------------------------

fig_1_plot <- function(d) {
  lake_ice_plot(d, "Maximum percent ice cover",
                "%d, %s\nMaximum ice cover: %.1f%%", c(0, 100))
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 5.5)

fig_1_table <- function(d) lake_ice_table(d)

# ---- Figure 2: duration of ice cover -----------------------------------------

fig_2_plot <- function(d) {
  lake_ice_plot(d, "Days with more than 5% ice cover",
                "%d, %s\n%.0f days with more than 5%% ice cover", c(0, NA))
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 5.5)

fig_2_table <- function(d) lake_ice_table(d)
