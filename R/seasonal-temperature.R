# Figures for indicators/weather-and-extreme-events/seasonal-temperature.qmd.

REPO <- "seasonal-temperature"

# Every value on this page is an anomaly or a change relative to a baseline, so
# zero is a real reference the reader has to be able to find, not just a spot
# on the axis. Each figure draws it explicitly.
BASELINE <- 0

SEASON_KEYS   <- c("winter", "spring", "summer", "fall")
SEASON_LABELS <- c("Winter", "Spring", "Summer", "Fall")

# EPA's own Key Points lead with winter ("increased by about 3 F", the largest
# and most-discussed change; see data/seasonal_temperature_total_change_by_season.csv),
# so winter takes the `focus` slot. Spring gets its own called-out number
# ("about 2 F") and takes `compare`, the peer read alongside it. Summer and
# fall are grouped together in the same sentence ("about 1.6 F") and take the
# two remaining roles. The legend itself still reads in calendar order.
FIG_COLOUR_KEYS <- c("summer", "winter", "spring", "fall")
FIG_LEGEND_KEYS <- SEASON_KEYS

# ---- Figure 1: average seasonal temperature anomaly, 1896-2023 ---------------

# *_plot() builds the plain ggplot object; fig_*() wraps it for the page. The
# split exists so a plot can be ggsave()'d for a static check without pulling
# in the htmlwidget machinery.
fig_1_plot <- function(d) {
  d$series_key <- tolower(d$season)

  ggplot(d, aes(x = year, y = value, colour = season, group = series_key)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_line_interactive(linewidth = 0.8) +
    geom_point_interactive(
      aes(
        data_id = series_key,
        tooltip = sprintf("%d — %s\n%+.2f°F anomaly", year, season, value)
      ),
      size = 0.9
    ) +
    scale_colour_manual(
      values = label_colours(d, "series_key", "season", FIG_COLOUR_KEYS),
      breaks = label_order(d, "series_key", "season", FIG_LEGEND_KEYS)
    ) +
    scale_x_continuous(breaks = seq(1900, 2020, 20)) +
    labs(x = NULL, y = "Temperature anomaly (°F, vs. 1901-2000 average)") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = season, values_from = value)
}

# ---- Figure 2: change in seasonal temperature by state, 1896-2023 ------------
#
# EPA publishes this as four side-by-side maps, one per season. Drawn here as
# four stacked panels of horizontal diverging bars, one panel per season,
# ranked and coloured the same way length-of-growing-season.R ranks and
# colours its own state-level figures (EPA choropleth in, ranked bars out) —
# see that file's Figures 3/5/6. What is new here is that this is one EPA
# figure with four panels rather than four separate EPA figures, so all four
# share one state ordering (fixed from winter's ranking, the season EPA's Key
# Points leads with) instead of each panel sorting itself independently: a
# reader can follow one state's row down through all four seasons.

fig_2_state_order <- function(d) {
  winter <- d[d$season == "Winter", ]
  winter$state[order(winter$value, decreasing = TRUE)]
}

SIGN_KEYS <- c("negative", "positive")

fig_2_plot <- function(d) {
  d$state <- factor(d$state, levels = rev(fig_2_state_order(d)))
  d$season <- factor(d$season, levels = SEASON_LABELS)
  d$sign_key <- ifelse(d$value < BASELINE, "negative", "positive")

  ggplot(d, aes(y = state, x = value, fill = sign_key)) +
    geom_vline(xintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_col_interactive(
      aes(
        data_id = paste(state, season),
        tooltip = sprintf("%s — %s\n%+.2f°F total change, 1896-2023", state, season, value)
      ),
      width = 0.72
    ) +
    scale_fill_manual(values = series_colours(SIGN_KEYS), guide = "none") +
    facet_wrap(~season, ncol = 1) +
    scale_x_continuous(expand = expansion(mult = 0.06), position = "top") +
    labs(x = "Total change in temperature, 1896–2023 (°F)", y = NULL) +
    theme_indicator() +
    theme(
      # theme_indicator() is built for a vertical chart: gridlines running
      # across the categories and a baseline under them. Both flip here, the
      # same override length-of-growing-season.R's fig_state_plot() uses.
      panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      axis.line.x        = element_blank(),
      axis.text.y        = element_text(size = rel(0.55)),
      axis.ticks.length  = unit(0, "pt")
    )
}

# Four stacked 48-state panels need much more vertical room than a single one;
# length-of-growing-season.R's single-panel state figures use height = 10.
fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 34)

fig_2_table <- function(d) {
  wide <- tidyr::pivot_wider(d, id_cols = state, names_from = season, values_from = value)
  wide <- wide[order(wide$state), ]
  data.frame(
    State  = wide$state,
    Winter = sprintf("%+.3f", wide$Winter),
    Spring = sprintf("%+.3f", wide$Spring),
    Summer = sprintf("%+.3f", wide$Summer),
    Fall   = sprintf("%+.3f", wide$Fall),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ---- Figure 3: total change in temperature by season, 1896-2023 --------------

fig_3_plot <- function(d) {
  d$series_key <- tolower(d$season)
  d$season <- factor(d$season, levels = SEASON_LABELS)

  ggplot(d, aes(x = season, y = value, fill = season)) +
    geom_col_interactive(
      aes(
        data_id = series_key,
        tooltip = sprintf("%s\n%+.2f°F total change, 1896-2023", season, value)
      ),
      width = 0.6
    ) +
    scale_fill_manual(
      values = label_colours(d, "series_key", "season", FIG_COLOUR_KEYS),
      guide  = "none"
    ) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Total change in temperature, 1896–2023 (°F)") +
    theme_indicator()
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d))

fig_3_table <- function(d) {
  data.frame(
    Season = d$season,
    "Total change (°F)" = sprintf("%+.3f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
