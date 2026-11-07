# Figures for indicators/ecosystems/cherry-blossom-bloom.qmd.

REPO <- "cherry-blossom-bloom"

FIG1_LABELS <- c(
  festival   = "National Cherry Blossom Festival",
  peak_bloom = "Peak bloom date"
)

# The festival band is the backdrop the bloom dates are read against, so it
# takes `base` and the peak bloom dates take `focus`.
FIG1_KEYS <- c("festival", "peak_bloom")

# EPA's own axis labelling: the source counts days from January 1, and EPA's
# chart prints calendar dates at every tenth day. Kept as published rather than
# recomputed here, because the day-to-date mapping shifts by one in leap years.
FIG1_DATE_LABELS <- c(
  "70" = "March 10", "80" = "March 21", "90" = "March 31",
  "100" = "April 10", "110" = "April 20"
)

# One row per year, with the festival columns NA before 1934 and in 1942-1946,
# when it was not held. NA is what makes the band stop instead of bridging.
fig_1_wide <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = series_key, values_from = value)
}

fig_1_plot <- function(d) {
  w <- fig_1_wide(d)
  w$festival_end <- w$festival_start + w$festival_duration
  w$peak_label     <- FIG1_LABELS[["peak_bloom"]]
  w$festival_label <- FIG1_LABELS[["festival"]]
  w$tip <- sprintf(
    "%d\nPeak bloom: day %d",
    w$year, as.integer(w$peak_bloom)
  )
  held <- !is.na(w$festival_start)
  w$tip[held] <- sprintf(
    "%s\nFestival: starts day %d, runs %d days",
    w$tip[held], as.integer(w$festival_start[held]), as.integer(w$festival_duration[held])
  )

  legend <- data.frame(key = FIG1_KEYS, label = unname(FIG1_LABELS[FIG1_KEYS]))
  cols   <- label_colours(legend, "key", "label", FIG1_KEYS)
  breaks <- label_order(legend, "key", "label", FIG1_KEYS)

  ggplot(w, aes(x = year)) +
    geom_ribbon(
      aes(ymin = festival_start, ymax = festival_end, fill = festival_label),
      alpha = 0.3
    ) +
    geom_line(aes(y = peak_bloom, colour = peak_label), linewidth = 0.5) +
    geom_point_interactive(
      aes(y = peak_bloom, colour = peak_label, data_id = year, tooltip = tip),
      size = 1.4
    ) +
    scale_fill_manual(values = cols, breaks = breaks, name = NULL) +
    scale_colour_manual(values = cols, breaks = breaks, name = NULL) +
    # Reversed so an earlier bloom sits higher, as on EPA's chart.
    scale_y_reverse(
      limits = c(120, 70),
      breaks = as.numeric(names(FIG1_DATE_LABELS)),
      labels = unname(FIG1_DATE_LABELS)
    ) +
    scale_x_continuous(breaks = seq(1920, 2020, 20)) +
    labs(x = NULL, y = "Date (days from January 1)") +
    theme_indicator() +
    legend_top()
}

fig_1       <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  w <- fig_1_wide(d)
  data.frame(
    Year = w$year,
    `Yoshino peak bloom date (days from January 1)` = w$peak_bloom,
    `Festival start date (days from January 1)`     = w$festival_start,
    `Festival duration (days)`                      = w$festival_duration,
    check.names = FALSE
  )
}
