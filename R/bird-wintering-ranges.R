# Figures for indicators/ecosystems/bird-wintering-ranges.qmd.

REPO <- "bird-wintering-ranges"

# Both figures are changes from 1966, which is 0 by construction, so zero is a
# real reference line.
BASELINE <- 0

# The line is the mean; the shaded band is the likely range around it. Upstream
# stores them as three `series` values.
band_plot <- function(d, ylab, what) {
  w <- tidyr::pivot_wider(d, id_cols = year, names_from = series, values_from = value)
  col <- INDICATOR_PALETTE[["focus"]]

  ggplot(w, aes(x = year)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower_confidence, ymax = upper_confidence),
                fill = col, alpha = 0.25, colour = NA) +
    geom_line(aes(y = mean), colour = col, linewidth = 0.7) +
    geom_point_interactive(
      aes(
        y = mean, data_id = year,
        tooltip = sprintf("%d\nAverage %s: %+.1f miles\nLikely range: %+.1f to %+.1f miles",
                          year, what, mean, lower_confidence, upper_confidence)
      ),
      colour = col, size = 1
    ) +
    scale_x_continuous(breaks = seq(1970, 2010, 10)) +
    labs(x = NULL, y = ylab) +
    theme_indicator()
}

band_table <- function(d) {
  d$series <- factor(d$series, levels = c("mean", "lower_confidence", "upper_confidence"),
                     labels = c("mean distance (miles)", "lower confidence (miles)",
                                "upper confidence (miles)"))
  tidyr::pivot_wider(d[order(d$series), ], id_cols = year, names_from = series, values_from = value)
}

# ---- Figure 1: latitude of the center of abundance, 1966-2013 ----------------

fig_1_plot <- function(d) {
  band_plot(d, "Average distance moved north (miles)", "distance north")
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- band_table

# ---- Figure 2: distance to coast of the center of abundance, 1966-2013 -------

fig_2_plot <- function(d) {
  band_plot(d, "Average distance moved inland from coast (miles)", "distance inland")
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d))

fig_2_table <- band_table
