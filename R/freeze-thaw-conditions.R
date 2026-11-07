# Figures for indicators/snow-and-ice/freeze-thaw-conditions.qmd.

REPO <- "freeze-thaw-conditions"

# Both series are deviations from their own 1979-2023 average, so zero is the
# reference the reader reads every bar against.
BASELINE <- 0

# ---- Figures 1 and 2: unfrozen days, contiguous 48 states and Alaska ----------
#
# EPA draws each year as a bar above or below the average. Bars are coloured by
# which side of zero they fall on, so a year with more unfrozen days than
# average reads as such before the axis does.
SIGN_KEYS <- c("negative", "positive")

fig_unfrozen_plot <- function(d) {
  d$sign_key <- ifelse(d$value < BASELINE, "negative", "positive")

  ggplot(d, aes(x = year, y = value, fill = sign_key)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_col_interactive(
      aes(
        data_id = year,
        tooltip = sprintf("%d\n%+.1f days vs. the 1979-2023 average", year, value)
      ),
      width = 0.8
    ) +
    scale_fill_manual(values = series_colours(SIGN_KEYS), guide = "none") +
    scale_x_continuous(breaks = seq(1980, 2020, 10)) +
    labs(x = NULL, y = "Unfrozen days fewer or more than average") +
    theme_indicator()
}

fig_unfrozen_table <- function(d) {
  data.frame(
    Year = d$year,
    "Unfrozen days fewer or more than average" = sprintf("%+.2f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

fig_1_plot  <- function(d) fig_unfrozen_plot(d)
fig_1       <- function(d) girafe_indicator(fig_1_plot(d))
fig_1_table <- function(d) fig_unfrozen_table(d)

fig_2_plot  <- function(d) fig_unfrozen_plot(d)
fig_2       <- function(d) girafe_indicator(fig_2_plot(d))
fig_2_table <- function(d) fig_unfrozen_table(d)
