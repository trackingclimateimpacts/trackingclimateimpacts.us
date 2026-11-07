# Figures for indicators/snow-and-ice/ice-sheets.qmd.

REPO <- "ice-sheets"

# ---- Figure 1: cumulative mass balance of Greenland and Antarctica, 1992-2023 ----

# IMBIE's combined estimate is the series the figure is about; NASA JPL is the
# single-analysis reference line drawn against it.
FIG1_KEYS    <- c("IMBIE", "NASA")
FIG1_LABELS  <- c(IMBIE = "Combined estimate (IMBIE)", NASA = "NASA JPL")
FIG1_REGIONS <- c("Greenland", "Antarctica")

# Upstream stores the decimal year as text; it is only ever plotted or printed.
fig_1_data <- function(d) {
  d$decimal_year <- as.numeric(d$decimal_year)
  d$region <- factor(d$region, levels = FIG1_REGIONS)
  d
}

# NASA's record has no months between the GRACE and GRACE-FO missions
# (mid-2017 to mid-2018). A new segment starts after any jump of more than half
# a year, so the line breaks there instead of bridging the gap.
nasa_segments <- function(n) {
  n <- n[order(n$region, n$decimal_year), ]
  jump <- ave(n$decimal_year, n$region, FUN = function(x) c(0, diff(x)) > 0.5)
  n$segment <- paste(n$region, ave(jump, n$region, FUN = cumsum))
  n
}

fig_1_plot <- function(d) {
  d <- fig_1_data(d)
  imbie <- tidyr::pivot_wider(
    d[d$source == "IMBIE", ],
    id_cols = c(decimal_year, region), names_from = measure, values_from = value
  )
  imbie$source <- "IMBIE"
  nasa <- nasa_segments(d[d$source == "NASA" & d$measure == "mass", ])
  cols <- series_colours(FIG1_KEYS)

  ggplot(mapping = aes(x = decimal_year, colour = source)) +
    geom_hline(yintercept = 0, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_ribbon(
      data = imbie,
      aes(ymin = mass - uncertainty, ymax = mass + uncertainty),
      fill = cols[["IMBIE"]], colour = NA, alpha = 0.25
    ) +
    geom_line(data = nasa, aes(y = value, group = segment), linewidth = 0.4) +
    geom_line(data = imbie, aes(y = mass), linewidth = 0.8) +
    geom_point_interactive(
      data = nasa,
      aes(
        y = value, data_id = paste(region, source, decimal_year),
        tooltip = sprintf("%s, NASA JPL\n%.2f\n%s billion metric tons",
                          region, decimal_year, format(round(value), big.mark = ","))
      ),
      size = 0.5
    ) +
    geom_point_interactive(
      data = imbie,
      aes(
        y = mass, data_id = paste(region, source, decimal_year),
        tooltip = sprintf("%s, combined estimate\n%.2f\n%s ± %s billion metric tons",
                          region, decimal_year, format(round(mass), big.mark = ","),
                          format(round(uncertainty), big.mark = ","))
      ),
      size = 0.3, alpha = 0
    ) +
    facet_wrap(~region, ncol = 2) +
    scale_colour_manual(values = cols, labels = FIG1_LABELS, breaks = FIG1_KEYS) +
    scale_x_continuous(breaks = seq(1995, 2020, 5)) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = NULL, y = "Cumulative mass change since 2002\n(billion metric tons)") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

# One row per time step, as in EPA's download; a series with no estimate at
# that time is left blank.
fig_1_table <- function(d) {
  d <- fig_1_data(d)
  d$key <- paste(d$source, d$region, d$measure)
  w <- tidyr::pivot_wider(d, id_cols = decimal_year, names_from = key, values_from = value)
  w <- w[order(w$decimal_year), ]
  fmt <- function(x) ifelse(is.na(x), "", format(round(x), big.mark = ",", trim = TRUE))
  data.frame(
    Year                               = sprintf("%.2f", w$decimal_year),
    `Greenland, combined`              = fmt(w$`IMBIE Greenland mass`),
    `Greenland, combined uncertainty`  = fmt(w$`IMBIE Greenland uncertainty`),
    `Greenland, NASA JPL`              = fmt(w$`NASA Greenland mass`),
    `Antarctica, combined`             = fmt(w$`IMBIE Antarctica mass`),
    `Antarctica, combined uncertainty` = fmt(w$`IMBIE Antarctica uncertainty`),
    `Antarctica, NASA JPL`             = fmt(w$`NASA Antarctica mass`),
    check.names = FALSE
  )
}
