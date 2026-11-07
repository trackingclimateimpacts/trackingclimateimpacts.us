# Figures for indicators/coastal-and-marine/coastal-flooding.qmd.

REPO <- "coastal-flooding"

# Upstream periods use a hyphen; EPA's captions print an en dash.
period_label <- function(x) sub("-", "–", x, fixed = TRUE)

# Ranks sites by their most recent value, ties broken by name, and returns the
# factor levels bottom to top so the highest site sits at the top of the chart.
site_levels <- function(d, latest) {
  r <- d[d$period == latest, ]
  r$site[order(r$value, r$site)]
}

# ---- Figure 1: flood days per year, 2014-2023 versus 1950-1959 ---------------

FIG1_PERIODS <- c("1950-1959", "2014-2023")

# EPA's caption, reproduced verbatim on this page, names the colours: orange for
# 1950-1959 and purple for 2014-2023. `focus` is the palette's orange and
# `other` its plum, so the chart and the caption say the same thing.
fig_1_colours <- function() {
  stats::setNames(
    c(INDICATOR_PALETTE[["focus"]], INDICATOR_PALETTE[["other"]]),
    FIG1_PERIODS
  )
}

# A dumbbell per site rather than EPA's map of small bar graphs: 42 sites, many
# clustered in the Mid-Atlantic, would overlap on a map at this size, and the
# Key Points compare each site's two decades.
fig_1_plot <- function(d) {
  stopifnot(setequal(unique(d$period), FIG1_PERIODS))
  d$site   <- factor(d$site, levels = site_levels(d, "2014-2023"))
  d$period <- factor(d$period, levels = FIG1_PERIODS)
  w <- tidyr::pivot_wider(d, id_cols = site, names_from = period, values_from = value)

  ggplot(d, aes(y = site)) +
    geom_segment(
      data = w, aes(x = `1950-1959`, xend = `2014-2023`, yend = site),
      colour = CHART_GREY[["rule"]], linewidth = 0.5
    ) +
    geom_point_interactive(
      aes(
        x = value, colour = period, data_id = site,
        tooltip = sprintf("%s, %s\n%.2f flood days per year",
                          site, period_label(period), value)
      ),
      size = 2.4
    ) +
    scale_colour_manual(values = fig_1_colours(), labels = period_label) +
    # A small left pad so sites at zero draw as whole dots, not halves.
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.015, 0.04))) +
    labs(x = "Average number of flood days per year", y = NULL) +
    theme_indicator() +
    theme(
      panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4),
      axis.text.y        = element_text(size = rel(0.75))
    ) +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 9.5)

# Sorted like the chart, highest 2014-2023 value first.
fig_1_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = c(site, latitude, longitude),
                          names_from = period, values_from = value)
  w <- w[match(rev(site_levels(d, "2014-2023")), w$site), ]
  data.frame(
    Site      = w$site,
    Latitude  = sprintf("%s°N", w$latitude),
    # Wake Island lies east of the antimeridian, so not every site is west.
    Longitude = ifelse(startsWith(w$longitude, "-"),
                       sprintf("%s°W", sub("^-", "", w$longitude)),
                       sprintf("%s°E", w$longitude)),
    `1950–1959 (days per year)` = w[["1950-1959"]],
    `2014–2023 (days per year)` = w[["2014-2023"]],
    check.names = FALSE
  )
}

# ---- Figure 2: flood events per year, four periods since 1950 ----------------

FIG2_PERIODS <- c("1950-1969", "1970-1989", "1990-2009", "2010-2023")

# Colour slots are assigned out of chronological order so the first and last
# periods take the same orange and plum as Figure 1's first and last decades;
# the legend still reads oldest to newest.
FIG2_COLOUR_KEYS <- c("1970-1989", "1950-1969", "1990-2009", "2010-2023")

fig_2_plot <- function(d) {
  stopifnot(setequal(unique(d$period), FIG2_PERIODS))
  d$site   <- factor(d$site, levels = site_levels(d, "2010-2023"))
  d$period <- factor(d$period, levels = FIG2_PERIODS)
  span <- do.call(rbind, lapply(split(d$value, d$site), range))
  span <- data.frame(site = factor(rownames(span), levels = levels(d$site)),
                     lo = span[, 1], hi = span[, 2])

  ggplot(d, aes(y = site)) +
    geom_segment(
      data = span, aes(x = lo, xend = hi, yend = site),
      colour = CHART_GREY[["grid"]], linewidth = 0.5
    ) +
    geom_point_interactive(
      aes(
        x = value, colour = period, data_id = site,
        tooltip = sprintf("%s, %s\n%.2f flood events per year",
                          site, period_label(period), value)
      ),
      size = 2.2
    ) +
    scale_colour_manual(
      values = series_colours(FIG2_COLOUR_KEYS), breaks = FIG2_PERIODS,
      labels = period_label
    ) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.015, 0.04))) +
    labs(x = "Average number of flood events per year", y = NULL) +
    theme_indicator() +
    theme(
      panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4),
      axis.text.y        = element_text(size = rel(0.75))
    ) +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 9.5)

# Sorted like the chart, highest 2010-2023 value first.
fig_2_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = site, names_from = period, values_from = value)
  w <- w[match(rev(site_levels(d, "2010-2023")), w$site), ]
  out <- data.frame(Site = w$site, check.names = FALSE)
  for (p in FIG2_PERIODS) {
    out[[paste0(period_label(p), " (events per year)")]] <- w[[p]]
  }
  out
}
