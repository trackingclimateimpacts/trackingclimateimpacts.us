# Figures for indicators/weather-and-extreme-events/high-and-low-temperatures.qmd.

REPO <- "high-and-low-temperatures"

fmt <- function(x, digits = 1) sprintf(paste0("%.", digits, "f"), x)

# ---- Figures 1 and 2: area of the contiguous 48 states with unusually hot/cold temperatures ----

# Both figures carry the same four-series shape: a raw annual value and its
# 9-point binomial smooth, for a "highs" series and a "lows" series. Role and
# smoothing are read off the series label itself so one function draws both
# figures; EPA phrases smoothing as "(smoothed)" on Figure 1 and "9-pt" on
# Figure 2, and phrases the role as "highs"/"lows" (case varies) on both.
AREA_ROLE_KEYS <- c("high", "low")

area_role     <- function(series) ifelse(grepl("low", series, ignore.case = TRUE), "low", "high")
area_smoothed <- function(series) grepl("smoothed|9-pt", series, ignore.case = TRUE)

area_plot <- function(d, y_lab) {
  d$role      <- factor(area_role(d$series), levels = AREA_ROLE_KEYS, labels = c("Highs", "Lows"))
  d$smoothing <- ifelse(area_smoothed(d$series), "smoothed", "raw")
  d$tooltip   <- sprintf("%s\n%d\n%s: %s", d$series, d$year, y_lab, scales::percent(d$value, accuracy = 0.1))

  colours <- stats::setNames(unname(series_colours(AREA_ROLE_KEYS)), c("Highs", "Lows"))

  ggplot(d, aes(x = year, y = value, colour = role, linewidth = smoothing, group = series)) +
    geom_line_interactive(aes(data_id = series, tooltip = tooltip)) +
    scale_colour_manual(values = colours) +
    # Width is what actually carries "raw vs. nine-year average" (EPA draws
    # the smoothed line thick and the annual line thin); a linewidth legend
    # would just repeat what the caption text already says.
    scale_linewidth_manual(values = c(raw = 0.45, smoothed = 1.3), guide = "none") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    labs(x = NULL, y = y_lab) +
    theme_indicator() +
    legend_top()
}

area_table <- function(d) {
  w <- tidyr::pivot_wider(d[c("year", "series", "value")], names_from = series, values_from = value)
  w <- w[order(w$year), ]
  out <- data.frame(Year = w$year, check.names = FALSE)
  for (nm in setdiff(names(w), "year")) out[[nm]] <- scales::label_percent(accuracy = 0.1)(w[[nm]])
  out
}

fig_1_plot  <- function(d) area_plot(d, "Land area with unusually hot summer temperatures")
fig_1       <- function(d) girafe_indicator(fig_1_plot(d))
fig_1_table <- area_table

fig_2_plot  <- function(d) area_plot(d, "Land area with unusually cold winter temperatures")
fig_2       <- function(d) girafe_indicator(fig_2_plot(d))
fig_2_table <- area_table

# ---- Figures 3 and 4: change in unusually hot/cold days, by station --------

# CONUS state polygons for the basemap. Not indicator data, but bundled
# geometry shipped with the maps package and not fetched over the network, so
# it is built here rather than threaded in from the page's setup chunk.
us_states <- function() {
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
}

# NAD83 / Conus Albers (EPSG:5070): the standard equal-area projection for
# CONUS-wide maps, so state shapes and station spacing read correctly instead
# of the straight-line distortion of unprojected lon/lat. Every station in
# both figures is in the contiguous 48 states, so there is no off-map strip.
MAP_CRS <- 5070

TREND_LEVELS <- c("increase", "decrease", "none", "unknown")
TREND_SHAPES <- c(increase = 24L, decrease = 25L, none = 21L, unknown = 4L)

# `unknown` is not part of EPA's own classification: it is the 18 Figure-3
# stations upstream could not match to a p-value at all (13 outside the
# regression's smaller network, 5 more with an ambiguous coordinate in EPA's
# own published file — see that repository's data-raw/PROVENANCE.md). They
# still carry a real change-in-days value, so they stay on the map with a
# fourth, distinct marker rather than being silently dropped for lacking a
# significance figure.
trend_colours <- function(more_common) {
  stopifnot("more_common must be 'increase' or 'decrease'" = more_common %in% c("increase", "decrease"))
  less_common <- setdiff(c("increase", "decrease"), more_common)
  stats::setNames(
    c(INDICATOR_PALETTE[["focus"]], INDICATOR_PALETTE[["base"]], CHART_GREY[["rule"]], CHART_GREY[["annotation"]]),
    c(more_common, less_common, "none", "unknown")
  )[TREND_LEVELS]
}

prep_station <- function(d) {
  d$lat   <- as.numeric(d$lat)
  d$long  <- as.numeric(d$long)
  d$trend <- ifelse(!nzchar(d$trend), "unknown", d$trend)
  d$trend <- factor(d$trend, levels = TREND_LEVELS)
  m <- max(abs(d$value))
  d$rel_mag <- if (m == 0) rep(0, nrow(d)) else abs(d$value) / m
  d
}

station_tooltip <- function(d, unit_label, labels) {
  detail <- ifelse(
    d$trend == "unknown",
    "no significance value available",
    sprintf("%s (p = %s)", unname(labels[as.character(d$trend)]),
            ifelse(is.na(suppressWarnings(as.numeric(d$p_value))), "NA", fmt(as.numeric(d$p_value), 3)))
  )
  sprintf("%s\n%.4f°N, %.4f°W\n%+.2f %s\n%s",
          ifelse(nzchar(d$station), d$station, d$state), d$lat, abs(d$long), d$value, unit_label, detail)
}

# `warming` names which of the two directions ("increase"/"decrease" in
# `trend`) gets the warm colour: for Figure 3 that is "increase" (more
# unusually hot days), for Figure 4 it is "decrease" (fewer unusually cold
# days), matching EPA's own captions.
station_map_plot <- function(d, unit_label, labels, warming) {
  d <- prep_station(d)
  colours <- trend_colours(warming)

  ggplot(d) +
    geom_sf(
      data = us_states(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    geom_point_interactive(
      aes(
        x = long, y = lat, fill = trend, shape = trend, size = sqrt(rel_mag),
        data_id = ifelse(nzchar(station), station, paste(state, lat, long)),
        tooltip = station_tooltip(d, unit_label, labels)
      ),
      colour = CHART_GREY[["surface"]], stroke = 0.5, alpha = 0.9
    ) +
    scale_fill_manual(values = colours, labels = labels, drop = FALSE) +
    scale_shape_manual(values = TREND_SHAPES, labels = labels, drop = FALSE) +
    scale_size(range = c(1.0, 4.6), guide = "none") +
    # default_crs tells coord_sf() that geom_point's raw longitude/latitude
    # columns are unprojected WGS84, so both the sf basemap and the station
    # points get projected into MAP_CRS together.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    guides(
      fill  = guide_legend(nrow = 2, override.aes = list(size = 3.0, alpha = 1)),
      shape = guide_legend(nrow = 2)
    ) +
    theme_void() +
    legend_top()
}

FIG3_LABELS <- c(increase = "More unusually hot days", decrease = "Fewer unusually hot days",
                 none = "No significant trend", unknown = "Significance not available")
FIG4_LABELS <- c(increase = "More unusually cold days", decrease = "Fewer unusually cold days",
                 none = "No significant trend", unknown = "Significance not available")

fig_3_plot <- function(d) {
  station_map_plot(d, "days/year hotter than the 95th percentile", FIG3_LABELS, warming = "increase")
}
fig_4_plot <- function(d) {
  station_map_plot(d, "days/year colder than the 5th percentile", FIG4_LABELS, warming = "decrease")
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d), height = 4.6)
fig_4 <- function(d) girafe_indicator(fig_4_plot(d), height = 4.6)

station_table <- function(d, labels, change_col) {
  d <- prep_station(d)
  d <- d[order(d$value, decreasing = TRUE), ]
  out <- data.frame(
    Station   = ifelse(nzchar(d$station), d$station, NA_character_),
    State     = d$state,
    Latitude  = sprintf("%.4f°N", d$lat),
    Longitude = sprintf("%.4f°W", abs(d$long)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out[[change_col]] <- sprintf("%+.2f", d$value)
  out$Trend <- unname(labels[as.character(d$trend)])
  out
}

fig_3_table <- function(d) station_table(d, FIG3_LABELS, "Change in days per year")
fig_4_table <- function(d) station_table(d, FIG4_LABELS, "Change in days per year")

# ---- Figure 5: record daily highs and lows, by decade -----------------------

DECADE_LEVELS <- c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s")
FIG5_KEYS     <- c("High", "Low")

fig_5_plot <- function(d) {
  d$decade <- factor(d$decade, levels = DECADE_LEVELS)
  d$series <- factor(d$series, levels = FIG5_KEYS)
  d$tooltip <- sprintf("%s\n%s\n%s%% of daily records", d$decade, d$series, fmt(abs(d$value), 2))

  ggplot(d, aes(x = decade, y = value, fill = series)) +
    geom_hline(yintercept = 0, colour = CHART_GREY[["text"]], linewidth = 0.4) +
    geom_col_interactive(
      aes(data_id = paste(decade, series), tooltip = tooltip),
      width = 0.7
    ) +
    scale_fill_manual(
      values = stats::setNames(unname(series_colours(c("high", "low"))), FIG5_KEYS),
      breaks = FIG5_KEYS, labels = FIG5_KEYS
    ) +
    scale_y_continuous(
      limits = c(-100, 100), breaks = seq(-100, 100, 25),
      labels = function(x) scales::label_percent(scale = 1)(abs(x))
    ) +
    labs(x = NULL, y = "Share of daily temperature records") +
    theme_indicator() +
    legend_top() +
    # The zero line is the reference here, so the horizontal gridlines the
    # default theme suppresses on x are what the eye needs on y instead.
    theme(panel.grid.major.y = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4))
}

fig_5 <- function(d) girafe_indicator(fig_5_plot(d), height = 3.8)

fig_5_table <- function(d) {
  d$decade <- factor(d$decade, levels = DECADE_LEVELS)
  w <- tidyr::pivot_wider(d[c("decade", "series", "value")], names_from = series, values_from = value)
  w <- w[order(w$decade), ]
  out <- data.frame(Decade = as.character(w$decade), check.names = FALSE)
  for (nm in setdiff(names(w), "decade")) out[[nm]] <- sprintf("%.2f%%", w[[nm]])
  out
}
