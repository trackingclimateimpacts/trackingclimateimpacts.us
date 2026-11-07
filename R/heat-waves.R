# Figures for indicators/weather-and-extreme-events/heat-waves.qmd.

REPO <- "heat-waves"

# The four characteristics, in the order EPA presents them and the order
# their palette role is assigned: base, focus, compare, other. They are four
# separate measures rather than peers read against one another, so the
# palette order carries no ranking.
SERIES_KEYS <- c("frequency", "duration", "season", "intensity")

# Upstream keeps every value as text so the source file's precision survives
# byte for byte; this is where each figure turns what it needs into a number.
fmt <- function(x) sprintf("%.2f", x)

series_factor <- function(d) {
  factor(d$series_key, levels = SERIES_KEYS,
         labels = d$series_label[match(SERIES_KEYS, d$series_key)])
}

# ---- Figure 1: heat wave characteristics by decade, 1961-2023 ----------------

decade_factor <- function(d) {
  lv <- unique(d$decade[order(as.integer(d$decade_start))])
  factor(d$decade, levels = lv)
}

fig_1_plot <- function(d) {
  d$decade <- decade_factor(d)
  d$panel  <- series_factor(d)

  ggplot(d, aes(x = decade, y = value, fill = series_key)) +
    geom_col_interactive(
      aes(
        data_id = paste(series_key, decade, sep = "/"),
        tooltip = sprintf("%s, %s\n%s %s", series_label, decade, fmt(value), unit)
      ),
      width = 0.7
    ) +
    scale_fill_manual(values = series_colours(SERIES_KEYS), guide = "none") +
    # The four characteristics share no common scale (season length runs to
    # tens of days, intensity to a couple of degrees), so each panel gets its
    # own y axis.
    facet_wrap(~ panel, nrow = 2, scales = "free_y") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = NULL) +
    theme_indicator() +
    theme(panel.spacing.y = unit(1.1, "lines"))
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 4.6)

fig_1_table <- function(d) {
  d$decade <- decade_factor(d)
  w <- tidyr::pivot_wider(d, id_cols = decade, names_from = series_label, values_from = value)
  out <- data.frame(Decade = as.character(w$decade), check.names = FALSE)
  for (nm in setdiff(names(w), "decade")) out[[nm]] <- fmt(w[[nm]])
  out
}

# ---- Figure 2: change in heat wave characteristics by city, 1961-2023 -------

# CONUS state polygons for the basemap. Not indicator data, but bundled
# geometry shipped with the maps package and not fetched over the network, so
# it is built here rather than threaded in from the page's setup chunk.
us_states <- function() {
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
}

# NAD83 / Conus Albers (EPSG:5070): the standard equal-area projection for a
# CONUS-wide map, so state shapes and station spacing read correctly instead
# of the straight-line distortion of unprojected lon/lat.
MAP_CRS <- 5070

# Two of EPA's 50 cities sit outside the CONUS projection this map uses:
# Honolulu, HI and San Juan, PR. Rather than dropping them, they are drawn
# separately in fig_2_offmap() on the same colour and size scale.
OFF_MAP_STATES <- c("HI", "PR")

conus  <- function(d) d[!d$state %in% OFF_MAP_STATES, , drop = FALSE]
offmap <- function(d) d[ d$state %in% OFF_MAP_STATES, , drop = FALSE]

DIRECTION_LEVELS <- c("increase", "decrease")
DIRECTION_LABELS <- c(increase = "Increase", decrease = "Decrease")

# Every one of the four characteristics reads as "hotter" when it rises: more
# heat waves, longer ones, a longer season, a hotter one. So a single warm/cool
# colour pair works across all four panels.
direction_colours <- function() {
  stats::setNames(c(INDICATOR_PALETTE[["focus"]], INDICATOR_PALETTE[["base"]]), DIRECTION_LEVELS)
}

prep_2 <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d$panel     <- series_factor(d)
  d$direction <- factor(ifelse(d$value >= 0, "increase", "decrease"), levels = DIRECTION_LEVELS)
  # Magnitude is scaled within each characteristic, not across all four, so a
  # circle's size means "large for this characteristic" on every panel rather
  # than being dominated by whichever measure happens to have the widest
  # range (frequency's change reaches into the teens, intensity's barely
  # exceeds one degree). The unrounded value stays in the tooltip and table.
  d$rel_mag <- ave(abs(d$value), d$series_key, FUN = function(x) {
    m <- max(x)
    if (m == 0) rep(0, length(x)) else x / m
  })
  d
}

city_tooltip <- function(d) {
  sprintf(
    "%s, %s\n%s: %+.2f %s (%s)",
    d$city, d$state, d$series_label, d$value, d$unit,
    ifelse(d$direction == "increase", "increase", "decrease")
  )
}

fig_2_plot <- function(d) {
  d <- conus(prep_2(d))

  ggplot(d) +
    geom_sf(
      data = us_states(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = direction, size = sqrt(rel_mag),
        data_id = paste(station, series_key, sep = "/"), tooltip = city_tooltip(d)
      ),
      shape = 21, colour = CHART_GREY[["surface"]], stroke = 0.4, alpha = 0.9
    ) +
    scale_fill_manual(values = direction_colours(), labels = DIRECTION_LABELS) +
    scale_size(range = c(1.1, 5.2), guide = "none") +
    facet_wrap(~ panel, nrow = 2) +
    # default_crs tells coord_sf() that the point layer's raw longitude and
    # latitude are unprojected WGS84, so the sf basemap and the stations get
    # projected into MAP_CRS together.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(size = 3.2, alpha = 1))) +
    theme_void() +
    legend_top() +
    theme(strip.text = element_text(colour = CHART_GREY[["ink"]], face = "bold",
                                    hjust = 0, size = rel(0.95)))
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 5.2)

# Honolulu and San Juan, drawn as a plain strip on the same colour and size
# scale as the map above rather than a second, mostly-empty map: the point is
# that these two cities are part of the indicator, not where they sit.
fig_2_offmap_plot <- function(d) {
  d <- offmap(prep_2(d))
  d$place <- factor(paste0(d$city, ", ", d$state))

  ggplot(d, aes(x = panel, y = place)) +
    geom_point_interactive(
      aes(fill = direction, size = sqrt(rel_mag), data_id = paste(station, series_key, sep = "/"),
          tooltip = city_tooltip(d)),
      shape = 21, colour = CHART_GREY[["surface"]], stroke = 0.4, alpha = 0.9
    ) +
    scale_fill_manual(values = direction_colours(), guide = "none") +
    scale_size(range = c(1.1, 5.2), guide = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_indicator() +
    theme(panel.grid.major.y = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4))
}

fig_2_offmap <- function(d) girafe_indicator(fig_2_offmap_plot(d), height = 1.5)

# Sorted by the largest increase in frequency first, so the cities where heat
# waves became most common lead the table.
fig_2_table <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  w <- tidyr::pivot_wider(d, id_cols = c(city, state, latitude, longitude),
                          names_from = series_label, values_from = value)
  w <- w[order(w[["Frequency"]], decreasing = TRUE), ]
  out <- data.frame(
    City      = w$city,
    State     = w$state,
    Latitude  = sprintf("%.2f°N", w$latitude),
    Longitude = sprintf("%.2f°W", abs(w$longitude)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  for (nm in c("Frequency", "Duration", "Season length", "Intensity")) {
    out[[nm]] <- sprintf("%+.2f", w[[nm]])
  }
  out
}

# ---- Figure 3: U.S. Annual Heat Wave Index, 1895-2021 ------------------------

fig_3_plot <- function(d) {
  ggplot(d, aes(x = year, y = value)) +
    geom_col_interactive(
      aes(data_id = year, tooltip = sprintf("%d\nHeat Wave Index: %s", year, fmt(value))),
      fill = INDICATOR_PALETTE[["focus"]], width = 0.8
    ) +
    scale_x_continuous(breaks = seq(1900, 2020, 20)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "U.S. Annual Heat Wave Index") +
    theme_indicator()
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d))

fig_3_table <- function(d) {
  data.frame(Year = d$year, "Heat Wave Index" = fmt(d$value), check.names = FALSE)
}
