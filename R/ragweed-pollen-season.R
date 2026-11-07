# Figures for indicators/ecosystems/ragweed-pollen-season.qmd.

REPO <- "ragweed-pollen-season"

# Upstream holds latitude and longitude as text so the source file's precision
# survives byte for byte, and read_indicator() only coerces `value`. They
# become numeric here, once, for the chart and the table.
station_points <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d
}

# A handful of EPA's own city names carry a trailing comma baked into the
# source text itself (e.g. "Austin/Georgetown,"); the indicator repository's
# data-raw/PROVENANCE.md documents it. The upstream CSV keeps that verbatim on
# purpose, so it is cleaned up here for display only, never touching the
# underlying data or the downloadable file.
display_city <- function(city) sub(",\\s*$", "", city)

# Nine of the eleven stations are US states, but Winnipeg and Saskatoon are
# Canadian provinces, so the basemap has to cover both countries and the map
# projection has to be defined for both, unlike growing-degree-days' CONUS-only
# EPSG:5070. `maps` ships no Canadian province layer, so this draws country
# outlines only, not state/province boundaries.
north_america <- function() {
  sf::st_as_sf(maps::map("world", region = c("USA", "Canada"), plot = FALSE, fill = TRUE))
}

# North America Albers Equal Area Conic (ESRI:102008): its usage area covers
# both the United States and all Canadian provinces, unlike a CONUS-only
# projection, which would badly distort the two Canadian stations.
MAP_CRS <- "ESRI:102008"

# EPA's own figure caption fixes the colour encoding directly: "Red circles
# represent a longer pollen season; the blue circle represents a shorter
# season." That is two categories keyed on the sign of `value`, not a legend
# read from meta.yml the way growing-degree-days' seven classes are.
DIRECTION_ROLE_ORDER   <- c("shorter", "longer")
# "Longer" is both the far more common outcome (10 of 11 stations) and the
# story EPA's Key Points lead with, so it reads first in the legend too.
DIRECTION_LEGEND_ORDER <- c("longer", "shorter")

fig_1_plot <- function(d) {
  d <- station_points(d)
  d$direction_key   <- ifelse(d$value > 0, "longer", "shorter")
  d$direction_label <- ifelse(
    d$direction_key == "longer", "Longer pollen season", "Shorter pollen season"
  )

  ggplot(d) +
    geom_sf(
      data = north_america(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    # Colour alone isn't enough: EPA's caption also says size carries
    # magnitude ("Larger circles indicate larger changes").
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = direction_label,
        size = sqrt(abs(value)), data_id = seq_len(nrow(d)),
        tooltip = sprintf(
          "%s, %s\n%+.1f days, 1995 to 2015 (%s)",
          display_city(city), state_province, value, tolower(direction_label)
        )
      ),
      shape = 21, colour = CHART_GREY[["surface"]], stroke = 0.6
    ) +
    scale_fill_manual(
      values = label_colours(d, "direction_key", "direction_label", DIRECTION_ROLE_ORDER),
      breaks = label_order(d, "direction_key", "direction_label", DIRECTION_LEGEND_ORDER)
    ) +
    scale_size(range = c(2.4, 7), guide = "none") +
    # default_crs tells coord_sf() that geom_point's raw longitude/latitude
    # columns are unprojected WGS84, so both the basemap and the station
    # points get projected into MAP_CRS together.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(size = 3.2))) +
    theme_void() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 4.6)

# Sorted from the largest increase down, so Kansas City (the station with the
# biggest change) is the first row and Austin/Georgetown (the one station with
# a shorter season) is the last.
fig_1_table <- function(d) {
  d <- station_points(d)
  d <- d[order(d$value, decreasing = TRUE), ]
  data.frame(
    City             = display_city(d$city),
    "State/Province" = d$state_province,
    Latitude         = sprintf("%.4f°N", d$latitude),
    Longitude        = sprintf("%.4f°W", abs(d$longitude)),
    "Change (days)"  = sprintf("%+.1f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
