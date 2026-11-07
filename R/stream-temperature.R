# Figures for indicators/freshwater-systems/stream-temperature.qmd.

REPO <- "stream-temperature"

DIRECTION_LABELS <- c(increase = "Temperature increased", decrease = "Temperature decreased")

# Upstream stores every column as text so the source precision survives byte
# for byte. read_indicator() only coerces `value`; the coordinates become
# numeric here, once. Only the total-change series is drawn: it is the
# 1960-2014 change the map caption describes.
site_points <- function(d) {
  d <- d[d$series == "total_change_f", , drop = FALSE]
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d$direction <- factor(ifelse(d$value >= 0, "increase", "decrease"),
                        levels = c("increase", "decrease"))
  d$rel_mag <- abs(d$value) / max(abs(d$value))
  d
}

# CONUS state polygons for the basemap, from the maps package (bundled, not
# fetched over the network).
us_states <- function() {
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
}

# NAD83 / Conus Albers (EPSG:5070), the equal-area projection for CONUS maps.
MAP_CRS <- 5070

# ---- Figure 1: change in stream water temperature, 1960-2014 -----------------

# Colours follow EPA's caption: red where temperature increased, blue where it
# decreased. The upstream data has no significance flag, so unlike EPA's map
# this one cannot distinguish filled (significant) from open circles.
fig_1_plot <- function(d) {
  d <- site_points(d)
  cols <- stats::setNames(unname(INDICATOR_PALETTE[c("focus", "base")]), names(DIRECTION_LABELS))

  ggplot(d) +
    geom_sf(
      data = us_states(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = direction, size = sqrt(rel_mag),
        data_id = paste(longitude, latitude, sep = "/"),
        tooltip = sprintf("%.3f°N, %.3f°W\n%s, 1960-2014\n%.2f °F",
                          latitude, abs(longitude),
                          unname(DIRECTION_LABELS[as.character(direction)]), value)
      ),
      shape = 21, colour = CHART_GREY[["surface"]], stroke = 0.4, alpha = 0.9
    ) +
    scale_fill_manual(values = cols, labels = DIRECTION_LABELS) +
    scale_size(range = c(1.2, 5), guide = "none") +
    # xlim/ylim are read in default_crs (lon/lat) and frame the Chesapeake Bay
    # region rather than the whole country.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA,
             xlim = c(-84, -74.5), ylim = c(36, 42.5)) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(size = 3.5, alpha = 1))) +
    theme_void() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 5.0)

# One row per site, both measures side by side, sorted by total change.
fig_1_table <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  trend <- d[d$series == "trend_f_per_year", ]
  total <- d[d$series == "total_change_f", ]
  out <- data.frame(
    Latitude  = sprintf("%.3f°N", total$latitude),
    Longitude = sprintf("%.3f°W", abs(total$longitude)),
    `Trend (°F per year)` = sprintf("%.4f", trend$value[match(paste(total$latitude, total$longitude),
                                                                paste(trend$latitude, trend$longitude))]),
    `Total change (°F)` = sprintf("%.4f", total$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out[order(total$value), ]
}
