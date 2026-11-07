# Figures for indicators/ecosystems/growing-degree-days.qmd.

REPO <- "growing-degree-days"

# Upstream holds latitude and longitude as text so the source file's precision
# survives byte for byte, and read_indicator() only coerces `value`, `year` and
# `date`. They become numeric here, once, for the chart and the table.
station_points <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d
}

# CONUS state polygons for the Figure 1 basemap. Not indicator data — bundled
# geometry shipped with the maps package, not fetched over the network — so it
# is built here rather than threaded in from the page's setup chunk.
us_states <- function() {
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
}

# NAD83 / Conus Albers (EPSG:5070): the standard equal-area projection for
# CONUS-wide maps (USGS, Census), so state shapes and station spacing read
# correctly instead of the straight-line distortion of unprojected lon/lat.
MAP_CRS <- 5070

# EPA's caption for this figure says a station's percent change reads through
# both colour and size, so both encode `value` directly rather than a binned
# legend class. The colour ramp ends at the two named roles a chart already
# reads its increases and decreases through elsewhere, meeting at CHART_GREY's
# rule colour on zero, so a station with no change sits on the same grey as
# every reference line on the site.
fig_1_plot <- function(d) {
  d <- station_points(d)

  ggplot(d) +
    geom_sf(
      data = us_states(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, colour = value, size = abs(value),
        data_id = seq_len(nrow(d)),
        tooltip = sprintf(
          "%.4f°N, %.4f°W\n%+.1f%% change, 1948 to 2023",
          latitude, abs(longitude), value
        )
      ),
      alpha = 0.85
    ) +
    scale_colour_gradient2(
      low = INDICATOR_PALETTE[["base"]], mid = CHART_GREY[["rule"]],
      high = INDICATOR_PALETTE[["focus"]], midpoint = 0,
      name = "Percent change,\n1948 to 2023"
    ) +
    scale_size(range = c(1.2, 5.5), guide = "none") +
    # default_crs tells coord_sf() that geom_point's raw longitude/latitude
    # columns are unprojected WGS84, so both the sf basemap and the station
    # points get projected into MAP_CRS together.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    theme_void() +
    theme(
      legend.position   = "top",
      legend.key.width  = unit(3, "lines"),
      legend.title      = element_text(colour = CHART_GREY[["text"]], size = rel(0.85)),
      legend.text       = element_text(colour = CHART_GREY[["text"]], size = rel(0.8))
    )
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 4.6)

# Sorted from the largest increase down, so the fifty stations EPA singles out
# as having gained 20 percent or more are the first thing the table shows.
fig_1_table <- function(d) {
  d <- station_points(d)
  d <- d[order(d$value, decreasing = TRUE), ]
  data.frame(
    Latitude         = sprintf("%.4f°N", d$latitude),
    Longitude        = sprintf("%.4f°W", abs(d$longitude)),
    "Percent change" = sprintf("%+.2f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
