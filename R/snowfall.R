# Figures for indicators/snow-and-ice/snowfall.qmd.

REPO <- "snowfall"

# Upstream stores latitude and longitude as text so the source file's precision
# survives byte for byte. read_indicator() only coerces `value`; the
# coordinates become numeric here, once.
station_points <- function(d) {
  d$lat <- as.numeric(d$latitude)
  d$lon <- as.numeric(d$longitude)
  d
}

# Bundled geometry shipped with the maps package and not fetched over the
# network, so it is built here rather than threaded in from the setup chunk.
conus_states <- function() {
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
}

# NAD83 / Conus Albers (EPSG:5070), the same equal-area projection the Snowpack
# maps use.
MAP_CRS <- 5070

# EPA's captions colour an increase blue and a decrease red on both maps.
DIRECTION_COLOURS <- c(increase = INDICATOR_PALETTE[["base"]], decrease = INDICATOR_PALETTE[["focus"]])

prep <- function(d) {
  d <- station_points(d)
  d$direction <- factor(ifelse(d$value >= 0, "increase", "decrease"),
                        levels = c("increase", "decrease"))
  d$rel_mag <- abs(d$value) / max(abs(d$value))
  d
}

coords_label <- function(d) sprintf("%.2f°N, %.2f°W", d$lat, abs(d$lon))

basemap <- function() {
  geom_sf(
    data = conus_states(), fill = CHART_GREY[["grid"]],
    colour = CHART_GREY[["rule"]], linewidth = 0.3
  )
}

# Table coordinates print the upstream text, so no digit is lost or added.
lat_label <- function(d) sprintf("%s°N", d$latitude)
lon_label <- function(d) sprintf("%s°W", sub("^-", "", d$longitude))

# ---- Figure 1: total snowfall trend, 1930-2007 --------------------------------

FIG1_LABELS <- c(increase = "Snowfall increased", decrease = "Snowfall decreased")

fig_1_plot <- function(d) {
  d <- prep(d)

  ggplot(d) +
    basemap() +
    geom_point_interactive(
      aes(
        x = lon, y = lat, fill = direction, size = sqrt(rel_mag),
        data_id = paste(latitude, longitude, sep = "/"),
        tooltip = sprintf("%s\n%s, 1930–2007\n%.2f%% per year",
                          coords_label(d), FIG1_LABELS[as.character(direction)], value)
      ),
      shape = 21, colour = CHART_GREY[["surface"]], stroke = 0.3, alpha = 0.9
    ) +
    scale_fill_manual(values = DIRECTION_COLOURS, labels = FIG1_LABELS) +
    scale_size(range = c(0.6, 3.6), guide = "none") +
    # default_crs tells coord_sf() that geom_point's raw longitude/latitude
    # columns are unprojected WGS84, so basemap and points project together.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(size = 3.0, alpha = 1))) +
    theme_void() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 5.4)

# Sorted by value ascending, largest decrease first.
fig_1_table <- function(d) {
  d <- d[order(d$value), ]
  data.frame(
    Latitude  = lat_label(d),
    Longitude = lon_label(d),
    `Change (percent per year)` = d$value,
    check.names = FALSE
  )
}

# ---- Figure 2: snow-to-precipitation ratio change, 1949-2024 ------------------

FIG2_LABELS <- c(increase = "Ratio increased", decrease = "Ratio decreased")

# EPA's caption: solid-color circles are statistically significant trends, so
# the rest draw as open rings in the same direction colour.
SIGNIFICANCE_SHAPES <- c(significant = 16L, `not significant` = 1L)

fig_2_plot <- function(d) {
  d <- prep(d)
  d$significance <- factor(ifelse(d$significant == "1", "significant", "not significant"),
                           levels = names(SIGNIFICANCE_SHAPES))

  ggplot(d) +
    basemap() +
    geom_point_interactive(
      aes(
        x = lon, y = lat, colour = direction, shape = significance, size = sqrt(rel_mag),
        data_id = paste(latitude, longitude, sep = "/"),
        tooltip = sprintf("%s\n%s\n%s, 1949–2024\n%.2f%% (%s)",
                          state, coords_label(d), FIG2_LABELS[as.character(direction)],
                          value, significance)
      ),
      stroke = 0.9
    ) +
    scale_colour_manual(values = DIRECTION_COLOURS, labels = FIG2_LABELS) +
    scale_shape_manual(
      values = SIGNIFICANCE_SHAPES,
      labels = c(significant = "Statistically significant", `not significant` = "Not significant")
    ) +
    scale_size(range = c(0.8, 4.2), guide = "none") +
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    guides(
      colour = guide_legend(nrow = 1, order = 1, override.aes = list(size = 3.0, shape = 16L)),
      shape  = guide_legend(nrow = 1, order = 2,
                            override.aes = list(size = 3.0, colour = CHART_GREY[["text"]]))
    ) +
    theme_void() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 5.6)

# Sorted by value ascending, largest decrease first.
fig_2_table <- function(d) {
  d <- d[order(d$value), ]
  data.frame(
    State     = d$state,
    Latitude  = lat_label(d),
    Longitude = lon_label(d),
    `Change (percent)`          = d$value,
    `Statistically significant` = ifelse(d$significant == "1", "Yes", "No"),
    check.names = FALSE
  )
}
