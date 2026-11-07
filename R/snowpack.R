# Figures for indicators/snow-and-ice/snowpack.qmd.

REPO <- "snowpack"

# Upstream stores latitude, longitude, and value as text so the source file's
# precision survives byte for byte. read_indicator() only coerces `value`;
# latitude and longitude become numeric here, once.
station_points <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d
}

fmt <- function(x) sprintf("%.2f", x)

# The 12 states with monitoring sites in this indicator (South Dakota for a
# single Black Hills site), drawn on their own because every site is in the
# West and a full CONUS basemap would leave two thirds of the map empty.
WESTERN_STATES <- c(
  "washington", "oregon", "california", "idaho", "nevada", "montana",
  "wyoming", "utah", "colorado", "arizona", "new mexico", "south dakota"
)

# Bundled geometry shipped with the maps package and not fetched over the
# network, so it is built here rather than threaded in from the setup chunk.
western_states <- function() {
  sf::st_as_sf(maps::map("state", regions = WESTERN_STATES, plot = FALSE, fill = TRUE))
}

# NAD83 / Conus Albers (EPSG:5070), the same equal-area projection the
# Streamflow maps use.
MAP_CRS <- 5070

# EPA's captions colour an increase blue and a decrease red on all three maps.
DIRECTION_COLOURS <- c(increase = INDICATOR_PALETTE[["base"]], decrease = INDICATOR_PALETTE[["focus"]])

prep <- function(d) {
  d <- station_points(d)
  d$direction <- factor(ifelse(d$value >= 0, "increase", "decrease"),
                        levels = c("increase", "decrease"))
  d$rel_mag <- abs(d$value) / max(abs(d$value))
  d
}

station_tooltip <- function(d, unit_label, labels, coverage) {
  sprintf(
    "%s\n%.4f°N, %.4f°W\n%s, %s\n%s %s",
    d$state, d$latitude, abs(d$longitude),
    unname(labels[as.character(d$direction)]), coverage,
    fmt(d$value), unit_label
  )
}

# `shapes` is named by direction: Figures 1 and 4 use circles for both, Figure 2
# up and down triangles, matching EPA's captions.
station_map_plot <- function(d, unit_label, labels, coverage, shapes) {
  d <- prep(d)

  ggplot(d) +
    geom_sf(
      data = western_states(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = direction, shape = direction,
        size = sqrt(rel_mag),
        data_id = paste(round(longitude, 4), round(latitude, 4), sep = "/"),
        tooltip = station_tooltip(d, unit_label, labels, coverage)
      ),
      colour = CHART_GREY[["surface"]], stroke = 0.3, alpha = 0.9
    ) +
    scale_fill_manual(values = DIRECTION_COLOURS, labels = labels) +
    scale_shape_manual(values = shapes, labels = labels) +
    scale_size(range = c(0.8, 4.2), guide = "none") +
    # default_crs tells coord_sf() that geom_point's raw longitude/latitude
    # columns are unprojected WGS84, so basemap and points project together.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    guides(
      fill  = guide_legend(nrow = 1, override.aes = list(size = 3.0, alpha = 1)),
      shape = guide_legend(nrow = 1)
    ) +
    theme_void() +
    legend_top()
}

# Sorted by value ascending, most negative change first.
station_map_table <- function(d, unit_label) {
  d <- station_points(d)
  d <- d[order(d$value), ]
  data.frame(
    State     = d$state,
    Latitude  = sprintf("%.5f°N", d$latitude),
    Longitude = sprintf("%.5f°W", abs(d$longitude)),
    Value     = fmt(d$value),
    Unit      = unit_label,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

CIRCLES   <- c(increase = 21L, decrease = 21L)
TRIANGLES <- c(increase = 24L, decrease = 25L) # filled triangle up / down

# ---- Figure 1: April snowpack trend ------------------------------------------

APRIL_LABELS <- c(increase = "Snowpack increased", decrease = "Snowpack decreased")

fig_1_plot  <- function(d) station_map_plot(d, "percent change", APRIL_LABELS, "1955-2023", CIRCLES)
fig_1       <- function(d) girafe_indicator(fig_1_plot(d), height = 6.0)
fig_1_table <- function(d) station_map_table(d, "percent change")

# ---- Figure 2: peak snowpack timing change -----------------------------------

TIMING_LABELS <- c(increase = "Later peak", decrease = "Earlier peak")

fig_2_plot  <- function(d) station_map_plot(d, "days", TIMING_LABELS, "1982-2023", TRIANGLES)
fig_2       <- function(d) girafe_indicator(fig_2_plot(d), height = 6.0)
fig_2_table <- function(d) station_map_table(d, "days")

# ---- Figure 3: average date of peak snowpack ---------------------------------

fig_3_plot <- function(d) {
  d$water_year <- as.integer(d$water_year)
  colour <- INDICATOR_PALETTE[["base"]]

  ggplot(d, aes(x = water_year, y = value)) +
    geom_line(colour = colour, linewidth = 0.7) +
    geom_point_interactive(
      aes(data_id = water_year,
          tooltip = sprintf("Water year %d\nPeak snowpack: day %.1f of the water year", water_year, value)),
      colour = colour, size = 1.2
    ) +
    scale_x_continuous(breaks = seq(1985, 2020, 5)) +
    labs(x = NULL, y = "Day of water year") +
    theme_indicator()
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d))

fig_3_table <- function(d) {
  data.frame(
    `Water year`                 = as.integer(d$water_year),
    `Peak date (day of water year)` = sprintf("%.1f", d$value),
    check.names = FALSE
  )
}

# ---- Figure 4: snowpack season length change ---------------------------------

SEASON_LABELS <- c(increase = "Longer season", decrease = "Shorter season")

fig_4_plot  <- function(d) station_map_plot(d, "days", SEASON_LABELS, "1982-2023", CIRCLES)
fig_4       <- function(d) girafe_indicator(fig_4_plot(d), height = 6.0)
fig_4_table <- function(d) station_map_table(d, "days")
