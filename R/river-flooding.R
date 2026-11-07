# Figures for indicators/weather-and-extreme-events/river-flooding.qmd.

REPO <- "river-flooding"

# Upstream stores latitude, longitude, and value as text so the source file's
# precision survives byte for byte. read_indicator() only coerces `value`;
# latitude and longitude become numeric here, once.
station_points <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d
}

fmt <- function(x) sprintf("%.3f", x)

COVERAGE <- "1965-2015"

# CONUS state polygons for the basemap. Not indicator data, but bundled geometry
# shipped with the maps package and not fetched over the network, so it is built
# here rather than threaded in from the page's setup chunk.
us_states <- function() {
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
}

# NAD83 / Conus Albers (EPSG:5070): the standard equal-area projection for
# CONUS-wide maps, so state shapes and station spacing read correctly instead of
# the straight-line distortion of unprojected lon/lat.
MAP_CRS <- 5070

# This indicator's reference-gauge network reaches outside the Conus Albers
# panel: Alaska, Hawaii, and Puerto Rico all have stations. There is no station
# name or state code upstream, only coordinates, so membership is decided by a
# coordinate box rather than by a lookup column.
is_offmap <- function(d) {
  (d$latitude > 50 & d$longitude < -125) |                        # Alaska
    (d$longitude < -152 & d$longitude > -161 & d$latitude < 23) | # Hawaii
    (d$longitude > -68 & d$latitude < 20)                         # Puerto Rico
}

offmap_region <- function(d) {
  ifelse(
    d$latitude > 50 & d$longitude < -125, "Alaska",
    ifelse(d$longitude < -152 & d$longitude > -161 & d$latitude < 23, "Hawaii", "Puerto Rico")
  )
}

conus  <- function(d) d[!is_offmap(d), , drop = FALSE]
offmap <- function(d) d[is_offmap(d), , drop = FALSE]

# Direction reads through colour and shape. The shapes mirror EPA's own up/down
# triangles, and the two palette roles are picked for hue rather than for their
# usual focus/base meaning: EPA's caption, reproduced verbatim on this page,
# tells the reader that blue marks locations where floods grew and a warm colour
# marks where they shrank. `base` is the palette's blue and `focus` its warmest
# slot, so this mapping keeps the caption and the map saying the same thing.
direction_colours <- function() {
  stats::setNames(
    c(INDICATOR_PALETTE[["base"]], INDICATOR_PALETTE[["focus"]]),
    c("increase", "decrease")
  )
}
DIRECTION_SHAPES <- c(increase = 24L, decrease = 25L) # filled triangle up / down
DIRECTION_LABELS <- c(increase = "Floods grew", decrease = "Floods shrank")

# Labels for Figure 2, where the same two directions mean frequency, not size.
FREQUENCY_LABELS <- c(increase = "More frequent", decrease = "Less frequent")

prep <- function(d) {
  d <- station_points(d)
  d$direction <- factor(ifelse(d$value >= 0, "increase", "decrease"),
                        levels = c("increase", "decrease"))
  m <- max(abs(d$value))
  d$rel_mag <- if (m == 0) rep(0, nrow(d)) else abs(d$value) / m
  d
}

station_tooltip <- function(d, unit_label, labels) {
  sprintf(
    "%.4f°N, %.4f°W\n%s, %s\n%s %s",
    d$latitude, abs(d$longitude),
    unname(labels[as.character(d$direction)]), COVERAGE,
    fmt(d$value), unit_label
  )
}

station_map_plot <- function(d, unit_label, labels) {
  d <- conus(prep(d))

  ggplot(d) +
    geom_sf(
      data = us_states(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = direction, shape = direction,
        size = sqrt(rel_mag),
        data_id = paste(round(longitude, 4), round(latitude, 4), sep = "/"),
        tooltip = station_tooltip(d, unit_label, labels)
      ),
      colour = CHART_GREY[["surface"]], stroke = 0.4, alpha = 0.9
    ) +
    scale_fill_manual(values = direction_colours(), labels = labels) +
    scale_shape_manual(values = DIRECTION_SHAPES, labels = labels) +
    scale_size(range = c(1.0, 4.6), guide = "none") +
    # default_crs tells coord_sf() that geom_point's raw longitude/latitude
    # columns are unprojected WGS84, so both the sf basemap and the station
    # points get projected into MAP_CRS together.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    guides(
      fill  = guide_legend(nrow = 1, override.aes = list(size = 3.0, alpha = 1)),
      shape = guide_legend(nrow = 1)
    ) +
    theme_void() +
    legend_top()
}

# The stations the Albers panel cannot hold, plotted by trend value along one
# axis and grouped by region along the other, on the same colour, shape, and
# size scale as the map above. Not a second map: at this size an Alaska, Hawaii,
# or Puerto Rico coastline would be a few unreadable pixels.
station_offmap_plot <- function(d, unit_label, labels) {
  d <- offmap(prep(d))
  d$region <- factor(offmap_region(d), levels = c("Alaska", "Hawaii", "Puerto Rico"))

  ggplot(d, aes(x = value, y = region)) +
    geom_vline(xintercept = 0, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_point_interactive(
      aes(
        fill = direction, shape = direction, size = sqrt(rel_mag),
        data_id = paste(round(longitude, 4), round(latitude, 4), sep = "/"),
        tooltip = station_tooltip(d, unit_label, labels)
      ),
      colour = CHART_GREY[["surface"]], stroke = 0.4, alpha = 0.9,
      position = position_jitter(height = 0.15, width = 0, seed = 1)
    ) +
    scale_fill_manual(values = direction_colours(), guide = "none") +
    scale_shape_manual(values = DIRECTION_SHAPES, guide = "none") +
    scale_size(range = c(1.0, 4.6), guide = "none") +
    labs(x = unit_label, y = NULL) +
    theme_indicator() +
    theme(panel.grid.major.y = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4))
}

# Sorted by value ascending, which is also the order EPA's own source file uses.
station_map_table <- function(d, unit_label) {
  d <- station_points(d)
  d <- d[order(d$value), ]
  data.frame(
    Latitude  = sprintf("%.5f°N", d$latitude),
    Longitude = sprintf("%.5f°W", abs(d$longitude)),
    Value     = fmt(d$value),
    Unit      = unit_label,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# How many stations fall outside the CONUS panel, phrased for the sentence that
# introduces the off-map strip. Counted rather than written into the prose, so
# the page cannot go stale against a data update.
offmap_summary <- function(d) {
  r <- offmap_region(offmap(station_points(d)))
  n <- table(factor(r, levels = c("Alaska", "Hawaii", "Puerto Rico")))
  n <- n[n > 0]                       # a region with no stations is not named
  parts <- sprintf("%d %s", as.integer(n), names(n))
  if (length(parts) == 1L) return(parts)
  paste0(paste(parts[-length(parts)], collapse = ", "), ", and ", parts[length(parts)])
}

# The caveat EPA's caption raises but the published data cannot support. Taken
# from the upstream meta.yml `note` field rather than written here, so it stays
# tied to the dataset it describes.
significance_note <- function(meta, file) {
  knitr::asis_output(sprintf(
    "::: {.callout-note appearance=\"simple\"}\n%s\n:::\n",
    meta_for(meta, file)$note
  ))
}

# ---- Figure 1: magnitude trend (Mann-Kendall tau per station) ----------------

fig_1_plot        <- function(d) station_map_plot(d, "tau value", DIRECTION_LABELS)
fig_1             <- function(d) girafe_indicator(fig_1_plot(d), height = 5.0)
fig_1_offmap_plot <- function(d) station_offmap_plot(d, "tau value", DIRECTION_LABELS)
fig_1_offmap      <- function(d) girafe_indicator(fig_1_offmap_plot(d), height = 2.1)
fig_1_table       <- function(d) station_map_table(d, "tau value")

# ---- Figure 2: frequency trend (Poisson regression slope per station) --------

fig_2_plot        <- function(d) station_map_plot(d, "slope value", FREQUENCY_LABELS)
fig_2             <- function(d) girafe_indicator(fig_2_plot(d), height = 5.0)
fig_2_offmap_plot <- function(d) station_offmap_plot(d, "slope value", FREQUENCY_LABELS)
fig_2_offmap      <- function(d) girafe_indicator(fig_2_offmap_plot(d), height = 2.1)
fig_2_table       <- function(d) station_map_table(d, "slope value")
