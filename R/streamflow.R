# Figures for indicators/freshwater-systems/streamflow.qmd.

REPO <- "streamflow"

# Upstream stores latitude, longitude, and value as text so the source file's
# precision survives byte for byte. read_indicator() only coerces `value`;
# latitude and longitude become numeric here, once.
station_points <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d
}

fmt <- function(x) sprintf("%.2f", x)

COVERAGE <- "1940-2022"

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
# panel into Alaska and Hawaii (no Puerto Rico gauge, unlike River Flooding).
is_offmap <- function(d) {
  (d$latitude > 50 & d$longitude < -125) |                        # Alaska
    (d$longitude < -152 & d$longitude > -161 & d$latitude < 23)   # Hawaii
}

offmap_region <- function(d) {
  ifelse(d$latitude > 50 & d$longitude < -125, "Alaska", "Hawaii")
}

conus  <- function(d) d[!is_offmap(d), , drop = FALSE]
offmap <- function(d) d[is_offmap(d), , drop = FALSE]

# Shape always follows the sign of the trend value: an upward triangle is a
# positive trend, a downward triangle negative, on every figure. Which colour
# goes with which sign is not fixed the same way: EPA's own caption for Figure
# 5 assigns brown to a *positive* trend (more low-flow days, drier) and blue to
# negative (wetter), the opposite of the increase-is-`base`, decrease-is-`focus`
# assignment used for Figures 1-4. `swap` picks that reversed mapping.
direction_colours <- function(swap = FALSE) {
  roles <- if (swap) c(increase = "focus", decrease = "base") else c(increase = "base", decrease = "focus")
  stats::setNames(unname(INDICATOR_PALETTE[roles]), names(roles))
}
DIRECTION_SHAPES <- c(increase = 24L, decrease = 25L) # filled triangle up / down

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
    "%s\n%.4f°N, %.4f°W\n%s, %s\n%s %s",
    d$station_name, d$latitude, abs(d$longitude),
    unname(labels[as.character(d$direction)]), COVERAGE,
    fmt(d$value), unit_label
  )
}

station_map_plot <- function(d, unit_label, labels, swap_colours = FALSE) {
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
    scale_fill_manual(values = direction_colours(swap_colours), labels = labels) +
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
# size scale as the map above. Not a second map: at this size an Alaska or
# Hawaii coastline would be a few unreadable pixels.
station_offmap_plot <- function(d, unit_label, labels, swap_colours = FALSE) {
  d <- offmap(prep(d))
  d$region <- factor(offmap_region(d), levels = c("Alaska", "Hawaii"))

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
    scale_fill_manual(values = direction_colours(swap_colours), guide = "none") +
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
    Station   = d$station_name,
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
  n <- table(factor(r, levels = c("Alaska", "Hawaii")))
  n <- n[n > 0]                       # a region with no stations is not named
  if (length(n) == 0L) return("No")   # Figure 4's subnetwork has no such gauge
  parts <- sprintf("%d %s", as.integer(n), names(n))
  if (length(parts) == 1L) return(parts)
  paste0(paste(parts[-length(parts)], collapse = ", "), ", and ", parts[length(parts)])
}

# ---- Figure 1: seven-day low streamflow trend --------------------------------

FLOW_LABELS <- c(increase = "Streamflow increased", decrease = "Streamflow decreased")

fig_1_plot        <- function(d) station_map_plot(d, "percent change", FLOW_LABELS)
fig_1             <- function(d) girafe_indicator(fig_1_plot(d), height = 5.0)
fig_1_offmap_plot <- function(d) station_offmap_plot(d, "percent change", FLOW_LABELS)
fig_1_offmap      <- function(d) girafe_indicator(fig_1_offmap_plot(d), height = 2.1)
fig_1_table       <- function(d) station_map_table(d, "percent change")

# ---- Figure 2: three-day high streamflow trend -------------------------------

fig_2_plot        <- function(d) station_map_plot(d, "percent change", FLOW_LABELS)
fig_2             <- function(d) girafe_indicator(fig_2_plot(d), height = 5.0)
fig_2_offmap_plot <- function(d) station_offmap_plot(d, "percent change", FLOW_LABELS)
fig_2_offmap      <- function(d) girafe_indicator(fig_2_offmap_plot(d), height = 2.1)
fig_2_table       <- function(d) station_map_table(d, "percent change")

# ---- Figure 3: annual average streamflow trend -------------------------------

fig_3_plot        <- function(d) station_map_plot(d, "percent change", FLOW_LABELS)
fig_3             <- function(d) girafe_indicator(fig_3_plot(d), height = 5.0)
fig_3_offmap_plot <- function(d) station_offmap_plot(d, "percent change", FLOW_LABELS)
fig_3_offmap      <- function(d) girafe_indicator(fig_3_offmap_plot(d), height = 2.1)
fig_3_table       <- function(d) station_map_table(d, "percent change")

# ---- Figure 4: winter-spring runoff timing trend ------------------------------

# No station in this dataset shows a positive shift, so the "increase" bucket
# holds only the handful of stations with no change (value == 0); there is no
# Alaska or Hawaii gauge in this subnetwork, so there is no off-map strip.
TIMING_LABELS <- c(increase = "No change", decrease = "Earlier")

fig_4_plot  <- function(d) station_map_plot(d, "days", TIMING_LABELS)
fig_4       <- function(d) girafe_indicator(fig_4_plot(d), height = 5.0)
fig_4_table <- function(d) station_map_table(d, "days")

# ---- Figure 5: very-low-streamflow-days trend --------------------------------

# EPA's caption reverses the usual colour mapping for this figure: brown marks
# a positive trend (more low-flow days, drier) and blue a negative one (fewer,
# wetter), so `swap_colours = TRUE` on every call below.
DRYNESS_LABELS <- c(increase = "More low-flow days (drier)", decrease = "Fewer low-flow days (wetter)")

fig_5_plot        <- function(d) station_map_plot(d, "days", DRYNESS_LABELS, swap_colours = TRUE)
fig_5             <- function(d) girafe_indicator(fig_5_plot(d), height = 5.0)
fig_5_offmap_plot <- function(d) station_offmap_plot(d, "days", DRYNESS_LABELS, swap_colours = TRUE)
fig_5_offmap      <- function(d) girafe_indicator(fig_5_offmap_plot(d), height = 2.1)
fig_5_table       <- function(d) station_map_table(d, "days")
