# Figures for indicators/ecosystems/leaf-and-bloom-dates.qmd.

REPO <- "leaf-and-bloom-dates"

# Figures 1 and 2 are deviations from the 1981-2010 average, so zero is a real
# value the reader has to be able to find.
BASELINE <- 0

EVENT_LABELS <- c(leaf = "First leaf", bloom = "First bloom")

# Leaf and bloom are peers read in the order spring unfolds, so they take the
# two colour roles a chart reads first.
EVENT_KEYS <- c("leaf", "bloom")

# ---- Figures 1 and 2: deviation from the 1981-2010 average ---------------------
#
# Each event is drawn twice, as EPA does: the yearly values thin and the
# nine-year weighted curve thick. Only the yearly points carry a tooltip, since
# the smoothed curve is derived from them.
fig_years_plot <- function(d) {
  d$series_label <- unname(EVENT_LABELS[d$event])
  yearly   <- d[d$statistic == "annual_mean", ]
  smoothed <- d[d$statistic == "9yr_normal_curve", ]
  start    <- min(d$year)

  ggplot(mapping = aes(x = year, y = value, colour = series_label, group = event)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_line(data = yearly, linewidth = 0.4, alpha = 0.6) +
    geom_point_interactive(
      data = yearly,
      aes(
        data_id = interaction(event, year),
        tooltip = sprintf("%d — %s\n%+.1f days vs. the 1981-2010 average", year, series_label, value)
      ),
      size = 1.2
    ) +
    geom_line(data = smoothed, linewidth = 1.3) +
    scale_colour_manual(
      values = label_colours(d, "event", "series_label", EVENT_KEYS),
      breaks = label_order(d, "event", "series_label", EVENT_KEYS)
    ) +
    scale_x_continuous(breaks = seq(ceiling(start / 20) * 20, 2020, 20)) +
    labs(x = NULL, y = "Days later or earlier than average") +
    theme_indicator() +
    legend_top()
}

fig_years_table <- function(d) {
  d$series_label <- paste(
    unname(EVENT_LABELS[d$event]),
    ifelse(d$statistic == "annual_mean", "mean", "9-yr normal curve")
  )
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}

fig_1_plot  <- fig_years_plot
fig_1       <- function(d) girafe_indicator(fig_1_plot(d))
fig_1_table <- fig_years_table

fig_2_plot  <- fig_years_plot
fig_2       <- function(d) girafe_indicator(fig_2_plot(d))
fig_2_table <- fig_years_table

# ---- Figures 3 and 4: change at individual stations -----------------------------

# Upstream holds latitude and longitude as text so the source file's precision
# survives byte for byte, and read_indicator() only coerces `value`. They
# become numeric here, once, for the chart and the table.
station_points <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d
}

# The stations span the contiguous 48 states and Alaska, so the basemap is the
# US outline cropped to the mainland and Alaska: the Aleutians cross the
# antimeridian and Hawaii has no stations, and either would stretch the extent.
# `maps` has no state layer for Alaska in its "world" database, so this draws
# the national outline only.
us_outline <- function() {
  us <- sf::st_as_sf(maps::map("world", region = "USA", plot = FALSE, fill = TRUE))
  suppressWarnings(sf::st_crop(us, xmin = -170, xmax = -60, ymin = 24, ymax = 72))
}

# North America Albers Equal Area Conic (ESRI:102008): unlike a CONUS-only
# projection it keeps Alaska in a sensible position and shape.
MAP_CRS <- "ESRI:102008"

# A negative change is an earlier date. Earlier takes `base` and later `focus`,
# so the minority outcome in the South and Upper Midwest is the one that stands
# out.
DIRECTION_ROLE_ORDER   <- c("earlier", "later")
DIRECTION_LEGEND_ORDER <- c("earlier", "later")
DIRECTION_LABELS       <- c(earlier = "Earlier", later = "Later")

fig_change_plot <- function(d, event_label) {
  d <- station_points(d)
  d$direction_key   <- ifelse(d$value < 0, "earlier", "later")
  d$direction_label <- unname(DIRECTION_LABELS[d$direction_key])
  # Draw the larger changes first so a big circle never hides a small one.
  d <- d[order(abs(d$value), decreasing = TRUE), ]

  ggplot(d) +
    geom_sf(
      data = us_outline(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = direction_label,
        size = sqrt(abs(value)),
        data_id = paste(latitude, longitude),
        tooltip = sprintf(
          "%.2f°N, %.2f°W\n%+.1f days, %s (%s)",
          latitude, abs(longitude), value, event_label, tolower(direction_label)
        )
      ),
      shape = 21, colour = CHART_GREY[["surface"]], stroke = 0.2, alpha = 0.85
    ) +
    scale_fill_manual(
      values = label_colours(d, "direction_key", "direction_label", DIRECTION_ROLE_ORDER),
      breaks = label_order(d, "direction_key", "direction_label", DIRECTION_LEGEND_ORDER)
    ) +
    # About 1,300 stations overlap heavily in the East, so the circles stay small.
    scale_size(range = c(0.6, 2.6), guide = "none") +
    # default_crs tells coord_sf() that geom_point's raw longitude/latitude
    # columns are unprojected WGS84, so the basemap and the station points get
    # projected into MAP_CRS together.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(size = 3.2))) +
    theme_void() +
    legend_top()
}

fig_change_table <- function(d, value_label) {
  d <- station_points(d)
  d <- d[order(d$value), ]
  out <- data.frame(
    Latitude  = sprintf("%.2f°N", d$latitude),
    Longitude = sprintf("%.2f°W", abs(d$longitude)),
    sprintf("%+.2f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(out)[3] <- value_label
  out
}

fig_3_plot  <- function(d) fig_change_plot(d, "first leaf date")
fig_3       <- function(d) girafe_indicator(fig_3_plot(d), height = 4.6)
fig_3_table <- function(d) fig_change_table(d, "Change in first leaf date (days)")

fig_4_plot  <- function(d) fig_change_plot(d, "first bloom date")
fig_4       <- function(d) girafe_indicator(fig_4_plot(d), height = 4.6)
fig_4_table <- function(d) fig_change_table(d, "Change in first bloom date (days)")
