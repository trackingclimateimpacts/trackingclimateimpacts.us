# Figures for indicators/freshwater-systems/lake-temperature.qmd.

REPO <- "lake-temperature"

DIRECTION_LABELS <- c(increase = "Warming", decrease = "Cooling")
SIGNIFICANCE_LABELS <- c(yes = "Statistically significant", no = "Not significant")

# Upstream stores every column as text so the source precision survives byte
# for byte. read_indicator() only coerces `value`; the coordinates become
# numeric here, once.
lake_points <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d$direction <- factor(ifelse(d$value >= 0, "increase", "decrease"),
                        levels = c("increase", "decrease"))
  d$significance <- factor(ifelse(d$significant == "Yes", "yes", "no"),
                           levels = c("yes", "no"))
  d$rel_mag <- abs(d$value) / max(abs(d$value))
  d
}

# Country outlines for the basemap, from the maps package (bundled, not
# fetched over the network). Prefix matching (not exact) pulls in the islands
# the lakes sit on, such as Cape Breton Island for Bras d'Or Lake, and Alaska
# for Iliamna Lake; Hawaii falls outside the map frame.
north_america <- function() {
  sf::st_as_sf(maps::map("world", regions = c("Canada", "Mexico", "USA"),
                         exact = FALSE, plot = FALSE, fill = TRUE))
}

# Lambert azimuthal equal-area centred on the lakes (US, Canada, Alaska, Mexico).
MAP_CRS <- "+proj=laea +lat_0=50 +lon_0=-105 +datum=WGS84"

# ---- Figure 1: change in lake surface temperature, 1985-2009 -----------------

# Colours follow EPA's caption: red circles for warming, blue for cooling, a
# dark border on the lakes where the trend was statistically significant.
fig_1_plot <- function(d) {
  d <- lake_points(d)
  cols <- stats::setNames(unname(INDICATOR_PALETTE[c("focus", "base")]), names(DIRECTION_LABELS))
  rings <- stats::setNames(c(CHART_GREY[["ink"]], CHART_GREY[["surface"]]), names(SIGNIFICANCE_LABELS))

  ggplot(d) +
    geom_sf(
      data = north_america(), fill = CHART_GREY[["grid"]],
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = direction, colour = significance,
        size = sqrt(rel_mag),
        # Row index, not the lake name: ggiraph rejects the apostrophe in
        # "Bras d'Or Lake" inside a data_id attribute.
        data_id = seq_along(lake),
        tooltip = sprintf("%s\n%.2f °F, 1985-2009\n%s", lake, value,
                          unname(SIGNIFICANCE_LABELS[as.character(significance)]))
      ),
      shape = 21, stroke = 0.9, alpha = 0.9
    ) +
    scale_fill_manual(values = cols, labels = DIRECTION_LABELS) +
    scale_colour_manual(values = rings, labels = SIGNIFICANCE_LABELS) +
    scale_size(range = c(1.5, 6), guide = "none") +
    # xlim/ylim are read in default_crs (lon/lat) and frame the lakes rather
    # than the whole continent.
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA,
             xlim = c(-170, -50), ylim = c(15, 85)) +
    guides(
      fill   = guide_legend(nrow = 1, order = 1, override.aes = list(size = 3.5, alpha = 1, colour = CHART_GREY[["surface"]])),
      colour = guide_legend(nrow = 1, order = 2, override.aes = list(size = 3.5, alpha = 1, fill = CHART_GREY[["grid"]]))
    ) +
    theme_void() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 5.5)

# One row per lake, sorted by total change.
fig_1_table <- function(d) {
  out <- data.frame(
    Lake = d$lake,
    Latitude  = sprintf("%.2f°N", as.numeric(d$latitude)),
    Longitude = sprintf("%.2f°W", abs(as.numeric(d$longitude))),
    `Total change (°F)` = sprintf("%.2f", d$value),
    Significant = d$significant,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out[order(d$value, decreasing = TRUE), ]
}
