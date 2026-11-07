# Figures for indicators/permafrost.qmd.

REPO <- "permafrost"

# ---- Figure 1: permafrost temperatures by site -------------------------------

# Site order and region follow EPA's column order: the eight interior Alaska
# boreholes first, then the seven in northern Alaska. Figure 1 names the sites
# differently from Figure 2, so the region split is listed here by its own names.
INTERIOR_SITES <- c("Livengood", "C'uul C'ena' (Gulkana)", "Birch Lake", "Old Man",
                    "T'eedrinjik (Chandalar Shelf)", "Deenaalee (Healy)",
                    "Troth Yeddha' (College Peat)", "Coldfoot")
NORTHERN_SITES <- c("West Dock", "Deadhorse", "Franklin Bluffs", "Happy Valley",
                    "Galbraith Lake", "Utqiagvik (Barrow)", "Qaaktugvik (Kaktovik)")
SITES <- c(INTERIOR_SITES, NORTHERN_SITES)

REGION_LABELS <- c(interior = "Interior Alaska", northern = "Northern Alaska")

# Fifteen sites are more series than the palette has slots, so each site gets
# its own panel, coloured by region.
fig_1_plot <- function(d) {
  d$site   <- factor(d$site, levels = SITES)
  d$region <- factor(ifelse(d$site %in% NORTHERN_SITES, "northern", "interior"),
                     levels = names(REGION_LABELS))

  ggplot(d, aes(x = year, y = value, colour = region, group = site)) +
    geom_line(linewidth = 0.6) +
    geom_point_interactive(
      # Row index, not site and year: ggiraph rejects the apostrophes in
      # native site names such as "C'uul C'ena'" inside a data_id attribute.
      aes(data_id = seq_along(site),
          tooltip = sprintf("%d, %s\n%.1f°F", year, site, value)),
      size = 0.8
    ) +
    scale_colour_manual(values = series_colours(names(REGION_LABELS)), labels = REGION_LABELS) +
    scale_x_continuous(breaks = seq(1980, 2020, 20)) +
    facet_wrap(~site, ncol = 3) +
    labs(x = NULL, y = "Permafrost temperature (°F)") +
    theme_indicator(base_size = 12) +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 8)

fig_1_table <- function(d) {
  d$site <- factor(d$site, levels = SITES)
  out <- tidyr::pivot_wider(d[order(d$year, d$site), ], id_cols = year,
                            names_from = site, values_from = value, names_sort = TRUE)
  names(out)[1] <- "Year"
  out
}

# ---- Figure 2: change in permafrost temperature per decade -------------------

TREND_LABELS <- c(cooling = "Cooling", warming = "Warming")

# Coastline from the maps package (bundled, not fetched over the network).
alaska <- function() {
  sf::st_as_sf(maps::map("world", "USA:Alaska", plot = FALSE, fill = TRUE))
}

# Alaska Albers equal-area.
MAP_CRS <- 3338

# EPA's caption: larger circles for faster warming, and the one cooling site
# (Livengood) drawn as a blue circle.
fig_2_plot <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d$trend     <- factor(ifelse(d$value < 0, "cooling", "warming"), levels = names(TREND_LABELS))
  d$size      <- abs(d$value)
  d$label     <- ifelse(d$native_name == "", d$site, sprintf("%s (%s)", d$native_name, d$site))

  ggplot(d) +
    geom_sf(data = alaska(), fill = CHART_GREY[["grid"]],
            colour = CHART_GREY[["surface"]], linewidth = 0.4) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = trend, size = size,
        data_id = site,
        tooltip = sprintf("%s\n%s\n%+.2f°F per decade", label, region, value)
      ),
      shape = 21, colour = CHART_GREY[["surface"]], stroke = 0.6, alpha = 0.9
    ) +
    scale_fill_manual(values = series_colours(names(TREND_LABELS)), labels = TREND_LABELS) +
    # A floor on the size range keeps the near-zero Livengood trend visible.
    scale_size(range = c(2.5, 10), guide = "none") +
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA,
             xlim = c(-166, -141), ylim = c(59, 71.5)) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(size = 4, alpha = 1))) +
    theme_void() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 6)

# One row per site, fastest warming first.
fig_2_table <- function(d) {
  out <- data.frame(
    Site          = d$site,
    `Native name` = d$native_name,
    Region        = d$region,
    `Depth (m)`   = d$depth_m,
    Latitude      = sprintf("%.2f°N", as.numeric(d$latitude)),
    Longitude     = sprintf("%.2f°W", abs(as.numeric(d$longitude))),
    `Elevation (m)` = d$elevation_m,
    `Change (°F per decade)` = sprintf("%+.2f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out[order(-d$value), ]
}
