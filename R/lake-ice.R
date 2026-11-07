# Figures for indicators/snow-and-ice/lake-ice.qmd.

REPO <- "lake-ice"

# Lake order follows EPA's column order: the nine lakes with freeze records,
# then the five (Lake Osakis and the four Maine lakes) that only have thaw dates.
FREEZE_LAKES <- c("Detroit Lake", "Lake George", "Lake Mendota", "Lake Monona", "Mirror Lake",
                  "Otsego Lake", "Shell Lake", "Lake Superior at Bayfield", "Geneva Lake")
THAW_LAKES   <- c(FREEZE_LAKES, "Lake Osakis", "Damariscotta Lake", "Cobbosseecontee Lake",
                  "Sebago Lake", "Moosehead Lake")

# ---- Figures 1 and 2: freeze and thaw dates by lake --------------------------

# Nine and fourteen lakes are more series than the palette has slots, so each
# lake gets its own panel and every panel draws in the same colour.
lake_dates_plot <- function(d, lakes, colour, what, y_label) {
  d$lake <- factor(d$lake, levels = lakes)

  ggplot(d, aes(x = year, y = value, group = lake)) +
    geom_line(colour = colour, linewidth = 0.6) +
    geom_point_interactive(
      aes(data_id = paste(lake, year),
          tooltip = sprintf("%d, %s\n%s: %.1f days from January 1", year, lake, what, value)),
      colour = colour, size = 0.6
    ) +
    scale_x_continuous(breaks = seq(1850, 2000, 50)) +
    facet_wrap(~lake, ncol = 3) +
    labs(x = NULL, y = y_label) +
    theme_indicator(base_size = 12)
}

lake_dates_table <- function(d, lakes) {
  d$lake <- factor(d$lake, levels = lakes)
  out <- tidyr::pivot_wider(d[order(d$year, d$lake), ], id_cols = year,
                            names_from = lake, values_from = value)
  names(out)[1] <- "Year"
  out
}

fig_1_plot <- function(d) {
  lake_dates_plot(d, FREEZE_LAKES, INDICATOR_PALETTE[["base"]], "First freeze",
                  "Date of first freeze (days from January 1)")
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 6)

fig_1_table <- function(d) lake_dates_table(d, FREEZE_LAKES)

fig_2_plot <- function(d) {
  lake_dates_plot(d, THAW_LAKES, INDICATOR_PALETTE[["base"]], "Ice thaw",
                  "Date of ice thaw (days from January 1)")
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 8)

fig_2_table <- function(d) lake_dates_table(d, THAW_LAKES)

# ---- Figure 3: change in ice thaw dates, 1905-2019 ---------------------------

CHANGE_LABELS <- c(earlier = "Earlier thaw", none = "No change")

# State outlines for the basemap, from the maps package (bundled, not fetched
# over the network), cropped below to the Upper Midwest and Northeast where the
# lakes are.
lake_states <- function() {
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
}

MAP_CRS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-82 +datum=WGS84"

# EPA's caption: red circles for the twelve lakes thawing earlier, larger circles
# for larger changes. The two lakes with no change draw as the smallest, grey.
fig_3_plot <- function(d) {
  d$latitude  <- as.numeric(d$latitude)
  d$longitude <- as.numeric(d$longitude)
  d$change    <- factor(ifelse(d$value < 0, "earlier", "none"), levels = names(CHANGE_LABELS))
  d$size      <- abs(d$value)
  cols <- stats::setNames(c(INDICATOR_PALETTE[["focus"]], CHART_GREY[["rule"]]), names(CHANGE_LABELS))

  ggplot(d) +
    geom_sf(data = lake_states(), fill = CHART_GREY[["grid"]],
            colour = CHART_GREY[["surface"]], linewidth = 0.4) +
    geom_point_interactive(
      aes(
        x = longitude, y = latitude, fill = change, size = size,
        data_id = lake,
        tooltip = sprintf("%s\n%+.1f days, 1905-2019", lake, value)
      ),
      shape = 21, colour = CHART_GREY[["surface"]], stroke = 0.6, alpha = 0.9
    ) +
    scale_fill_manual(values = cols, labels = CHANGE_LABELS) +
    # A floor on the size range keeps the zero-change lakes visible.
    scale_size(range = c(2.5, 10), guide = "none") +
    coord_sf(crs = MAP_CRS, default_crs = 4326, datum = NA,
             xlim = c(-99, -67), ylim = c(41, 49)) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(size = 4, alpha = 1))) +
    theme_void() +
    legend_top()
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d), height = 4)

# One row per lake, earliest-thaw change first.
fig_3_table <- function(d) {
  out <- data.frame(
    Lake = d$lake,
    Latitude  = sprintf("%.2f°N", as.numeric(d$latitude)),
    Longitude = sprintf("%.2f°W", abs(as.numeric(d$longitude))),
    `Change in thaw date, 1905-2019 (days)` = sprintf("%.1f", d$value),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out[order(d$value), ]
}
