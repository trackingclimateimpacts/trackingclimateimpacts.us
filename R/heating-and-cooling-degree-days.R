# Figures for indicators/societal-systems/heating-and-cooling-degree-days.qmd.

REPO <- "heating-and-cooling-degree-days"

fmt_n <- function(x) format(round(as.numeric(x)), big.mark = ",")

# ---- Figure 1: national annual heating and cooling degree days, 1895-2023 ----

FIG1_ORDER <- c("heating", "cooling")

fig_1_plot <- function(d) {
  ggplot(d, aes(x = year, y = value, colour = series_label, group = series_key)) +
    geom_line_interactive(aes(data_id = series_key), linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = series_key,
        tooltip = sprintf("%d — %s\n%s degree days", year, series_label, fmt_n(value))
      ),
      size = 1.6
    ) +
    scale_colour_manual(
      values = label_colours(d, "series_key", "series_label", FIG1_ORDER),
      breaks = label_order(d, "series_key", "series_label", FIG1_ORDER)
    ) +
    scale_x_continuous(breaks = seq(1900, 2020, 20)) +
    scale_y_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, 0.06)),
      labels = function(x) format(x, big.mark = ",")
    ) +
    labs(x = NULL, y = "Degree days") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
  out <- data.frame(Year = w$year, check.names = FALSE)
  for (nm in setdiff(names(w), "year")) out[[nm]] <- fmt_n(w[[nm]])
  out
}

# ---- Figures 2 and 3: change in heating/cooling degree days by state, --------
# ---- 1960-2023 versus 1895-1959 -----------------------------------------------
#
# EPA's own figures are choropleths, so like the wildfires state maps (and
# unlike the point-based maps elsewhere on this site), this fills the CONUS
# state polygons themselves rather than overlaying markers on them.

# CONUS state polygons for the basemap. Not indicator data, but bundled
# geometry shipped with the maps package and not fetched over the network, so
# it is built here rather than threaded in from the page's setup chunk.
us_states <- function() {
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
}

# NAD83 / Conus Albers (EPSG:5070): the standard equal-area projection for
# CONUS-wide maps, so state shapes read correctly instead of the straight-line
# distortion of unprojected lon/lat.
MAP_CRS <- 5070

title_case <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

# The basemap's 49 CONUS-plus-DC polygons all have a match in this indicator's
# 48-state data except DC, which the source data never reports; na.value
# shades it the same grey as any true gap would be.

# EPA colours this map by temperature, not by the raw sign of the change: a
# state that warmed needs less heat, so a *negative* change in heating degree
# days is the "warm" colour and a *positive* change (it got colder) is the
# "cool" colour — the reverse of figure 3, where a positive change in cooling
# degree days already means the state got warmer.
fig_2_map_plot <- function(d) {
  d$region <- tolower(state.name[match(d$state, state.abb)])
  poly <- dplyr::left_join(us_states(), d, by = c("ID" = "region"))
  lim  <- max(abs(poly$value), na.rm = TRUE)

  ggplot(poly) +
    geom_sf_interactive(
      aes(
        fill = value, data_id = ID,
        tooltip = sprintf("%s\n%+.1f heating degree days", title_case(ID), value)
      ),
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    scale_fill_gradient2(
      low = INDICATOR_PALETTE[["focus"]], mid = "white", high = INDICATOR_PALETTE[["base"]],
      midpoint = 0, limits = c(-lim, lim), na.value = CHART_GREY[["grid"]],
      guide = guide_colourbar(barwidth = unit(7, "lines"), barheight = unit(0.5, "lines"))
    ) +
    coord_sf(crs = MAP_CRS, datum = NA) +
    labs(fill = "Change in heating degree days") +
    theme_void() +
    theme(
      legend.position = "top",
      legend.title    = element_text(size = rel(0.8), colour = CHART_GREY[["text"]]),
      legend.text     = element_text(size = rel(0.7), colour = CHART_GREY[["text"]])
    )
}

fig_2_map <- function(d) girafe_indicator(fig_2_map_plot(d), height = 5.0)

# Sorted by value ascending: largest decrease (warmest) first, largest
# increase (coolest) last.
fig_2_table <- function(d) {
  d <- d[order(as.numeric(d$value)), ]
  data.frame(
    State  = state.name[match(d$state, state.abb)],
    Change = sprintf("%+.2f", as.numeric(d$value)),
    check.names = FALSE
  )
}

# Figure 3: same shape as figure 2, but here a positive change in cooling
# degree days already means the state got warmer, so the usual direction
# applies — negative (cooler) draws in the base blue, positive (warmer) in the
# focus orange.
fig_3_map_plot <- function(d) {
  d$region <- tolower(state.name[match(d$state, state.abb)])
  poly <- dplyr::left_join(us_states(), d, by = c("ID" = "region"))
  lim  <- max(abs(poly$value), na.rm = TRUE)

  ggplot(poly) +
    geom_sf_interactive(
      aes(
        fill = value, data_id = ID,
        tooltip = sprintf("%s\n%+.1f cooling degree days", title_case(ID), value)
      ),
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    scale_fill_gradient2(
      low = INDICATOR_PALETTE[["base"]], mid = "white", high = INDICATOR_PALETTE[["focus"]],
      midpoint = 0, limits = c(-lim, lim), na.value = CHART_GREY[["grid"]],
      guide = guide_colourbar(barwidth = unit(7, "lines"), barheight = unit(0.5, "lines"))
    ) +
    coord_sf(crs = MAP_CRS, datum = NA) +
    labs(fill = "Change in cooling degree days") +
    theme_void() +
    theme(
      legend.position = "top",
      legend.title    = element_text(size = rel(0.8), colour = CHART_GREY[["text"]]),
      legend.text     = element_text(size = rel(0.7), colour = CHART_GREY[["text"]])
    )
}

fig_3_map <- function(d) girafe_indicator(fig_3_map_plot(d), height = 5.0)

fig_3_table <- function(d) {
  d <- d[order(as.numeric(d$value)), ]
  data.frame(
    State  = state.name[match(d$state, state.abb)],
    Change = sprintf("%+.2f", as.numeric(d$value)),
    check.names = FALSE
  )
}
