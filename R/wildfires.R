# Figures for indicators/weather-and-extreme-events/wildfires.qmd.

REPO <- "wildfires"

fmt_n <- function(x) format(round(as.numeric(x)), big.mark = ",")

MONTH_LEVELS <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

# ---- Figure 1: Wildfire Frequency in the United States, 1983-2022 ------------
#
# EPA's own caption calls the Forest Service series "the orange line"; the key
# order below puts it second so series_colours()/label_colours() assign it
# INDICATOR_PALETTE's focus slot, which is the palette's orange.
FIG12_ORDER <- c("nifc", "usfs")

fig_1_plot <- function(d) {
  ggplot(d, aes(x = year, y = value, colour = series_label, group = series_key)) +
    geom_line_interactive(aes(data_id = series_key), linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = series_key,
        tooltip = sprintf("%d — %s\n%s fires", year, series_label, fmt_n(value))
      ),
      size = 1.8
    ) +
    scale_colour_manual(
      values = label_colours(d, "series_key", "series_label", FIG12_ORDER),
      breaks = label_order(d, "series_key", "series_label", FIG12_ORDER)
    ) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(1985, 2020, 5)) +
    scale_y_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, 0.06)),
      labels = function(x) format(x, big.mark = ",")
    ) +
    labs(x = NULL, y = "Number of wildfires") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

# Forest Service reporting stops in 1997; later years are blank, not zero.
fig_1_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
  out <- data.frame(Year = w$year, check.names = FALSE)
  for (nm in setdiff(names(w), "year")) {
    out[[nm]] <- ifelse(is.na(w[[nm]]), "", fmt_n(w[[nm]]))
  }
  out
}

# ---- Figure 2: Wildfire Extent in the United States, 1983-2022 ---------------
#
# Same two series and the same "orange line" caption as figure 1.

fig_2_plot <- function(d) {
  ggplot(d, aes(x = year, y = value, colour = series_label, group = series_key)) +
    geom_line_interactive(aes(data_id = series_key), linewidth = 0.9) +
    geom_point_interactive(
      aes(
        data_id = series_key,
        tooltip = sprintf("%d — %s\n%s acres", year, series_label, fmt_n(value))
      ),
      size = 1.8
    ) +
    scale_colour_manual(
      values = label_colours(d, "series_key", "series_label", FIG12_ORDER),
      breaks = label_order(d, "series_key", "series_label", FIG12_ORDER)
    ) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(1985, 2020, 5)) +
    scale_y_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, 0.06)),
      labels = function(x) format(x, big.mark = ",")
    ) +
    labs(x = NULL, y = "Burned area (acres)") +
    theme_indicator() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d))

fig_2_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
  out <- data.frame(Year = w$year, check.names = FALSE)
  for (nm in setdiff(names(w), "year")) {
    out[[nm]] <- ifelse(is.na(w[[nm]]), "", fmt_n(w[[nm]]))
  }
  out
}

# ---- Figure 3: Damage Caused by Wildfires in the United States, 1984-2021 ----
#
# Stack order follows EPA's own column order, mild to severe, then the two
# non-severity categories. Colour order is different: high is what the page's
# Key Points single out (5 to 22 percent of a year's burned area), so it takes
# the focus slot even though it stacks in the middle rather than on top.
FIG3_STACK_ORDER  <- c("low", "moderate", "high", "unburned", "increased_greenness")
FIG3_COLOUR_KEYS  <- c("low", "high", "moderate", "unburned", "increased_greenness")

fig_3_plot <- function(d) {
  key_labels <- stats::setNames(
    d$series_label[match(FIG3_STACK_ORDER, d$series_key)], FIG3_STACK_ORDER
  )
  d$series_key <- factor(d$series_key, levels = FIG3_STACK_ORDER)

  ggplot(d, aes(x = year, y = value, fill = series_key, group = series_key)) +
    geom_area_interactive(
      aes(
        data_id = as.character(series_key),
        tooltip = sprintf(
          "%d — %s\n%s acres", year, key_labels[as.character(series_key)], fmt_n(value)
        )
      ),
      position = "stack", colour = NA
    ) +
    scale_fill_manual(
      values = series_colours(FIG3_COLOUR_KEYS),
      breaks = FIG3_STACK_ORDER, labels = key_labels[FIG3_STACK_ORDER]
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(1985, 2020, 5)) +
    scale_y_continuous(
      labels = function(x) format(x, big.mark = ","),
      limits = c(0, NA), expand = expansion(mult = c(0, 0.04))
    ) +
    labs(x = NULL, y = "Burned area (acres)") +
    theme_indicator() +
    legend_top()
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d), height = 4.6)

fig_3_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
  out <- data.frame(Year = w$year, check.names = FALSE)
  for (nm in setdiff(names(w), "year")) out[[nm]] <- fmt_n(w[[nm]])
  out
}

# ---- Figures 4 and 5: state-level burned acreage maps ------------------------
#
# EPA's own figures are choropleths, so unlike the point-based maps elsewhere
# on this site (heat-waves, river-flooding), this one fills the CONUS state
# polygons themselves rather than overlaying markers on them.

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

# maps::map("state") is the 49 CONUS-plus-DC polygon set: it has no Alaska or
# Hawaii geometry at all, though both report burned acreage upstream. Rather
# than a second, mostly-empty map (the drought indicator's choropleth-by-dot
# note explains that tradeoff for a much larger omission), their values are
# left in the table beneath the map and named in the prose above it, generated
# here so the page cannot go stale if a future data update adds or drops one.
offmap_states <- function(state_names_lower) {
  title_case(setdiff(unique(state_names_lower), us_states()$ID))
}

fig_4_offmap_states <- function(d) offmap_states(tolower(state.name[match(d$state, state.abb)]))
fig_5_offmap_states <- function(d) offmap_states(tolower(d$state))

# EPA shades a state grey when it had no fire large enough to qualify for this
# analysis. na.value on the fill scale reproduces that directly: every CONUS
# polygon with no matching row (five small eastern states, none of them
# missing for a geometry reason) draws in that same grey.
fig_4_map_plot <- function(d) {
  d$region <- tolower(state.name[match(d$state, state.abb)])
  poly <- dplyr::left_join(us_states(), d, by = c("ID" = "region"))

  ggplot(poly) +
    geom_sf_interactive(
      aes(
        fill = value, data_id = ID,
        tooltip = ifelse(
          is.na(value),
          paste0(title_case(ID), "\nNo qualifying fires"),
          sprintf("%s\n%.3f acres per square mile (trend)", title_case(ID), value)
        )
      ),
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    scale_fill_gradient(
      low = CHART_GREY[["grid"]], high = INDICATOR_PALETTE[["focus"]],
      na.value = CHART_GREY[["grid"]],
      guide = guide_colourbar(barwidth = unit(7, "lines"), barheight = unit(0.5, "lines"))
    ) +
    coord_sf(crs = MAP_CRS, datum = NA) +
    labs(fill = "Acres per square mile") +
    theme_void() +
    theme(
      legend.position = "top",
      legend.title    = element_text(size = rel(0.8), colour = CHART_GREY[["text"]]),
      legend.text     = element_text(size = rel(0.7), colour = CHART_GREY[["text"]])
    )
}

fig_4_map <- function(d) girafe_indicator(fig_4_map_plot(d), height = 5.0)

fig_4_table <- function(d) {
  out <- data.frame(
    State = state.name[match(d$state, state.abb)],
    `Trend (acres per square mile)` = sprintf("%.3f", as.numeric(d$value)),
    check.names = FALSE
  )
  out[order(out$State), ]
}

# Diverging: negative values are a decrease in burned acreage between the two
# periods, positive an increase. Limits are symmetric around zero so the same
# magnitude of increase and decrease reads as the same fill intensity.
fig_5_map_plot <- function(d) {
  d$region <- tolower(d$state)
  poly <- dplyr::left_join(us_states(), d, by = c("ID" = "region"))
  lim  <- max(abs(poly$value), na.rm = TRUE)

  ggplot(poly) +
    geom_sf_interactive(
      aes(
        fill = value, data_id = ID,
        tooltip = ifelse(
          is.na(value),
          paste0(title_case(ID), "\nNo qualifying fires"),
          sprintf("%s\n%+.3f acres per square mile change", title_case(ID), value)
        )
      ),
      colour = CHART_GREY[["rule"]], linewidth = 0.3
    ) +
    scale_fill_gradient2(
      low = INDICATOR_PALETTE[["base"]], mid = "white", high = INDICATOR_PALETTE[["focus"]],
      midpoint = 0, limits = c(-lim, lim), na.value = CHART_GREY[["grid"]],
      guide = guide_colourbar(barwidth = unit(7, "lines"), barheight = unit(0.5, "lines"))
    ) +
    coord_sf(crs = MAP_CRS, datum = NA) +
    labs(fill = "Change (acres per square mile)") +
    theme_void() +
    theme(
      legend.position = "top",
      legend.title    = element_text(size = rel(0.8), colour = CHART_GREY[["text"]]),
      legend.text     = element_text(size = rel(0.7), colour = CHART_GREY[["text"]])
    )
}

fig_5_map <- function(d) girafe_indicator(fig_5_map_plot(d), height = 5.0)

# Sorted by value ascending: largest decrease first, largest increase last.
fig_5_table <- function(d) {
  d <- d[order(as.numeric(d$value)), ]
  data.frame(
    State  = d$state,
    Change = sprintf("%+.3f", as.numeric(d$value)),
    check.names = FALSE
  )
}

# ---- Figure 6: Comparison of Monthly Burned Area Due to Wildfires in the United States Between 1984-2002 and 2003-2021 ----

FIG67_ORDER <- c("early", "late")

fig_6_plot <- function(d) {
  d$month <- factor(d$month, levels = MONTH_LEVELS)

  ggplot(d, aes(x = month, y = value, fill = series_label)) +
    geom_col_interactive(
      aes(
        data_id = series_key,
        tooltip = sprintf("%s, %s\n%.3f million acres per year", month, series_label, value)
      ),
      position = position_dodge(width = 0.8), width = 0.7
    ) +
    scale_fill_manual(
      values = label_colours(d, "series_key", "series_label", FIG67_ORDER),
      breaks = label_order(d, "series_key", "series_label", FIG67_ORDER)
    ) +
    scale_x_discrete(labels = substr(MONTH_LEVELS, 1, 3)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Average burned area (million acres per year)") +
    theme_indicator() +
    legend_top()
}

fig_6 <- function(d) girafe_indicator(fig_6_plot(d))

fig_6_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = month, names_from = series_label, values_from = value)
  w$month <- factor(w$month, levels = MONTH_LEVELS)
  w <- w[order(w$month), ]
  out <- data.frame(Month = as.character(w$month), check.names = FALSE)
  for (nm in setdiff(names(w), "month")) out[[nm]] <- sprintf("%.3f", as.numeric(w[[nm]]))
  out
}

# ---- Figure 7: Comparison of Monthly Burned Area Due to Wildfires in the Eastern and Western United States Between 1984-2002 and 2003-2021 ----
#
# Same two periods as figure 6, split by region into its own panel so the
# same colour mapping (FIG67_ORDER) reads the same way in both figures.

fig_7_plot <- function(d) {
  d$month <- factor(d$month, levels = MONTH_LEVELS)

  ggplot(d, aes(x = month, y = value, fill = series_label)) +
    geom_col_interactive(
      aes(
        data_id = paste(region, series_key, sep = "/"),
        tooltip = sprintf(
          "%s, %s, %s\n%.3f million acres per year", region, month, series_label, value
        )
      ),
      position = position_dodge(width = 0.8), width = 0.7
    ) +
    scale_fill_manual(
      values = label_colours(d, "series_key", "series_label", FIG67_ORDER),
      breaks = label_order(d, "series_key", "series_label", FIG67_ORDER)
    ) +
    # free_y: the West burns roughly an order of magnitude more area than the
    # East every month (see fig_7_table). A shared axis would flatten the East
    # panel to a flat line and hide its own seasonal pattern.
    facet_wrap(~region, ncol = 1, scales = "free_y") +
    scale_x_discrete(labels = substr(MONTH_LEVELS, 1, 3)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Average burned area (million acres per year)") +
    theme_indicator() +
    legend_top() +
    theme(panel.spacing.y = unit(1.1, "lines"))
}

fig_7 <- function(d) girafe_indicator(fig_7_plot(d), height = 6.0)

fig_7_table <- function(d) {
  w <- tidyr::pivot_wider(
    d, id_cols = c(month, region), names_from = series_label, values_from = value
  )
  w$month <- factor(w$month, levels = MONTH_LEVELS)
  w <- w[order(w$region, w$month), ]
  out <- data.frame(Region = w$region, Month = as.character(w$month), check.names = FALSE)
  for (nm in setdiff(names(w), c("month", "region"))) out[[nm]] <- sprintf("%.3f", as.numeric(w[[nm]]))
  out
}
