# Figures for indicators/ecosystems/marine-species-distribution.qmd.

REPO <- "marine-species-distribution"

# Figure 1 sets every region to zero in 1989, so zero is a real reference line.
BASELINE <- 0

# ---- Figure 1: change in latitude and depth, 1974-2022 -----------------------

FIG1_KEYS <- c("Multi-Region Average", "Northeast (Spring)",
               "Eastern Bering Sea", "Southeast (Spring)")
FIG1_PANELS <- c(latitude = "Change in latitude (miles)", depth = "Change in depth (feet)")

fig_1_plot <- function(d) {
  d$region   <- factor(d$region, levels = FIG1_KEYS)
  d$variable <- factor(d$variable, levels = names(FIG1_PANELS))
  cols <- series_colours(FIG1_KEYS)

  ggplot(d, aes(x = year, y = value, colour = region, group = region)) +
    geom_hline(yintercept = BASELINE, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_line(linewidth = 0.7) +
    geom_point_interactive(
      aes(
        data_id = paste(region, year),
        tooltip = sprintf("%s, %d\n%s: %+.1f %s", region, year,
                          unname(FIG1_PANELS[as.character(variable)]), value, unit)
      ),
      size = 1
    ) +
    facet_wrap(vars(variable), ncol = 1, scales = "free_y",
               labeller = as_labeller(FIG1_PANELS), strip.position = "left") +
    scale_colour_manual(values = cols) +
    scale_x_continuous(breaks = seq(1980, 2020, 10)) +
    labs(x = NULL, y = NULL) +
    theme_indicator() +
    theme(strip.placement = "outside") +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 6)

fig_1_table <- function(d) {
  d$variable <- paste(d$region, d$variable, sep = " - ")
  tidyr::pivot_wider(d[order(d$year), ], id_cols = year, names_from = variable, values_from = value)
}

# ---- Figures 2-4: center of biomass of three species -------------------------

# One panel per species, with its own axes: the three species of a region sit in
# different parts of the shelf, so shared axes would crush each track.
# Points run from light (first year) to dark (last year).
species_plot <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = c(year, species), names_from = variable, values_from = value)
  w$species <- factor(w$species, levels = unique(d$species))

  ggplot(w, aes(x = longitude, y = latitude)) +
    geom_path(colour = CHART_GREY[["grid"]], linewidth = 0.5) +
    geom_point_interactive(
      aes(
        colour = year, data_id = paste(species, year),
        tooltip = sprintf("%s, %d\nLatitude: %.2f°\nLongitude: %.2f°\nChange in latitude: %+.1f miles",
                          species, year, latitude, longitude, change_in_latitude)
      ),
      size = 2
    ) +
    facet_wrap(vars(species), nrow = 1, scales = "free") +
    scale_colour_gradient(low = CHART_GREY[["grid"]], high = INDICATOR_PALETTE[["base"]]) +
    labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
    theme_indicator() +
    theme(panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4)) +
    legend_top() +
    theme(legend.position = "top", legend.key.width = unit(6, "lines"))
}

species_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = c(species, year), names_from = variable, values_from = value)
  names(w) <- c("species", "year", "latitude (degrees)", "longitude (degrees)",
                "change in latitude (miles)")
  w[order(match(w$species, unique(d$species)), w$year), ]
}

fig_2_plot  <- species_plot
fig_2       <- function(d) girafe_indicator(fig_2_plot(d), height = 3.8)
fig_2_table <- species_table

fig_3_plot  <- species_plot
fig_3       <- function(d) girafe_indicator(fig_3_plot(d), height = 3.8)
fig_3_table <- species_table

fig_4_plot  <- species_plot
fig_4       <- function(d) girafe_indicator(fig_4_plot(d), height = 3.8)
fig_4_table <- species_table
