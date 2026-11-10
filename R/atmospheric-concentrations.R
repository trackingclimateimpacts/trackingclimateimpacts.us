# Figures for indicators/atmospheric-concentrations.qmd.

REPO <- "atmospheric-concentrations"

# read_indicator() only converts a column named `year`. Figures 1 to 4 carry
# fractional, and for the ice cores negative, calendar years in `year_ce` and
# `year_decimal`, so those are converted here.
as_numeric_time <- function(d) {
  for (col in intersect(c("year_ce", "year_decimal"), names(d))) d[[col]] <- as.numeric(d[[col]])
  d
}

# Calendar years in Figures 1 to 3 run from about 800,000 BC, stored as negative
# numbers. ggplot would print "-800000"; the tooltip says BC instead.
year_label <- function(y) {
  ifelse(y < 0, sprintf("%s BC", format(round(-y), big.mark = ",", trim = TRUE)),
         sprintf("%.0f", y))
}

# Figures 1 to 3 chart roughly 800,000 years in which the last 300 are the whole
# story. One axis cannot show both, so every chart is drawn twice side by side:
# the full record, then 1700 onward. The second panel is a zoom on the same rows,
# not different data.
LONG_RECORD_PANELS <- c("800,000 BC to 2023", "1700 to 2023")

with_zoom_panels <- function(d) {
  full <- transform(d, panel = LONG_RECORD_PANELS[[1]])
  zoom <- transform(d[d$year_ce >= 1700, ], panel = LONG_RECORD_PANELS[[2]])
  out <- rbind(full, zoom)
  out$panel <- factor(out$panel, levels = LONG_RECORD_PANELS)
  out
}

# Each series is either an ice core record or an air monitoring site. Seven
# monitoring sites in Figure 1 would run past the palette's six slots, and the
# comparison that matters is ice core against direct measurement, so colour
# follows that split and the tooltip names the individual series. Role order:
# direct measurements are what the Key Points quote, so they take `focus`.
SOURCE_KIND_KEYS <- c("Ice cores", "Monitoring sites")

long_record_plot <- function(d, ice_series, y_title, unit, digits = 1) {
  d <- as_numeric_time(d)
  d$kind <- factor(ifelse(d$series %in% ice_series, "Ice cores", "Monitoring sites"),
                   levels = SOURCE_KIND_KEYS)
  d <- with_zoom_panels(d)

  ggplot(d, aes(x = year_ce, y = value, group = series, colour = kind)) +
    geom_line_interactive(
      aes(
        data_id = series,
        tooltip = sprintf("%s\n%s: %s %s", series, year_label(year_ce),
                          formatC(value, format = "f", digits = digits, big.mark = ","), unit)
      ),
      linewidth = 0.6
    ) +
    facet_wrap(~panel, scales = "free_x") +
    scale_colour_manual(values = series_colours(SOURCE_KIND_KEYS)) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 3),
      labels = function(x) ifelse(x < 0, paste(scales::label_comma()(-x), "BC"), as.character(x))
    ) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = NULL, y = y_title) +
    theme_indicator() +
    legend_top()
}

# ---- Figure 1: carbon dioxide ------------------------------------------------

fig_1_plot <- function(d) {
  long_record_plot(d, ice_series = "Antarctic Ice Cores",
                   y_title = "Carbon dioxide (ppm)", unit = "ppm")
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  d <- as_numeric_time(d)
  tidyr::pivot_wider(d, id_cols = year_ce, names_from = series, values_from = value)
}

# ---- Figure 2: methane -------------------------------------------------------

# EPA publishes two methane readings of exactly 0 (1983 Mauna Loa, 1984 Cape
# Grim). Methane is never zero in the atmosphere, so the chart leaves those two
# points out rather than drawing a line down to the axis; the table beneath it
# still shows them as published.
fig_2_plot <- function(d) {
  long_record_plot(d[d$value > 0, ], ice_series = c("EPICA Dome C, Antarctica", "Law Dome"),
                   y_title = "Methane (ppb)", unit = "ppb", digits = 0)
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d))

fig_2_table <- fig_1_table

# ---- Figure 3: nitrous oxide -------------------------------------------------

fig_3_plot <- function(d) {
  long_record_plot(d, ice_series = "EPICA Dome C, Antarctica",
                   y_title = "Nitrous oxide (ppb)", unit = "ppb")
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d))

fig_3_table <- fig_1_table

# ---- Figure 4: halogenated gases ---------------------------------------------

# Thirteen gases whose concentrations differ by orders of magnitude (EPA's own
# note says its scale increases by factors of 10). Small multiples with a free
# y axis per gas keep every line readable, and no palette slot is spent
# telling gases apart, so all panels draw in the one baseline colour.
fig_4_plot <- function(d) {
  d <- as_numeric_time(d)
  d$series <- factor(d$series, levels = unique(d$series))

  ggplot(d, aes(x = year_decimal, y = value, group = series)) +
    geom_line_interactive(
      aes(
        data_id = series,
        tooltip = sprintf("%s, %.1f\n%s ppt", series, year_decimal,
                          formatC(value, format = "f", digits = 2, big.mark = ","))
      ),
      colour = INDICATOR_PALETTE[["base"]], linewidth = 0.6
    ) +
    facet_wrap(~series, ncol = 3, scales = "free_y") +
    scale_x_continuous(breaks = c(1980, 2000, 2020)) +
    scale_y_continuous(limits = c(0, NA), labels = scales::label_comma(),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = "Concentration (ppt)") +
    theme_indicator()
}

fig_4 <- function(d) girafe_indicator(fig_4_plot(d), height = 9)

fig_4_table <- function(d) {
  d <- as_numeric_time(d)
  tidyr::pivot_wider(d, id_cols = year_decimal, names_from = series, values_from = value)
}

# ---- Figure 5: ozone ---------------------------------------------------------

# Roles: the total column is the series the other two sum to, the troposphere
# is the layer the Key Points say increased (about 12 percent) and the
# stratosphere is the layer that fell (all of the roughly 4 percent decline in
# the total).
FIG5_COLOUR_KEYS <- c("Total column", "Troposphere", "Stratosphere")

fig_5_plot <- function(d) {
  d$series <- sub("^Total column: SBUV$", "Total column", d$series)
  # Only the total column exists for 1994 to 1996. Completing the grid gives the
  # other two layers an explicit gap there instead of a line drawn straight across.
  d <- tidyr::complete(d, year, series)
  d$series <- factor(d$series, levels = FIG5_COLOUR_KEYS)

  ggplot(d, aes(x = year, y = value, group = series, colour = series)) +
    geom_line_interactive(
      aes(data_id = series, tooltip = ifelse(is.na(value), NA_character_,
                                             sprintf("%d, %s\n%.1f Dobson units", year, series, value))),
      linewidth = 0.8
    ) +
    scale_colour_manual(values = series_colours(FIG5_COLOUR_KEYS)) +
    scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Ozone (Dobson units)") +
    theme_indicator() +
    legend_top()
}

fig_5 <- function(d) girafe_indicator(fig_5_plot(d))

fig_5_table <- function(d) {
  tidyr::pivot_wider(d, id_cols = year, names_from = series, values_from = value)
}
