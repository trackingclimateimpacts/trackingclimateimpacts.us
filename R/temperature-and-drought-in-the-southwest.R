# Figures for indicators/weather-and-extreme-events/temperature-and-drought-in-the-southwest.qmd.

REPO <- "temperature-and-drought-in-the-southwest"

fmt <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)

# ---- Figure 1: temperature departure by climate division ---------------------

# EPA draws this as a map of the region's NOAA climate divisions. No
# climate-division geometry ships with any package this site depends on (see
# drought.R's Figure 3 for the same constraint), so divisions are drawn as
# bars instead, one per division, grouped and coloured by state so a reader
# can compare states at a glance without losing the within-state spread EPA's
# map shows. Six states, six INDICATOR_PALETTE slots, none spare; states are
# ordered alphabetically since nothing in the narrative singles one out.
FIG1_STATE_ORDER <- c("Arizona", "California", "Colorado", "Nevada", "New Mexico", "Utah")

fig_1_plot <- function(d) {
  d$state <- factor(d$state, levels = FIG1_STATE_ORDER)
  d$tooltip <- sprintf("%s, division %s\n+%s°F versus 1895-2023 average", d$state, d$division, fmt(d$value))

  # Group divisions by state, warmest division first within each group (the
  # same order fig_1_table already uses for the whole table, applied one
  # state at a time). A discrete y axis draws its first factor level at the
  # bottom, so the levels are built in reverse row order to read top to
  # bottom in the same state order as the legend.
  d <- d[order(d$state, -d$value), ]
  d$division_key <- factor(seq_len(nrow(d)), levels = rev(seq_len(nrow(d))))
  division_labels <- stats::setNames(d$division, as.character(d$division_key))

  ggplot(d, aes(x = value, y = division_key, fill = state)) +
    geom_vline(xintercept = 0, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_col_interactive(
      aes(data_id = climate_division_id, tooltip = tooltip)
    ) +
    scale_y_discrete(labels = division_labels) +
    scale_fill_manual(values = series_colours(FIG1_STATE_ORDER), breaks = FIG1_STATE_ORDER) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = "Departure from 1895-2023 average temperature (°F)", y = NULL) +
    theme_indicator() +
    theme(panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4)) +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 8.5)

# Warmest division first, which is the order the chart reads top to bottom.
fig_1_table <- function(d) {
  d <- d[order(-d$value), ]
  data.frame(
    State                          = d$state,
    Division                       = d$division,
    `Temperature departure (°F)` = fmt(d$value, 3),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ---- Figure 2: land area under drought conditions -----------------------------

FIG2_KEYS <- c("D0", "D1", "D2", "D3", "D4")

# Five ordered severity classes, matching the national Drought indicator's
# Figure 4 of the same shape: the fill scale interpolates across three named
# roles so severity reads as darkness while every colour still comes from
# common.R.
fig_2_colours <- function() {
  stats::setNames(
    grDevices::colorRampPalette(c(
      INDICATOR_PALETTE[["extra"]], INDICATOR_PALETTE[["focus"]], INDICATOR_PALETTE[["other"]]
    ))(length(FIG2_KEYS)),
    FIG2_KEYS
  )
}

fig_2_plot <- function(d) {
  labels <- stats::setNames(d$category_label[match(FIG2_KEYS, d$category)], FIG2_KEYS)

  peak <- tapply(seq_len(nrow(d)), d$category, function(i) i[which.max(d$value[i])])
  d$tooltip <- sprintf("%s\npeaked at %s%% of Southwest land on %s",
                       labels[d$category],
                       fmt(d$value[peak[d$category]]),
                       format(d$date[peak[d$category]], "%Y-%m-%d"))

  d$category <- factor(d$category, levels = FIG2_KEYS)

  ggplot(d, aes(x = date, y = value, fill = category)) +
    geom_area_interactive(
      aes(data_id = category, tooltip = tooltip),
      position = position_stack(reverse = TRUE), linewidth = 0
    ) +
    scale_fill_manual(values = fig_2_colours(), labels = labels) +
    scale_x_date(breaks = seq(as.Date("2000-01-01"), max(d$date), by = "4 years"),
                 date_labels = "%Y", expand = expansion(mult = 0.01)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       expand = expansion(mult = c(0, 0.05))) +
    # Five class names do not fit on one row at the width the page renders at.
    guides(fill = guide_legend(nrow = 2)) +
    labs(x = NULL, y = "Percent of land area") +
    theme_indicator() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 4.6)

fig_2_table <- function(d) {
  w <- tidyr::pivot_wider(
    d[c("date", "category_label", "value")],
    names_from = "category_label", values_from = "value"
  )
  w <- w[order(w$date), ]
  w[-1] <- lapply(w[-1], fmt, digits = 2)
  names(w)[1] <- "Date"
  as.data.frame(w, check.names = FALSE)
}

# ---- Figure 3: Palmer Drought Severity Index ----------------------------------

# Role order: the annual series is the one everything else is read against, the
# nine-year weighted average is the line EPA asks the reader to follow.
FIG3_KEYS <- c("annual", "nine_year")

# EPA draws the nine-year average as the thicker line, so width carries the
# distinction between the two series and colour only names them.
FIG3_WIDTHS <- c(annual = 0.45, nine_year = 1.3)

fig_3_plot <- function(d) {
  colours <- label_colours(d, "series", "series_label", FIG3_KEYS)
  levels  <- label_order(d, "series", "series_label", FIG3_KEYS)

  span <- tapply(d$value, d$series, function(v) sprintf("%s to %s", fmt(min(v)), fmt(max(v))))
  d$tooltip      <- sprintf("%s\n%d-%d\n%s", d$series_label, min(d$year), max(d$year), span[d$series])
  d$series_label <- factor(d$series_label, levels = levels)

  ggplot(d, aes(x = year, y = value)) +
    # Zero is the 1931-1990 average, which is what makes a value wet or dry.
    geom_hline(yintercept = 0, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_line_interactive(
      aes(colour = series_label, linewidth = series,
          data_id = series, tooltip = tooltip)
    ) +
    scale_colour_manual(values = colours) +
    scale_linewidth_manual(values = FIG3_WIDTHS, guide = "none") +
    scale_x_continuous(breaks = scales::breaks_width(20)) +
    labs(x = NULL, y = "Palmer Drought Severity Index") +
    theme_indicator() +
    legend_top()
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d))

fig_3_table <- function(d) {
  w <- tidyr::pivot_wider(
    d[c("year", "series_label", "value")],
    names_from = "series_label", values_from = "value"
  )
  w[-1] <- lapply(w[-1], fmt, digits = 3)
  names(w)[1] <- "Year"
  as.data.frame(w, check.names = FALSE)
}
