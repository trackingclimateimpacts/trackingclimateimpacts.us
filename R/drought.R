# Figures for indicators/weather-and-extreme-events/drought.qmd.

REPO <- "drought"

fmt <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)

# ---- Figure 1: Palmer Drought Severity Index ---------------------------------

# Role order: the annual series is the one everything else is read against, the
# nine-year weighted average is the line EPA asks the reader to follow.
FIG1_KEYS <- c("annual", "nine_year")

# EPA draws the nine-year average as the thicker line, so width carries the
# distinction between the two series and colour only names them.
FIG1_WIDTHS <- c(annual = 0.45, nine_year = 1.3)

fig_1_plot <- function(d) {
  colours <- label_colours(d, "series", "series_label", FIG1_KEYS)
  levels  <- label_order(d, "series", "series_label", FIG1_KEYS)

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
    scale_linewidth_manual(values = FIG1_WIDTHS, guide = "none") +
    scale_x_continuous(breaks = scales::breaks_width(20)) +
    labs(x = NULL, y = "Palmer Drought Severity Index") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  w <- tidyr::pivot_wider(
    d[c("year", "series_label", "value")],
    names_from = "series_label", values_from = "value"
  )
  w[-1] <- lapply(w[-1], fmt, digits = 3)
  names(w)[1] <- "Year"
  as.data.frame(w, check.names = FALSE)
}

# ---- Figure 2: five-year SPEI ------------------------------------------------

fig_2_plot <- function(d) {
  d$tooltip <- sprintf("Five-year SPEI\n%d-%d\n%s to %s",
                       min(d$year), max(d$year), fmt(min(d$value)), fmt(max(d$value)))

  ggplot(d, aes(x = year, y = value)) +
    geom_hline(yintercept = 0, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_line_interactive(
      aes(data_id = "spei", tooltip = tooltip),
      colour = INDICATOR_PALETTE[["base"]], linewidth = 1.1
    ) +
    scale_x_continuous(breaks = scales::breaks_width(20)) +
    labs(x = NULL, y = "Five-year SPEI") +
    theme_indicator()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d))

fig_2_table <- function(d) {
  data.frame(
    Year                   = d$year,
    `Five-year SPEI value` = fmt(d$value, 3),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ---- Figure 3: change in five-year SPEI, by climate division -----------------

# EPA draws this as a choropleth of the 344 NOAA climate divisions. No
# climate-division geometry ships with any package this site depends on, and the
# site holds no data of its own, so the divisions are drawn as points on a value
# axis and grouped by state instead. Every division stays visible and nothing is
# averaged away.

# EPA's caption tells the reader that blue marks increased moisture and brown
# marks decreased moisture, so the two roles here are picked for hue rather than
# for their usual base/focus meaning: `base` is the palette's blue and `focus`
# its warmest slot.
FIG3_KEYS   <- c("wetter", "drier")
FIG3_LABELS <- c(wetter = "Increased moisture", drier = "Decreased moisture")

fig_3_plot <- function(d) {
  d$direction <- ifelse(d$value >= 0, "wetter", "drier")
  d$state     <- state.name[match(d$state, state.abb)]

  # States read wettest at the top down to driest at the bottom, so the western
  # block EPA's Key Points describe gathers at one end of the axis instead of
  # scattering through an alphabetical list.
  d$state <- factor(d$state, levels = names(sort(tapply(d$value, d$state, stats::median))))

  d$tooltip <- sprintf("%s, %s\n%s change in five-year SPEI",
                       d$division, d$state, fmt(d$value, 3))

  ggplot(d, aes(x = value, y = state)) +
    geom_vline(xintercept = 0, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_point_interactive(
      aes(fill = direction, data_id = climate_division, tooltip = tooltip),
      shape = 21, size = 2.1, stroke = 0.35,
      colour = CHART_GREY[["surface"]], alpha = 0.9
    ) +
    scale_fill_manual(values = series_colours(FIG3_KEYS),
                      breaks = FIG3_KEYS, labels = FIG3_LABELS) +
    labs(x = "Change in five-year SPEI, 1900 to 2023", y = NULL) +
    theme_indicator() +
    theme(panel.grid.major.x = element_line(colour = CHART_GREY[["grid"]], linewidth = 0.4)) +
    legend_top()
}

fig_3 <- function(d) girafe_indicator(fig_3_plot(d), height = 8.6)

# Driest division first, which is the order the chart reads top to bottom.
fig_3_table <- function(d) {
  d <- d[order(d$value), ]
  data.frame(
    `Climate division`               = d$division,
    State                            = d$state,
    `Change in five-year SPEI value` = fmt(d$value, 3),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ---- Figure 4: U.S. land area under drought ----------------------------------

FIG4_KEYS <- c("D0", "D1", "D2", "D3", "D4")

# Five ordered severity classes. INDICATOR_PALETTE is categorical, so giving
# each class its own role would draw an ordinal scale as five unrelated things;
# the fill scale interpolates across three named roles instead, which keeps
# every colour coming from common.R and still re-themes with it. Luminance falls
# monotonically from D0 to D4, so severity reads as darkness.
fig_4_colours <- function() {
  stats::setNames(
    grDevices::colorRampPalette(c(
      INDICATOR_PALETTE[["extra"]], INDICATOR_PALETTE[["focus"]], INDICATOR_PALETTE[["other"]]
    ))(length(FIG4_KEYS)),
    FIG4_KEYS
  )
}

fig_4_plot <- function(d) {
  labels <- stats::setNames(d$category_label[match(FIG4_KEYS, d$category)], FIG4_KEYS)

  peak <- tapply(seq_len(nrow(d)), d$category, function(i) i[which.max(d$value[i])])
  d$tooltip <- sprintf("%s\npeaked at %s%% of U.S. land on %s",
                       labels[d$category],
                       fmt(d$value[peak[d$category]]),
                       format(d$date[peak[d$category]], "%Y-%m-%d"))

  d$category <- factor(d$category, levels = FIG4_KEYS)

  ggplot(d, aes(x = date, y = value, fill = category)) +
    geom_area_interactive(
      aes(data_id = category, tooltip = tooltip),
      position = position_stack(reverse = TRUE), linewidth = 0
    ) +
    scale_fill_manual(values = fig_4_colours(), labels = labels) +
    scale_x_date(breaks = seq(as.Date("2000-01-01"), max(d$date), by = "4 years"),
                 date_labels = "%Y", expand = expansion(mult = 0.01)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       expand = expansion(mult = c(0, 0.05))) +
    # Five class names do not fit on one row at the width the page renders at.
    guides(fill = guide_legend(nrow = 2)) +
    labs(x = NULL, y = "Percent of U.S. land area") +
    theme_indicator() +
    legend_top()
}

fig_4 <- function(d) girafe_indicator(fig_4_plot(d), height = 4.6)

fig_4_table <- function(d) {
  w <- tidyr::pivot_wider(
    d[c("date", "category_label", "value")],
    names_from = "category_label", values_from = "value"
  )
  w <- w[order(w$date), ]
  w[-1] <- lapply(w[-1], fmt, digits = 2)
  names(w)[1] <- "Date"
  as.data.frame(w, check.names = FALSE)
}
