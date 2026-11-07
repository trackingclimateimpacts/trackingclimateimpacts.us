# Figures for indicators/coastal-and-marine/marine-heat-waves.qmd. Figures 1 and 2 are drawn here
# from EPA's published data. Figures 3 and 4 are maps EPA published without
# data, so the page shows EPA's images and has no code for them.

REPO <- "marine-heat-waves"

# ---- Severity categories, shared by Figures 1 and 2 --------------------------

SEVERITY_KEYS <- c("Moderate", "Strong", "Severe", "Extreme")

# Four ordered categories, interpolated across three named roles the same way
# the drought page colours its classes, so severity reads as darkness.
severity_colours <- function() {
  stats::setNames(
    grDevices::colorRampPalette(c(
      INDICATOR_PALETTE[["extra"]], INDICATOR_PALETTE[["focus"]], INDICATOR_PALETTE[["other"]]
    ))(length(SEVERITY_KEYS)),
    SEVERITY_KEYS
  )
}

# ---- Figure 1: area affected by marine heat waves, by region -----------------

# Regions in EPA's column order: west to east along the contiguous coast, then
# Alaska and Hawaii.
FIG1_REGIONS <- c("West Coast", "Gulf of Mexico", "Southeast", "Northeast", "Alaska", "Hawaii")

# Each location counts once, under its most severe event of the year, so the
# categories are disjoint and the stack height is the area with any heat wave.
fig_1_plot <- function(d) {
  d$region   <- factor(d$region, levels = FIG1_REGIONS)
  d$category <- factor(d$category, levels = SEVERITY_KEYS)

  ggplot(d, aes(x = year, y = value, fill = category)) +
    geom_col_interactive(
      aes(
        data_id = paste(region, year),
        tooltip = sprintf("%s, %d\n%s: %.1f%% of area", region, year, category, value)
      ),
      position = position_stack(reverse = TRUE), width = 0.85
    ) +
    facet_wrap(~region, ncol = 3) +
    scale_fill_manual(values = severity_colours()) +
    scale_x_continuous(breaks = seq(1985, 2020, 10)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       breaks = seq(0, 100, 25),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "Percent of region's area") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 5.4)

fig_1_table <- function(d) {
  w <- tidyr::pivot_wider(d[c("year", "region", "category", "value")],
                          names_from = "category", values_from = "value")
  w <- w[order(w$year, match(w$region, FIG1_REGIONS)), ]
  data.frame(
    Year   = w$year,
    Region = w$region,
    stats::setNames(lapply(w[SEVERITY_KEYS], function(x) sprintf("%.1f", x)),
                    paste0(SEVERITY_KEYS, " (%)")),
    check.names = FALSE
  )
}

# ---- Figure 2: event timelines at five marine protected areas ----------------

# Upstream carries duration and intensity as two rows per event.
fig_2_events <- function(d) {
  w <- tidyr::pivot_wider(
    d[c("marine_protected_area", "event_no", "date_start", "date_end", "severity", "measure", "value")],
    names_from = "measure", values_from = "value"
  )
  w$date_start <- as.Date(w$date_start)
  w$date_end   <- as.Date(w$date_end)
  # EPA writes severity as "I Moderate" ... "IV Extreme"; the class name alone
  # keys the same colours Figure 1 uses.
  w$category   <- factor(sub("^[IV]+ ", "", w$severity), levels = SEVERITY_KEYS)
  w$marine_protected_area <- factor(w$marine_protected_area, levels = unique(d$marine_protected_area))
  w
}

# Each event is a column whose width spans its start and end dates and whose
# height is its mean intensity. Most events last under two weeks, a sliver on
# a 42-year axis, so each column is outlined in its own fill to stay visible.
fig_2_plot <- function(d) {
  w <- fig_2_events(d)

  ggplot(w) +
    geom_rect_interactive(
      aes(
        xmin = date_start, xmax = date_end + 1, ymin = 0, ymax = intensity_mean,
        fill = category, colour = category,
        data_id = paste(marine_protected_area, event_no),
        tooltip = sprintf("%s\n%s to %s (%d days)\nMean intensity: %.2f°F\nSeverity: %s",
                          marine_protected_area, format(date_start), format(date_end),
                          as.integer(duration), intensity_mean, category)
      ),
      linewidth = 0.3
    ) +
    facet_wrap(~marine_protected_area, ncol = 1) +
    scale_fill_manual(values = severity_colours(), drop = FALSE) +
    scale_colour_manual(values = severity_colours(), drop = FALSE, guide = "none") +
    scale_x_date(breaks = seq(as.Date("1985-01-01"), as.Date("2020-01-01"), by = "5 years"),
                 date_labels = "%Y", limits = as.Date(c("1982-01-01", "2024-01-01")),
                 expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Mean intensity (°F above threshold)") +
    theme_indicator() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d), height = 7.5)

fig_2_table <- function(d) {
  w <- fig_2_events(d)
  data.frame(
    `Marine protected area` = as.character(w$marine_protected_area),
    Start                   = format(w$date_start),
    End                     = format(w$date_end),
    `Duration (days)`       = as.integer(w$duration),
    `Mean intensity (°F)`   = sprintf("%.2f", w$intensity_mean),
    Severity                = as.character(w$category),
    check.names = FALSE
  )
}
