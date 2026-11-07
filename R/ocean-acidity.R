# Figures for indicators/coastal-and-marine/ocean-acidity.qmd.

REPO <- "ocean-acidity"

# ---- Figure 1: ocean carbon dioxide levels and acidity, 1983-2022 ------------

# Upstream source order. Every station is a peer, so they take the first four
# palette slots in that order.
FIG1_KEYS <- c("Hawaii", "Canary Islands", "Bermuda", "Cariaco")

# pCO2 in the left column, pH in the right, as in EPA's figure.
FIG1_MEASURES <- c(
  pCO2 = "carbon dioxide (pCO₂, µatm)",
  pH   = "pH (lower is more acidic)"
)

# One station per row, so each line gets a panel of its own instead of four
# seasonal sawtooths overplotting on shared axes.
fig_1_plot <- function(d) {
  d$decimal_year <- as.numeric(d$decimal_year)
  d$station      <- factor(d$station, levels = FIG1_KEYS)
  panels <- outer(FIG1_KEYS, FIG1_MEASURES, paste, sep = ", ")
  d$panel <- factor(
    paste(d$station, FIG1_MEASURES[d$measure], sep = ", "),
    levels = as.vector(t(panels))
  )

  # facet_wrap can only free y per panel; these invisible points widen every
  # panel in a column to that measure's full range, so stations stay comparable.
  ranges <- do.call(rbind, lapply(names(FIG1_MEASURES), function(m) {
    v <- range(d$value[d$measure == m], na.rm = TRUE)
    data.frame(panel = panels[, m], value = rep(v, each = length(FIG1_KEYS)))
  }))
  ranges$panel <- factor(ranges$panel, levels = levels(d$panel))
  ranges$decimal_year <- min(d$decimal_year)

  ggplot(d, aes(x = decimal_year, y = value)) +
    geom_blank(data = ranges) +
    geom_line_interactive(
      aes(colour = station, group = station, data_id = station, tooltip = station),
      linewidth = 0.45
    ) +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = series_colours(FIG1_KEYS)) +
    scale_x_continuous(breaks = seq(1985, 2020, by = 5)) +
    labs(x = NULL, y = NULL) +
    theme_indicator(base_size = 11) +
    theme(panel.spacing.y = unit(1, "lines"), panel.spacing.x = unit(1.5, "lines"))
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d), height = 8)

# Stations sample on their own dates, so there is no shared row key to pivot on.
fig_1_table <- function(d) {
  data.frame(
    Station        = d$station,
    `Decimal year` = d$decimal_year,
    Measure        = d$measure,
    Value          = as.character(d$value),
    Unit           = d$unit,
    check.names = FALSE
  )
}
