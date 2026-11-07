# Figures for indicators/coastal-and-marine/atlantic-coast.qmd.

REPO <- "atlantic-coast"

# All three periods start in 1996, so they are cumulative and read left to
# right in the order upstream stores them.
PERIODS <- c("1996-2001", "1996-2006", "1996-2011")

# ---- Figure 1: land loss along the Atlantic coast, 1996-2011 -----------------

# Side by side rather than stacked: the Key Points compare the two regions, and
# the Mid-Atlantic's 1996-2001 value is negative (a net gain), which a stack
# would hide below the other bar. The Southeast lost more, so it takes focus.
FIG1_KEYS <- c("Mid-Atlantic", "Southeast")

fig_1_plot <- function(d) {
  d$period <- factor(d$period, levels = PERIODS)
  d$region <- factor(d$region, levels = FIG1_KEYS)

  ggplot(d, aes(x = period, y = value, fill = region)) +
    geom_hline(yintercept = 0, colour = CHART_GREY[["rule"]], linewidth = 0.4) +
    geom_col_interactive(
      aes(
        data_id = region,
        tooltip = sprintf("%s, %s\n%.2f square miles", region, period, value)
      ),
      position = position_dodge(width = 0.8), width = 0.75
    ) +
    scale_fill_manual(values = series_colours(FIG1_KEYS)) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.06))) +
    labs(x = NULL, y = "Land lost to open water (square miles)") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = period, names_from = region, values_from = value)
  data.frame(
    Period                         = w$period,
    `Mid-Atlantic (square miles)`  = w[["Mid-Atlantic"]],
    `Southeast (square miles)`     = w[["Southeast"]],
    check.names = FALSE
  )
}

# ---- Figure 2: land submergence along the Atlantic coast, 1996-2011 ----------

# Stacked, because the Key Points read this figure as shares of one total ("at
# least half ... has been tidal wetland"). Tidal wetland is that half, so it
# takes focus and sits at the base of the stack where its height reads directly.
FIG2_STACK_ORDER <- c(
  "Dry land --> Open water",
  "Non-tidal wetland --> Open water",
  "Tidal wetland --> Open water"
)
FIG2_COLOUR_KEYS <- c(
  "Dry land --> Open water",
  "Tidal wetland --> Open water",
  "Non-tidal wetland --> Open water"
)

# EPA's "-->" reads as an arrow on the page.
transition_label <- function(x) sub(" --> ", " → ", x, fixed = TRUE)

fig_2_plot <- function(d) {
  d$period     <- factor(d$period, levels = PERIODS)
  d$transition <- factor(d$transition, levels = FIG2_STACK_ORDER)

  ggplot(d, aes(x = period, y = value, fill = transition)) +
    geom_col_interactive(
      aes(
        data_id = transition,
        tooltip = sprintf("%s, %s\n%.2f square miles",
                          transition_label(transition), period, value)
      ),
      position = "stack", width = 0.6
    ) +
    scale_fill_manual(
      values = series_colours(FIG2_COLOUR_KEYS), breaks = FIG2_STACK_ORDER,
      labels = transition_label
    ) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Land converted to open water (square miles)") +
    theme_indicator() +
    legend_top()
}

fig_2 <- function(d) girafe_indicator(fig_2_plot(d))

fig_2_table <- function(d) {
  w <- tidyr::pivot_wider(d, id_cols = period, names_from = transition, values_from = value)
  out <- data.frame(Period = w$period, check.names = FALSE)
  for (k in FIG2_STACK_ORDER) {
    out[[paste0(transition_label(k), " (square miles)")]] <- w[[k]]
  }
  out
}
