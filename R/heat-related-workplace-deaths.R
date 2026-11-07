# Figures for indicators/temperature-related-health/heat-related-workplace-deaths.qmd.

REPO <- "heat-related-workplace-deaths"

# ---- Figure 1: annual heat-related workplace deaths, by sector ---------------

FIG1_LABELS <- c(
  construction   = "Construction",
  other_sectors  = "All other industry sectors",
  all_industries = "All industries (sector breakdown not available)"
)

# Three bar shapes coexist on one year axis, driven entirely by which cells are
# populated in the source data (see its `flag`/`note` columns, taken from EPA's
# own figure caption):
#   - an ordinary year stacks two segments: construction, other_sectors.
#   - 2019 draws ONE segment (all_industries): BLS combined construction into
#     the all-industry total that year and never published a sector split, so
#     there is a real total (43) but no honest way to split it.
#   - 2020 draws NOTHING: BLS's own data collection fell below its reporting
#     threshold that year and published no total at all.
fig_1_plot <- function(d) {
  wide <- tidyr::pivot_wider(d, id_cols = year, names_from = series_key, values_from = value)

  split_d <- wide |>
    dplyr::filter(!is.na(construction), !is.na(other_sectors)) |>
    dplyr::select(year, construction, other_sectors) |>
    tidyr::pivot_longer(-year, names_to = "series_key", values_to = "value")
  merged_d <- wide |>
    dplyr::filter(is.na(construction), is.na(other_sectors), !is.na(all_industries)) |>
    dplyr::transmute(year, series_key = "all_industries", value = all_industries)
  bar_d <- dplyr::bind_rows(split_d, merged_d)

  # Stack order, top segment first. It is not the colour order below: every
  # other sector combined is the baseline, but construction is what the Key
  # Points single out (34% of all deaths from 6% of the workforce), so it takes
  # the attention colour while sitting on top of the bar.
  order <- c("construction", "other_sectors", "all_industries")
  bar_d$series_key <- factor(bar_d$series_key, levels = order)

  # `all_industries` only ever renders for a year where the sector split itself
  # is unavailable (2019), so the third slot marks it as a different kind of
  # bar rather than a third peer series.
  colour_keys <- c("other_sectors", "construction", "all_industries")

  ggplot(bar_d, aes(x = year, y = value, fill = series_key)) +
    geom_col_interactive(
      aes(
        data_id = series_key,
        tooltip = sprintf(
          "%d — %s\n%d deaths",
          year, FIG1_LABELS[as.character(series_key)], as.integer(value)
        )
      ),
      position = "stack", width = 0.75
    ) +
    scale_fill_manual(
      values = series_colours(colour_keys),
      breaks = order, labels = FIG1_LABELS[order]
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(1992, 2022, 5)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
    labs(x = NULL, y = "Number of deaths") +
    theme_indicator() +
    legend_top()
}

fig_1 <- function(d) girafe_indicator(fig_1_plot(d))

fig_1_table <- function(d) {
  labels <- c(
    all_industries = "All industries",
    construction   = "Construction",
    other_sectors  = "All other industry sectors"
  )
  d$series_label <- labels[d$series_key]
  tidyr::pivot_wider(d, id_cols = year, names_from = series_label, values_from = value)
}

# Example 1 (the county-level outdoor-workers map) has no figure function.
# Upstream it is EPA's own published map image, and its area-level CSV is a
# download rather than a rendered table: 4,426 rows, too many to render
# inline as a table.
