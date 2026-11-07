# climateindicators.us

 Source files for the main climateindicators.us website, built using
[Quarto](https://quarto.org/) and deployed to GitHub Pages via GitHub Actions.

Early replication of the snapshot at:

https://19january2025snapshot.epa.gov/climate-indicators/view-indicators/index.html

## Local development

```sh
quarto preview   # live-reload dev server on port 5679
quarto render    # build the full site into _site/
```

## Structure

- `_quarto.yml` — site config: navbar, sidebar, theme, render options
- `index.qmd` — home page
- `about.qmd` — about the project
- `indicators.qmd` — listing page for all indicators
- `indicators/<category>/` — one folder per category: its landing page plus one page per indicator
- `chunks/` — reusable content snippets pulled in with `{{< include >}}`
- `css/theme.scss` — custom theme (single `$accent` variable drives the palette)
- `images/` — static assets

## Deployment

Pushes to `main` trigger `.github/workflows/main.yml`, which renders the site
and publishes `_site/` to the `gh-pages` branch. See `CLAUDE.md` for the
one-time `gh-pages` branch and GitHub Pages setup steps.
