# Victorian Waterways — Gippsland Lakes and Western Port

An R Shiny coursework prototype by Grace Meghna Didla for FIT5147 Data Exploration and Visualisation, Monash University.

## Purpose

Explore water-quality measurements across Gippsland Lakes and Western Port using interactive time-series charts, seasonal comparisons and a narrative interface.

## Important data distinctions

- **Water-quality charts:** use the supplied cleaned coursework CSV (2,877 rows, years 1990–2025). Aggregations calculate annual means by region.
- **Vegetation plots:** use generated example values, with a fixed random seed added for reproducibility. They do not establish environmental correlations.
- **Site map:** uses hard-coded illustrative locations and summary statistics, rather than statistics calculated from the CSV. Its year control is not connected to the map.
- **Narrative claims:** retain original coursework explanations, fixed statistics and event annotations. These have not been independently validated and must not be treated as confirmed causal or statistical findings.

The app displays this distinction prominently. This is a visualisation prototype, not an operational monitoring product or environmental assessment.

## Features

- Indicator and year-range controls for annual comparisons.
- Seasonal boxplots for chlorophyll-a and nitrogen.
- Plotly charts, a Leaflet map and chapter-based navigation.
- R data aggregation with dplyr and readr.

## Run locally

Install R, then install the dependencies:

```r
install.packages(c("shiny", "ggplot2", "plotly", "leaflet", "tidyr", "readr", "dplyr"))
```

Open this repository directory in R and run:

```r
shiny::runApp(".")
```

Keep `app.R` and `water_quality_clean.csv` together. External map tiles and fonts need an internet connection. GitHub displays the source; it does not run a Shiny server.

## Files

- `app.R`: application UI, charts, aggregation and server.
- `water_quality_clean.csv`: cleaned water-quality data supplied with the local coursework project.

## Provenance and reuse

The CSV is copied unchanged from the coursework project. Source-provider attribution and redistribution terms have not yet been verified from the original download records. This repository does not grant a licence for third-party data. Do not use it for consequential environmental decisions.

## Portfolio preparation

The coursework application is preserved with a prominent limitations notice, explicit illustrative chart titles, a fixed simulation seed, and a relative CSV path. Student-number reports, assignment instructions and unrelated drafts are excluded.

## Next improvements

Derive map statistics from the measured data, replace simulated vegetation values with verified observations, compute narrative statistics dynamically, document original data sources and units, and connect or remove the map year control.

## Author

[Grace Didla](https://github.com/graced02)
