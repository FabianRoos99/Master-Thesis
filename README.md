# Master Thesis

**“The Power of Playlists: How Spotify’s Recommendations Shape Music Success”**

This repository contains the R code used for my master thesis on how Spotify playlists and recommendations shape music success. The project combines web scraping, descriptive analysis, event studies, regression discontinuity-style analyses, and fixed-effects regressions in R.

## Project Overview

The thesis analyzes Spotify chart and playlist data from **2017** and **2024**. It focuses on how playlist placements are related to streaming outcomes and music visibility.

The analysis covers:

- Global and country-level Spotify charts
- Major editorial playlists such as *Today’s Top Hits*, *RapCaviar*, *Viva Latino*, and *Baila Reggaeton*
- Country-specific *New Music Friday* playlists
- Global Top 50 / Top 200 chart dynamics
- Differences between domestic, international, major-label, and independent tracks

## Repository Structure

```text
.
├── Web-Scraping_Run_All.R
├── Web-Scraping_Global_Playlists_2017.R
├── Web-Scraping_Global_Playlists_2024.R
├── Web-Scraping_New_Music_Friday_2017.R
├── Web-Scraping_New_Music_Friday_2024.R
├── Playlist_Analysis_Master-Thesis.R
├── .gitignore
└── README.md
```

## Data Collection

The web scraping scripts were used to collect Spotify chart and playlist data from Spotontrack. They extract track-level information such as:

- playlist and chart positions
- track titles and artists
- dates and countries
- stream counts
- Spotontrack song IDs
- ISRC codes
- add dates and drop dates
- inferred country of origin, label category, and release year

## Important Reproducibility Note

The scraping scripts were functional as of **August 2025**. Since then, Spotontrack has changed its website structure and/or access patterns. As a result, the scraping code is currently **not directly executable without modifications**.

The scraping scripts are included to document the original data collection workflow. They should be understood as an archival version of the data collection process rather than as a currently functional scraper.

## Data Availability

The dataset used for the analysis, `all_data.RData`, is **not included** in this repository.

The data was originally collected from Spotontrack for academic research purposes. Since the dataset contains data obtained from a third-party platform, it is not redistributed publicly in this repository.

To run the analysis, the archived dataset must be available locally in the project root directory:

```text
all_data.RData
```

## Analysis

The main analysis script is:

```r
Playlist_Analysis_Master-Thesis.R
```

It loads `all_data.RData` and produces the tables and figures used in the thesis, including:

- descriptive playlist statistics
- streaming comparisons between 2017 and 2024
- event-study plots around playlist additions and removals
- Global Top 50 / Top 200 rank analyses
- New Music Friday rank-effect analyses
- fixed-effects regression tables

## How to Run the Analysis

Place the archived dataset `all_data.RData` in the project root directory and run:

```r
source("Playlist_Analysis_Master-Thesis.R")
```

The analysis assumes that `all_data.RData` is available locally. The dataset is not provided in this repository.

## Requirements

The analysis was written in R and uses packages including:

```r
tidyverse
fixest
ggplot2
patchwork
flextable
lubridate
modelsummary
kableExtra
webshot2
scales
```

The original scraping scripts also required a Spotontrack account and a local `credentials.R` file containing login credentials. This file should never be committed to GitHub.

## Status

This repository is intended as an academic research repository. The analysis code documents the empirical workflow and can be run only if the corresponding archived dataset is available locally. The scraping code is included for transparency but may require substantial updates before it can be executed again.
