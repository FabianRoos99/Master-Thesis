# ============================================================
# Script overview: Web scraping pipeline
# ============================================================
# This master script documents the web scraping pipeline used
# to collect Spotify chart and playlist data from Spotontrack.
#
# It sequentially executes four separate scraping scripts:
#   1. Web-Scraping_Global_Playlists_2017.R
#   2. Web-Scraping_Global_Playlists_2024.R
#   3. Web-Scraping_New_Music_Friday_2017.R
#   4. Web-Scraping_New_Music_Friday_2024.R
#
# The scripts were originally written to scrape daily Spotify
# chart and playlist snapshots for selected countries and years.
# They collect track-level information such as chart or playlist
# position, title, artist, Spotontrack song ID, date, country,
# and — where available — stream counts.
#
# In addition, the scripts enrich the scraped data with ISRC-based
# metadata, including inferred country of origin, label category,
# release year, add dates, and drop dates. The pipeline covers
# global Spotify charts, selected major editorial playlists
# such as Today's Top Hits, RapCaviar, Viva Latino, Baila Reggaeton,
# and country-specific New Music Friday playlists.
#
# Important reproducibility note:
# As of August 2025, this code was still functional with the
# Spotontrack website structure available at that time. However,
# Spotontrack has since changed its website structure and/or
# access patterns. As a result, the scraping scripts cannot be
# executed successfully in their current form without updating
# the affected URLs, HTML selectors, login workflow, or related
# scraping logic.
#
# Requirements:
# - A valid Spotontrack account was required when the scripts were
#   written.
# - Login credentials were expected to be stored locally in a
#   separate credentials.R file, which should not be committed
#   to GitHub.
# - The credentials.R file should define:
#       SPOTONTRACK_EMAIL
#       SPOTONTRACK_PASSWORD
#
# Output:
# When functional, the pipeline saved the complete R environment as:
#       all_data.RData
#
# Please note:
# This code is provided primarily for transparency, documentation,
# and reproducibility of the original data collection process.
# It should be treated as an archival version of the scraping
# workflow rather than a currently executable data collection tool.
# Users should also make sure that any future adaptation of the code
# complies with Spotontrack's terms of service and applicable data
# access rules.
# ============================================================


scripts <- c(
  "Web-Scraping_Global_Playlists_2017.R",
  "Web-Scraping_Global_Playlists_2024.R",
  "Web-Scraping_New_Music_Friday_2017.R",
  "Web-Scraping_New_Music_Friday_2024.R"
)

for (s in scripts) {
  message("Start: ", s)
  source(s)
  message("Done: ", s)
}

save.image(file = "all_data.RData")