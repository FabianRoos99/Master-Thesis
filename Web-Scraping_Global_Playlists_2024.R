# Loading libraries -------------------------------------------------------

library(tidyverse)
library(rvest)
library(httr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)


# Login-data (Spotontrack-Account needed)--------------------------------------------------------------
# Credentials are read from credentials.R (kept out of the repo via .gitignore).
# Template for credentials.R:
#   SPOTONTRACK_EMAIL    <- "your_email@example.com"
#   SPOTONTRACK_PASSWORD <- "your_password"

source("credentials.R")

login_url <- "https://www.spotontrack.com/login"
session <- session(login_url)
login_form <- session %>%
  html_form() %>%
  .[[1]]
filled_form <- html_form_set(login_form,
                             email = SPOTONTRACK_EMAIL,
                             password = SPOTONTRACK_PASSWORD)
session <- session_submit(session, filled_form)


# Relevant timeframe (2023 & 2024) -----------------------------------

months <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "1 month")
months <- format(months, "%Y-%m")


# List of countries for the daily Charts ----------------------------------

countries <- c("br", "ca", "ch", "co", "de", "dk", "es", "fi", "fr", "gb", "hk",
               "id", "is", "it", "mx", "my", "nl", "no", "ph", "pl", "pt", "se",
               "sg", "tr", "tw", "us")


# My functions for Web-Scraping -------------------------------------------

## Extracting Charts-data --------------------------------------------------

extract_data_for_charts <- function(session, date_url, date, country) {
  tryCatch({
    webpage <- session_jump_to(session, date_url)
    
    # extracting positions
    positions <- webpage %>% html_nodes(".position") %>% html_text(trim = TRUE)
    title_artist_nodes <- webpage %>% html_nodes("td.title")
    
    # extracting titles and artists
    titles <- title_artist_nodes %>% html_nodes(".title a") %>% html_text(trim = TRUE)
    artists <- title_artist_nodes %>%
      html_nodes(".artists") %>%
      map_chr(~ paste(html_nodes(.x, "a") %>% html_text(trim = TRUE), collapse = ", "))
    
    # extracting song-ids
    song_ids <- title_artist_nodes %>%
      html_nodes(".title a") %>%
      html_attr("href") %>%
      str_extract("/tracks/\\d+") %>%
      str_replace("/tracks/", "")
    
    # extracting daily streams
    streams <- webpage %>%
      html_nodes(".plays") %>%
      html_text(trim = TRUE) %>%
      gsub("[^0-9,]", "", .) %>%
      str_replace_all(",", "") %>%
      as.numeric()
    
    # checking for data
    min_length <- min(length(positions), length(titles), length(artists), length(song_ids), length(streams))
    if (min_length == 0) {
      message(paste("Keine Daten für das Datum:", date, "und Land:", country))
      return(NULL)
    }
    
    # collecting data in one dataframe
    data <- data.frame(
      position = positions[1:min_length],
      title = titles[1:min_length],
      artist = artists[1:min_length],
      song_id = song_ids[1:min_length],
      date = rep(date, min_length),
      streams = streams[1:min_length],
      country = rep(country, min_length),
      stringsAsFactors = FALSE
    )
    
    return(data)
  }, error = function(e) {
    message(paste("Error for date", date, "and country:", country, ":", e))
    return(NULL)
  })
}

## Extracting Playlist-data ------------------------------------------------

extract_data_for_playlist <- function(session, date_url, date) {
  webpage <- session_jump_to(session, date_url)
  
  # extracting titles
  titles <- webpage %>%
    html_nodes(".title") %>%
    html_text(trim = TRUE)
  
  # extracting artists
  artists <- webpage %>%
    html_nodes(".artists") %>%
    html_text(trim = TRUE) %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all(",\\s+", ", ") %>%
    str_replace_all("\\s*,\\s*", ", ")
  
  # extracting song-ids
  song_ids <- webpage %>%
    html_nodes(".title a") %>%
    html_attr("href") %>%
    str_extract("/tracks/\\d+") %>%
    str_replace("/tracks/", "")
  
  # extracting add-date
  add_dates <- webpage %>%
    html_nodes("td") %>%
    html_text(trim = TRUE) %>%
    str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
    na.omit()
  
  # number of songs per date
  num_songs <- length(titles)
  
  # ranking songs from 1 to number of songs
  positions <- 1:num_songs
  
  # collecting all data in one dataframe
  data <- data.frame(
    position = positions,
    title = titles,
    artist = artists,
    song_id = song_ids,
    add_date = add_dates,
    date = rep(date, num_songs),
    stringsAsFactors = FALSE
  )
  
  return(data)
}

## Extracting ISRC-data ----------------------------------------------------

extract_isrc <- function(song_id, session) {
  isrc_url <- paste0("https://www.spotontrack.com/tracks/", song_id)
  webpage <- session_jump_to(session, isrc_url)
  
  # extracting relevant text
  isrc_text <- webpage %>%
    html_nodes("div.h6.g-font-weight-100.w-100.g-mt-30") %>% 
    html_text(trim = TRUE)
  
  # extracting ISRC
  isrc <- str_extract(isrc_text, "[A-Z]{2}[A-Z0-9]{10}")
  
  # checking for ISRC
  if (length(isrc) == 0 || is.na(isrc) || isrc == "") {
    warning(paste("ISRC not found for song-id:", song_id))
    return(NA)
  } else {
    isrc <- str_trim(isrc)
    return(isrc)
  }
}

# Allocating countries and labels to ISRC-values ----------------------------------------

isrc_country <- c(
  US = "us",
  GB = "gb",
  DE = "de",
  FR = "fr",
  JP = "jp",
  KR = "kr",
  CA = "ca",
  IT = "it",
  AU = "au",
  BR = "br",
  ES = "es",
  SE = "se",
  NL = "nl",
  MX = "mx",
  RU = "ru",
  IN = "in",
  AR = "ar",
  CN = "cn",
  ZA = "za",
  BE = "be",
  FI = "fi",
  NO = "no",
  DK = "dk",
  IE = "ie",
  NZ = "nz",
  PL = "pl",
  CH = "ch",
  AT = "at",
  GR = "gr",
  PT = "pt",
  IL = "il",
  SG = "sg",
  PH = "ph",
  MY = "my",
  ID = "id",
  TR = "tr",
  CL = "cl",
  CO = "co",
  PE = "pe",
  UY = "uy",
  CY = "cy",
  EE = "ee",
  RO = "ro",
  SI = "si",
  LV = "lv",
  IS = "is",
  TW = "tw",
  HK = "hk",
  AE = "ae",
  BG = "bg",
  JM = "jm",
  GM = "gm",
  XX = "xx",
  QZ = "us",
  QM = "us",
  TC = "tc",
  BX = "bx",
  QI = "qi",
  UK = "gb"
)

isrc_label <- c(
  UG1 = "Universal",
  UM7 = "Universal",
  UYG = "Universal",
  C4R = "Universal",
  SYP = "Sony",
  SM1 = "Sony",
  SD1 = "Sony",
  SO1 = "Sony",
  ARL = "Sony",
  US1 = "Sony",
  BGA = "Sony",
  E86 = "Sony",
  HMU = "Sony",
  QX9 = "Sony",
  RC1 = "RCA",
  AT2 = "Atlantic",
  WB1 = "Warner",
  WL1 = "Warner",
  AHS = "Warner",
  AHT = "Warner",
  Z54 = "Warner",
  AYE = "Warner",
  RSZ = "Warner"
)


# Extracting Charts-data ---------------------------------------

## Extracting Charts-data by country ---------------------------------------

Charts <- data.frame()
base_playlist_url <- "https://www.spotontrack.com/charts/spotify/daily/streams/"

# Loop through every country, month and day
for (country in countries) {
  for (month in months) {
    days_in_month <- days_in_month(ymd(paste0(month, "-01")))
    
    for (day in 1:days_in_month) {
      date <- sprintf("%s-%02d", month, day)
      formatted_date <- format(ymd(date), "%d.%m.%Y")
      
      # URL for specific country and date
      date_url <- paste0(base_playlist_url, country, "/", date)
      print(paste("Processing URL:", date_url))
      
      # extracting data for country and date
      daily_data <- extract_data_for_charts(session, date_url, formatted_date, country)
      
      # collecting data in dataframe
      Charts <- bind_rows(Charts, daily_data)
    }
  }
}

Charts$date <- dmy(Charts$date)
Charts$date <- format(Charts$date, "%Y-%m-%d")

# total streams per country for 2024
streams_per_country <- Charts %>%
  filter(substr(date, 1, 4) == "2024") %>%
  group_by(country) %>%
  summarize(total_streams = sum(streams, na.rm = TRUE))

# Extracting global Charts-data -------------------------------------------

Top200_global <- data.frame()
base_playlist_url <- "https://www.spotontrack.com/charts/spotify/daily/streams/global"

# Loop through every month and day
for (month in months) {
  days_in_month <- days_in_month(ymd(paste0(month, "-01")))
  
  for (day in 1:days_in_month) {
    date <- sprintf("%s-%02d", month, day)
    formatted_date <- format(ymd(date), "%d.%m.%Y")
    
    # URL for specific date
    date_url <- paste0(base_playlist_url, "/", date)
    print(paste("Processing URL:", date_url))
    
    # extracting data for specific date
    daily_data <- extract_data_for_charts(session, date_url, formatted_date, "global")
    
    # collecting data in dataframe
    Top200_global <- bind_rows(Top200_global, daily_data)
  }
}

Top200_global$date <- dmy(Top200_global$date)
Top200_global$date <- format(Top200_global$date, "%Y-%m-%d")

# Extracting data for top global playlists  -----------------------

# Per-playlist configuration
playlist_specs <- list(
  TTH = list(short = "TTH", long = "Todays_Top_Hits", id = "879549", start = "2024-01-01"),
  RC  = list(short = "RC",  long = "Rap_Caviar",      id = "127201", start = "2024-01-01"),
  VL  = list(short = "VL",  long = "Viva_Latino",     id = "879552", start = "2024-01-01"),
  BR  = list(short = "BR",  long = "Baila_Reggaeton", id = "511794", start = "2024-01-01")
)

# Per-year constants
ISRC_YEAR_CUTOFF <- 24
END_YEAR_STR     <- "2024-12-31"
FILTER_YEAR      <- "2024"

# Process one editorial playlist end-to-end.
# Returns a list with the enriched playlist data and the playlist-charts merge.
process_editorial_playlist <- function(spec, session, charts,
                                       streams_per_country, months) {

  # --- (a) Scrape the playlist day by day, starting at spec$start ---
  playlist_data <- data.frame()
  base_url <- paste0("https://www.spotontrack.com/playlists/spotify/", spec$id, "/")
  for (month in months) {
    n_days <- days_in_month(ymd(paste0(month, "-01")))
    for (day in 1:n_days) {
      date_chr <- sprintf("%s-%02d", month, day)
      if (as.Date(date_chr) < as.Date(spec$start)) next
      formatted_date <- format(ymd(date_chr), "%d.%m.%Y")
      date_url <- paste0(base_url, date_chr)
      print(paste("Processing URL:", date_url))
      daily_data <- extract_data_for_playlist(session, date_url, formatted_date)
      playlist_data <- bind_rows(playlist_data, daily_data)
    }
  }

  # --- (b) Enrich with ISRC + derive origin / label / release_year ---
  unique_songs <- playlist_data %>% distinct(song_id)
  unique_songs$isrc <- sapply(unique_songs$song_id, extract_isrc, session = session)
  playlist_data <- merge(playlist_data, unique_songs, by = "song_id")

  playlist_data$date <- format(dmy(playlist_data$date), "%Y-%m-%d")
  playlist_data$origin <- substr(playlist_data$isrc, 1, 2)
  playlist_data$label  <- substr(playlist_data$isrc, 3, 5)
  playlist_data$release_year <- ifelse(
    as.numeric(substr(playlist_data$isrc, 6, 7)) <= ISRC_YEAR_CUTOFF,
    paste0("20", substr(playlist_data$isrc, 6, 7)),
    paste0("19", substr(playlist_data$isrc, 6, 7))
  )
  playlist_data$origin <- isrc_country[playlist_data$origin]
  playlist_data$label  <- ifelse(playlist_data$label %in% names(isrc_label),
                                 isrc_label[playlist_data$label],
                                 "Indie")

  # Open drop_date when the song is still in the playlist on the last day observed
  playlist_data <- playlist_data %>%
    group_by(song_id, add_date) %>%
    mutate(drop_date = if_else(max(date) == END_YEAR_STR, NA, max(date))) %>%
    ungroup()
  playlist_data$add_date  <- as.Date(playlist_data$add_date)
  playlist_data$drop_date <- as.Date(playlist_data$drop_date)

  # --- (c) Merge with country charts and derive time / normalisation columns ---
  merged <- merge(charts, playlist_data, by = c("date", "song_id"), all.x = TRUE)
  merged <- merged %>% filter(song_id %in% playlist_data$song_id)

  merged <- merged %>%
    group_by(song_id) %>%
    fill(isrc, origin, label, release_year, .direction = "downup") %>%
    ungroup()

  add_drop_date <- merged %>%
    filter(!is.na(add_date)) %>%
    distinct(song_id, add_date, drop_date)
  merged <- merge(merged, add_drop_date, by = "song_id", all.x = TRUE)

  merged <- merged %>%
    rename(title = title.x, artist = artist.x,
           charts_rank = position.x, playlist_rank = position.y,
           add_date = add_date.y, drop_date = drop_date.y)
  merged$title.y     <- NULL
  merged$artist.y    <- NULL
  merged$add_date.x  <- NULL
  merged$drop_date.x <- NULL

  merged$days_in     <- merged$drop_date - merged$add_date
  merged$date        <- as.Date(merged$date)
  merged$time_add    <- merged$date - merged$add_date
  merged$time_drop   <- merged$date - merged$drop_date
  merged$day_of_week <- weekdays(merged$date)

  merged <- merged %>%
    filter(substr(date, 1, 4) == FILTER_YEAR) %>%
    left_join(streams_per_country, by = "country") %>%
    mutate(normalized_streams = streams / total_streams * 1000000)
  merged$total_streams <- NULL

  list(playlist = playlist_data, charts = merged)
}

# Run the pipeline for every editorial playlist
for (key in names(playlist_specs)) {
  spec <- playlist_specs[[key]]
  cat("\n=== Processing", spec$long, "(", spec$short, ") ===\n")
  result <- process_editorial_playlist(spec, session, Charts,
                                       streams_per_country, months)
  assign(paste0(spec$long, "_2024"), result$playlist)
  assign(paste0(spec$short, "_Charts_2024"), result$charts)
}

## Global Top 50 ---------------------------------------------------------

Global_Top50 <- data.frame()
base_playlist_url <- "https://www.spotontrack.com/playlists/spotify/3498602/"

### Loop through every month and day ----------------------------------------

for (month in months) {
  days_in_month <- days_in_month(ymd(paste0(month, "-01")))
  
  for (day in 1:days_in_month) {
    date <- sprintf("%s-%02d", month, day)
    
    if (date < as.Date("2024-01-01")) {
      next
    }
    
    formatted_date <- format(ymd(date), "%d.%m.%Y")
    
    date_url <- paste0(base_playlist_url, date)
    print(paste("Processing URL:", date_url))
    
    # extracting data for certain date
    daily_data <- extract_data_for_playlist(session, date_url, formatted_date)
    
    # adding the daily data to the dataframe
    Global_Top50 <- bind_rows(Global_Top50, daily_data)
  }
}

### Adding the ISRC ---------------------------------------------------------

unique_songs <- Global_Top50 %>% distinct(song_id)
unique_songs$isrc <- sapply(unique_songs$song_id, extract_isrc, session = session)
Global_Top50 <- merge(Global_Top50, unique_songs, by = "song_id")

### Other adjustments -------------------------------------------------------

# correct format for date
Global_Top50$date <- dmy(Global_Top50$date)
Global_Top50$date <- format(Global_Top50$date, "%Y-%m-%d")

# extracting country origin, label and release year from ISRC
Global_Top50$origin <- substr(Global_Top50$isrc, 1, 2)
Global_Top50$label <- substr(Global_Top50$isrc, 3, 5)
Global_Top50$release_year <- ifelse(as.numeric(substr(Global_Top50$isrc, 6, 7)) <= 24,
                                    paste("20", substr(Global_Top50$isrc, 6, 7), sep = ""),
                                    paste("19", substr(Global_Top50$isrc, 6, 7), sep = ""))

# allocating ISRC-abbreviations to origin country and record label
Global_Top50$origin <- isrc_country[Global_Top50$origin]
Global_Top50$label <- ifelse(Global_Top50$label %in% names(isrc_label),
                                isrc_label[Global_Top50$label],
                                "Indie")

# adding the drop_date
Global_Top50 <- Global_Top50 %>%
  group_by(song_id, add_date) %>%
  mutate(drop_date = if_else(max(date) == "2024-12-31", NA, max(date))) %>%
  ungroup()

# converting add and drop dates
Global_Top50$add_date <- as.Date(Global_Top50$add_date)
Global_Top50$drop_date <- as.Date(Global_Top50$drop_date)

## Cleaning environment ----------------------------------------------------

rm(result, spec, key, playlist_specs,
   ISRC_YEAR_CUTOFF, END_YEAR_STR, FILTER_YEAR,
   process_editorial_playlist,
   daily_data,
   filled_form,
   login_form,
   session,
   base_playlist_url,
   date_url,
   date,
   day,
   days_in_month,
   formatted_date,
   login_url,
   month,
   months,
   country,
   countries,
   isrc_country,
   isrc_label,
   extract_data_for_charts,
   extract_data_for_playlist,
   extract_isrc)