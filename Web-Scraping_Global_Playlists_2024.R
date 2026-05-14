# Loading libraries -------------------------------------------------------

library(tidyverse)
library(rvest)
library(httr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)


# Login-data (Spotontrack-Account needed)--------------------------------------------------------------

login_url <- "https://www.spotontrack.com/login"
session <- session(login_url)
login_form <- session %>% 
  html_form() %>% 
  .[[1]]
filled_form <- html_form_set(login_form,
                             email = "example@thesis.com",
                             password = "example123")
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

## Todays Top Hits ---------------------------------------------------------

Todays_Top_Hits <- data.frame()
base_playlist_url <- "https://www.spotontrack.com/playlists/spotify/879549/"

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
    Todays_Top_Hits <- bind_rows(Todays_Top_Hits, daily_data)
  }
}

### Adding the ISRC ---------------------------------------------------------

unique_songs <- Todays_Top_Hits %>% distinct(song_id)
unique_songs$isrc <- sapply(unique_songs$song_id, extract_isrc, session = session)
Todays_Top_Hits <- merge(Todays_Top_Hits, unique_songs, by = "song_id")

### Other adjustments -------------------------------------------------------

# correct format for date
Todays_Top_Hits$date <- dmy(Todays_Top_Hits$date)
Todays_Top_Hits$date <- format(Todays_Top_Hits$date, "%Y-%m-%d")

# extracting country origin, label and release year from ISRC
Todays_Top_Hits$origin <- substr(Todays_Top_Hits$isrc, 1, 2) 
Todays_Top_Hits$label <- substr(Todays_Top_Hits$isrc, 3, 5)
Todays_Top_Hits$release_year <- ifelse(as.numeric(substr(Todays_Top_Hits$isrc, 6, 7)) <= 24,
                                       paste("20", substr(Todays_Top_Hits$isrc, 6, 7), sep = ""),
                                       paste("19", substr(Todays_Top_Hits$isrc, 6, 7), sep = ""))

# allocating ISRC-abbreviations to origin country and record label
Todays_Top_Hits$origin <- isrc_country[Todays_Top_Hits$origin]
Todays_Top_Hits$label <- ifelse(Todays_Top_Hits$label %in% names(isrc_label),
                           isrc_label[Todays_Top_Hits$label],
                           "Indie")

# adding the drop_date
Todays_Top_Hits <- Todays_Top_Hits %>%
  group_by(song_id, add_date) %>%
  mutate(drop_date = if_else(max(date) == "2024-12-31", NA, max(date))) %>%
  ungroup()

# converting add and drop dates
Todays_Top_Hits$add_date <- as.Date(Todays_Top_Hits$add_date)
Todays_Top_Hits$drop_date <- as.Date(Todays_Top_Hits$drop_date)

### Merging Playlist-data with Charts-data ----------------------------------

TTH_Charts <- merge(Charts, Todays_Top_Hits, by = c("date", "song_id"), all.x = TRUE)

# filtering for songs from playlist
TTH_Charts <- TTH_Charts %>% filter(song_id %in% Todays_Top_Hits$song_id)

# filling in missing data
TTH_Charts <- TTH_Charts %>%
  group_by(song_id) %>%
  fill(isrc, origin, label, release_year, .direction = "downup") %>%
  ungroup()
add_drop_date <- TTH_Charts %>%
  filter(is.na(add_date) == FALSE) %>%
  distinct(song_id, add_date, drop_date)
TTH_Charts <- merge(TTH_Charts, add_drop_date, by = ("song_id"), all.x = TRUE)

### Final adjustments -------------------------------------------------------

# renaming columns
TTH_Charts <- TTH_Charts %>%
  rename(title = title.x,
         artist = artist.x,
         charts_rank = position.x,
         playlist_rank = position.y,
         add_date = add_date.y,
         drop_date = drop_date.y)

# removing unnecessary columns
TTH_Charts$title.y <- NULL
TTH_Charts$artist.y <- NULL
TTH_Charts$add_date.x <- NULL
TTH_Charts$drop_date.x <- NULL

# spell length of a song in playlist
TTH_Charts$days_in <- TTH_Charts$drop_date - TTH_Charts$add_date

# formatting variable "date"
TTH_Charts$date <- as.Date(TTH_Charts$date)

# time to/since add-event (in days)
TTH_Charts$time_add <- TTH_Charts$date - TTH_Charts$add_date

# time to/since drop-event (in days)
TTH_Charts$time_drop <- TTH_Charts$date - TTH_Charts$drop_date

# adding weekday for date-variable
Sys.setlocale("LC_TIME", "en_US.UTF-8")
TTH_Charts$day_of_week <- weekdays(TTH_Charts$date)

# normalized streams
TTH_Charts <- TTH_Charts %>%
  left_join(streams_per_country, by = "country") %>%
  mutate(normalized_streams = streams / total_streams * 1000000)
TTH_Charts$total_streams <- NULL

## Rap Caviar ---------------------------------------------------------

Rap_Caviar <- data.frame()
base_playlist_url <- "https://www.spotontrack.com/playlists/spotify/127201/"

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
    Rap_Caviar <- bind_rows(Rap_Caviar, daily_data)
  }
}

### Adding the ISRC ---------------------------------------------------------

unique_songs <- Rap_Caviar %>% distinct(song_id)
unique_songs$isrc <- sapply(unique_songs$song_id, extract_isrc, session = session)
Rap_Caviar <- merge(Rap_Caviar, unique_songs, by = "song_id")

### Other adjustments -------------------------------------------------------

# correct format for date
Rap_Caviar$date <- dmy(Rap_Caviar$date)
Rap_Caviar$date <- format(Rap_Caviar$date, "%Y-%m-%d")

# extracting country origin, label and release year from ISRC
Rap_Caviar$origin <- substr(Rap_Caviar$isrc, 1, 2)
Rap_Caviar$label <- substr(Rap_Caviar$isrc, 3, 5)
Rap_Caviar$release_year <- ifelse(as.numeric(substr(Rap_Caviar$isrc, 6, 7)) <= 24,
                                  paste("20", substr(Rap_Caviar$isrc, 6, 7), sep = ""),
                                  paste("19", substr(Rap_Caviar$isrc, 6, 7), sep = ""))

# allocating ISRC-abbreviations to origin country and record label
Rap_Caviar$origin <- isrc_country[Rap_Caviar$origin]
Rap_Caviar$label <- ifelse(Rap_Caviar$label %in% names(isrc_label),
                          isrc_label[Rap_Caviar$label],
                          "Indie")

# adding the drop_date
Rap_Caviar <- Rap_Caviar %>%
  group_by(song_id, add_date) %>%
  mutate(drop_date = if_else(max(date) == "2024-12-31", NA, max(date))) %>%
  ungroup()

# converting add and drop dates
Rap_Caviar$add_date <- as.Date(Rap_Caviar$add_date)
Rap_Caviar$drop_date <- as.Date(Rap_Caviar$drop_date)

### Merging Playlist-data with Charts-data ----------------------------------

RC_Charts <- merge(Charts, Rap_Caviar, by = c("date", "song_id"), all.x = TRUE)

# filtering for songs from playlist
RC_Charts <- RC_Charts %>% filter(song_id %in% Rap_Caviar$song_id)

# filling in missing data
RC_Charts <- RC_Charts %>%
  group_by(song_id) %>%
  fill(isrc, origin, label, release_year, .direction = "downup") %>%
  ungroup()
add_drop_date <- RC_Charts %>%
  filter(is.na(add_date) == FALSE) %>%
  distinct(song_id, add_date, drop_date)
RC_Charts <- merge(RC_Charts, add_drop_date, by = ("song_id"), all.x = TRUE)

### Final adjustments -------------------------------------------------------

# renaming columns
RC_Charts <- RC_Charts %>%
  rename(title = title.x,
         artist = artist.x,
         charts_rank = position.x,
         playlist_rank = position.y,
         add_date = add_date.y,
         drop_date = drop_date.y)

# removing unnecessary columns
RC_Charts$title.y <- NULL
RC_Charts$artist.y <- NULL
RC_Charts$add_date.x <- NULL
RC_Charts$drop_date.x <- NULL

# spell length of a song in playlist
RC_Charts$days_in <- RC_Charts$drop_date - RC_Charts$add_date

# formatting variable "date"
RC_Charts$date <- as.Date(RC_Charts$date)

# time to/since add-event (in days)
RC_Charts$time_add <- RC_Charts$date - RC_Charts$add_date

# time to/since drop-event (in days)
RC_Charts$time_drop <- RC_Charts$date - RC_Charts$drop_date

# adding weekday for date-variable
RC_Charts$day_of_week <- weekdays(RC_Charts$date)

# normalized streams and filtering for 2024-data
RC_Charts <- RC_Charts %>%
  filter(substr(date, 1, 4) == "2024") %>%
  left_join(streams_per_country, by = "country") %>%
  mutate(normalized_streams = streams / total_streams * 1000000)
RC_Charts$total_streams <- NULL

## Viva Latino ---------------------------------------------------------

Viva_Latino <- data.frame()
base_playlist_url <- "https://www.spotontrack.com/playlists/spotify/879552/"

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
    Viva_Latino <- bind_rows(Viva_Latino, daily_data)
  }
}

### Adding the ISRC ---------------------------------------------------------

unique_songs <- Viva_Latino %>% distinct(song_id)
unique_songs$isrc <- sapply(unique_songs$song_id, extract_isrc, session = session)
Viva_Latino <- merge(Viva_Latino, unique_songs, by = "song_id")

### Other adjustments -------------------------------------------------------

# correct format for date
Viva_Latino$date <- dmy(Viva_Latino$date)
Viva_Latino$date <- format(Viva_Latino$date, "%Y-%m-%d")

# extracting country origin, label and release year from ISRC
Viva_Latino$origin <- substr(Viva_Latino$isrc, 1, 2)
Viva_Latino$label <- substr(Viva_Latino$isrc, 3, 5)
Viva_Latino$release_year <- ifelse(as.numeric(substr(Viva_Latino$isrc, 6, 7)) <= 24,
                                   paste("20", substr(Viva_Latino$isrc, 6, 7), sep = ""),
                                   paste("19", substr(Viva_Latino$isrc, 6, 7), sep = ""))

# allocating ISRC-abbreviations to origin country and record label
Viva_Latino$origin <- isrc_country[Viva_Latino$origin]
Viva_Latino$label <- ifelse(Viva_Latino$label %in% names(isrc_label),
                          isrc_label[Viva_Latino$label],
                          "Indie")

# adding the drop_date
Viva_Latino <- Viva_Latino %>%
  group_by(song_id, add_date) %>%
  mutate(drop_date = if_else(max(date) == "2024-12-31", NA, max(date))) %>%
  ungroup()

# converting add and drop dates
Viva_Latino$add_date <- as.Date(Viva_Latino$add_date)
Viva_Latino$drop_date <- as.Date(Viva_Latino$drop_date)

### Merging Playlist-data with Charts-data ----------------------------------

VL_Charts <- merge(Charts, Viva_Latino, by = c("date", "song_id"), all.x = TRUE)

# filtering for songs from playlist
VL_Charts <- VL_Charts %>% filter(song_id %in% Viva_Latino$song_id)

# filling in missing data
VL_Charts <- VL_Charts %>%
  group_by(song_id) %>%
  fill(isrc, origin, label, release_year, .direction = "downup") %>%
  ungroup()
add_drop_date <- VL_Charts %>%
  filter(is.na(add_date) == FALSE) %>%
  distinct(song_id, add_date, drop_date)
VL_Charts <- merge(VL_Charts, add_drop_date, by = ("song_id"), all.x = TRUE)

### Final adjustments -------------------------------------------------------

# renaming columns
VL_Charts <- VL_Charts %>%
  rename(title = title.x,
         artist = artist.x,
         charts_rank = position.x,
         playlist_rank = position.y,
         add_date = add_date.y,
         drop_date = drop_date.y)

# removing unnecessary columns
VL_Charts$title.y <- NULL
VL_Charts$artist.y <- NULL
VL_Charts$add_date.x <- NULL
VL_Charts$drop_date.x <- NULL

# spell length of a song in playlist
VL_Charts$days_in <- VL_Charts$drop_date - VL_Charts$add_date

# formatting variable "date"
VL_Charts$date <- as.Date(VL_Charts$date)

# time to/since add-event (in days)
VL_Charts$time_add <- VL_Charts$date - VL_Charts$add_date

# time to/since drop-event (in days)
VL_Charts$time_drop <- VL_Charts$date - VL_Charts$drop_date

# adding weekday for date-variable
VL_Charts$day_of_week <- weekdays(VL_Charts$date)

# normalized streams and filtering for 2024-data
VL_Charts <- VL_Charts %>%
  filter(substr(date, 1, 4) == "2024") %>%
  left_join(streams_per_country, by = "country") %>%
  mutate(normalized_streams = streams / total_streams * 1000000)
VL_Charts$total_streams <- NULL

## Baila Reggaeton ---------------------------------------------------------

Baila_Reggaeton <- data.frame()
base_playlist_url <- "https://www.spotontrack.com/playlists/spotify/511794/"

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
    Baila_Reggaeton <- bind_rows(Baila_Reggaeton, daily_data)
  }
}

### Adding the ISRC ---------------------------------------------------------

unique_songs <- Baila_Reggaeton %>% distinct(song_id)
unique_songs$isrc <- sapply(unique_songs$song_id, extract_isrc, session = session)
Baila_Reggaeton <- merge(Baila_Reggaeton, unique_songs, by = "song_id")

### Other adjustments -------------------------------------------------------

# correct format for date
Baila_Reggaeton$date <- dmy(Baila_Reggaeton$date)
Baila_Reggaeton$date <- format(Baila_Reggaeton$date, "%Y-%m-%d")

# extracting country origin, label and release year from ISRC
Baila_Reggaeton$origin <- substr(Baila_Reggaeton$isrc, 1, 2)
Baila_Reggaeton$label <- substr(Baila_Reggaeton$isrc, 3, 5)
Baila_Reggaeton$release_year <- ifelse(as.numeric(substr(Baila_Reggaeton$isrc, 6, 7)) <= 24,
                                       paste("20", substr(Baila_Reggaeton$isrc, 6, 7), sep = ""),
                                       paste("19", substr(Baila_Reggaeton$isrc, 6, 7), sep = ""))

# allocating ISRC-abbreviations to origin country and record label
Baila_Reggaeton$origin <- isrc_country[Baila_Reggaeton$origin]
Baila_Reggaeton$label <- ifelse(Baila_Reggaeton$label %in% names(isrc_label),
                          isrc_label[Baila_Reggaeton$label],
                          "Indie")

# adding the drop_date
Baila_Reggaeton <- Baila_Reggaeton %>%
  group_by(song_id, add_date) %>%
  mutate(drop_date = if_else(max(date) == "2024-12-31", NA, max(date))) %>%
  ungroup()

# converting add and drop dates
Baila_Reggaeton$add_date <- as.Date(Baila_Reggaeton$add_date)
Baila_Reggaeton$drop_date <- as.Date(Baila_Reggaeton$drop_date)

### Merging Playlist-data with Charts-data ----------------------------------

BR_Charts <- merge(Charts, Baila_Reggaeton, by = c("date", "song_id"), all.x = TRUE)

# filtering for songs from playlist
BR_Charts <- BR_Charts %>% filter(song_id %in% Baila_Reggaeton $song_id)

# filling in missing data
BR_Charts <- BR_Charts %>%
  group_by(song_id) %>%
  fill(isrc, origin, label, release_year, .direction = "downup") %>%
  ungroup()
add_drop_date <- BR_Charts %>%
  filter(is.na(add_date) == FALSE) %>%
  distinct(song_id, add_date, drop_date)
BR_Charts <- merge(BR_Charts, add_drop_date, by = ("song_id"), all.x = TRUE)

### Final adjustments -------------------------------------------------------

# renaming columns
BR_Charts <- BR_Charts %>%
  rename(title = title.x,
         artist = artist.x,
         charts_rank = position.x,
         playlist_rank = position.y,
         add_date = add_date.y,
         drop_date = drop_date.y)

# removing unnecessary columns
BR_Charts$title.y <- NULL
BR_Charts$artist.y <- NULL
BR_Charts$add_date.x <- NULL
BR_Charts$drop_date.x <- NULL

# spell length of a song in playlist
BR_Charts$days_in <- BR_Charts$drop_date - BR_Charts$add_date

# formatting variable "date"
BR_Charts$date <- as.Date(BR_Charts$date)

# time to/since add-event (in days)
BR_Charts$time_add <- BR_Charts$date - BR_Charts$add_date

# time to/since drop-event (in days)
BR_Charts$time_drop <- BR_Charts$date - BR_Charts$drop_date

# adding weekday for date-variable
BR_Charts$day_of_week <- weekdays(BR_Charts$date)

# normalized streams and filtering for 2024-data
BR_Charts <- BR_Charts %>%
  filter(substr(date, 1, 4) == "2024") %>%
  left_join(streams_per_country, by = "country") %>%
  mutate(normalized_streams = streams / total_streams * 1000000)
BR_Charts$total_streams <- NULL

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

rm(unique_songs,
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
   extract_isrc,
   add_drop_date)