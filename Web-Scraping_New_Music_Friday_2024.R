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


# Relevant timeframe (2024) -----------------------------------

months <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "1 month")
months <- format(months, "%Y-%m")

# My functions for Web-Scraping -------------------------------------------

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
  
  tryCatch({
    webpage <- session_jump_to(session, isrc_url)
    
    if (is.null(webpage) || length(html_nodes(webpage, "body")) == 0) {
      warning(paste("Error: page npt available for song_id:", song_id))
      return(NA)
    }
    
    # extracting relevant text
    isrc_text <- webpage %>%
      html_nodes("div.h6.g-font-weight-100.w-100.g-mt-30") %>% 
      html_text(trim = TRUE)
    
    # extracting ISRC
    isrc <- str_extract(isrc_text, "[A-Za-z]{2}[A-Za-z0-9]{10}|[A-Za-z]{2}-[A-Za-z0-9]{3}-[A-Za-z0-9]{2}-[A-Za-z0-9]{5}")
    
    # checking for ISRC
    if (length(isrc) == 0 || is.na(isrc) || isrc == "") {
      warning(paste("ISRC not found for song-id:", song_id))
      return(NA)
    } else {
      isrc <- gsub("-", "", isrc)
      isrc <- toupper(str_trim(isrc))
      return(isrc)
    }
  }, error = function(e) {
    warning(paste("Error for song_id:", song_id, "Error:", e$message))
    return(NA)
  })
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

# Extracting data for NMF playlists  -----------------------

countries <- list(
  br = "https://www.spotontrack.com/playlists/spotify/260110/",
  ca = "https://www.spotontrack.com/playlists/spotify/362568/",
  ch = "https://www.spotontrack.com/playlists/spotify/1902690/",
  co = "https://www.spotontrack.com/playlists/spotify/1384216/",
  de = "https://www.spotontrack.com/playlists/spotify/532157/",
  dk = "https://www.spotontrack.com/playlists/spotify/332983/",
  es = "https://www.spotontrack.com/playlists/spotify/290718/",
  fi = "https://www.spotontrack.com/playlists/spotify/367408/",
  fr = "https://www.spotontrack.com/playlists/spotify/367460/",
  gb = "https://www.spotontrack.com/playlists/spotify/587446/",
  hk = "https://www.spotontrack.com/playlists/spotify/326807/",
  id = "https://www.spotontrack.com/playlists/spotify/354168/",
  is = "https://www.spotontrack.com/playlists/spotify/327242/",
  it = "https://www.spotontrack.com/playlists/spotify/340680/",
  mx = "https://www.spotontrack.com/playlists/spotify/456827/",
  my = "https://www.spotontrack.com/playlists/spotify/398344/",
  nl = "https://www.spotontrack.com/playlists/spotify/362534/",
  no = "https://www.spotontrack.com/playlists/spotify/367404/",
  ph = "https://www.spotontrack.com/playlists/spotify/497562/",
  pl = "https://www.spotontrack.com/playlists/spotify/2406516/",
  pt = "https://www.spotontrack.com/playlists/spotify/290519/",
  se = "https://www.spotontrack.com/playlists/spotify/367610/",
  sg = "https://www.spotontrack.com/playlists/spotify/326375/",
  tr = "https://www.spotontrack.com/playlists/spotify/988176/",
  tw = "https://www.spotontrack.com/playlists/spotify/340695/",
  us = "https://www.spotontrack.com/playlists/spotify/72762/"
)
New_Music_Friday <- data.frame()

## Loop through every country ----------------------------------------

for (country in names(countries)) {
  base_playlist_url <- countries[[country]]
  
  # Loop through every month and day
  for (month in months) {
    days_in_month <- days_in_month(ymd(paste0(month, "-01")))
    
    for (day in 1:days_in_month) {
      date <- sprintf("%s-%02d", month, day)
      formatted_date <- format(ymd(date), "%d.%m.%Y")
      date_url <- paste0(base_playlist_url, date)
      print(paste("Processing URL:", date_url))
      
      # extracting data for certain date
      daily_data <- tryCatch({
        extract_data_for_playlist(session, date_url, formatted_date)
      }, error = function(e) {
        print(paste("Skipping", date_url, "- Error encountered"))
        return(NULL)
      })
      
      # checking if data is available
      if (is.null(daily_data) || nrow(daily_data) == 0) {
        print(paste("Skipping", date_url, "- No data available"))
        next
      }
      
      # adding country of NMF playlist
      daily_data$country <- country
      
      # adding the daily data to the dataframe
      New_Music_Friday <- bind_rows(New_Music_Friday, daily_data)
    }
  }
}

## Adding the ISRC ---------------------------------------------------------

unique_songs <- New_Music_Friday %>% distinct(song_id)
unique_songs$isrc <- sapply(unique_songs$song_id, extract_isrc, session = session)
New_Music_Friday <- merge(New_Music_Friday, unique_songs, by = "song_id")

## Other adjustments -------------------------------------------------------

# correct format for date
New_Music_Friday$date <- dmy(New_Music_Friday$date)
New_Music_Friday$date <- format(New_Music_Friday$date, "%Y-%m-%d")

# extracting country origin, label and release year from ISRC
New_Music_Friday$origin <- substr(New_Music_Friday$isrc, 1, 2)
New_Music_Friday$label <- substr(New_Music_Friday$isrc, 3, 5)
New_Music_Friday$release_year <- ifelse(as.numeric(substr(New_Music_Friday$isrc, 6, 7)) <= 24,
                                        paste("20", substr(New_Music_Friday$isrc, 6, 7), sep = ""),
                                        paste("19", substr(New_Music_Friday$isrc, 6, 7), sep = ""))

# allocating ISRC-abbreviations to origin country and record label
New_Music_Friday$origin <- isrc_country[New_Music_Friday$origin]
New_Music_Friday$label <- ifelse(New_Music_Friday$label %in% names(isrc_label),
                                isrc_label[New_Music_Friday$label],
                                "Indie")

# adding the drop_date
New_Music_Friday <- New_Music_Friday %>%
  group_by(song_id, add_date) %>%
  mutate(drop_date = if_else(max(date) == "2024-12-31", NA, max(date))) %>%
  ungroup()

# converting add and drop dates
New_Music_Friday$add_date <- as.Date(New_Music_Friday$add_date)
New_Music_Friday$drop_date <- as.Date(New_Music_Friday$drop_date)

# Cleaning environment ----------------------------------------------------

rm(unique_songs,
   country,
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
   countries,
   isrc_country,
   isrc_label,
   extract_data_for_playlist,
   extract_isrc)
