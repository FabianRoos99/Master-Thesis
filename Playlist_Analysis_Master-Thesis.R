# Loading libraries and data -------------------------------------------------------

library(tidyverse)
library(fixest)
library(ggplot2)
library(patchwork)
library(flextable)
library(lubridate)
library(modelsummary)
library(kableExtra)
library(webshot2)
library(purrr)
library(rlang)
library(gridExtra)
library(grid)
library(knitr)
library(scales)

load("all_data.RData")


# Playlists Types (Table 1) ---------------------------------------------------------

playlist_table <- data.frame(
  Type = c(
    "User-generated",
    "Label-curated",
    "Spotify editorial",
    "Spotify algotorial",
    "Spotify algorithmic"
  ),
  Source = c(
    "User",
    "Label",
    "Spotify Editors",
    "Spotify Editors + Algorithm",
    "Spotify Algorithm"
  ),
  Personalization = c(
    "Non-personalized",
    "Non-personalized",
    "Non-personalized",
    "Semi-personalized",
    "Fully-personalized / Non-personalized"
  ),
  Example = c(
    "Lofi Fruits Music",
    "80s HITS | TOP 100 SONGS",
    "Today's Top Hits",
    "Your Daily Mix",
    "Discover Weekly / Top 50 - Global"
  ),
  stringsAsFactors = FALSE
)

playlist_ft <- playlist_table %>%
  flextable() %>%
  # header labels
  set_header_labels(
    Type = "Type",
    Source = "Source",
    Personalization = "Personalization",
    Example = "Example"
  ) %>%
  # header/body emphasis
  bold(part = "header", bold = TRUE) %>%
  # layout & column widths (in inches)
  set_table_properties(layout = "fixed") %>%
  width(j = ~ Type,            width = 1.8) %>%
  width(j = ~ Source,          width = 1.7) %>%
  width(j = ~ Personalization, width = 2.3) %>%
  width(j = ~ Example,         width = 2.6) %>%
  align(j = ~ Type + Source + Personalization + Example, align = "left", part = "body") %>%
  align(j = ~ Type + Source + Personalization + Example, align = "left", part = "header") %>%
  padding(j = ~ Type, padding.left = 0, part = "all") %>%
  padding(padding.right = 8, part = "all") %>%
  font(part = "all", fontname = "Times New Roman")
save_as_image(
  playlist_ft,
  path = "D:/Uni/Master/Master-Thesis/Sources/Tables/Playlist_Types.png",
  zoom = 2
)


# Top 200 Streaming 2017 vs. 2024 (Table 2)-----------------------------------------

# Merging 2017 with 2024 streams
streams_per_country_2017_vs_2024 <- streams_per_country_2017 %>%
  left_join(streams_per_country_2024, by = "country") %>%
  rename(total_streams_2017 = total_streams.x, total_streams_2024 = total_streams.y)

# Adding total streams to streams per country data
streams_per_country_2017_vs_2024 <- rbind(
  streams_per_country_2017_vs_2024,
  data.frame(country = "total",
             total_streams_2017 = sum(streams_per_country_2017_vs_2024$total_streams_2017),
             total_streams_2024 = sum(streams_per_country_2017_vs_2024$total_streams_2024)
  ))
streams_per_country_2017_vs_2024 <- streams_per_country_2017_vs_2024 %>%
  mutate(
    chg_num = 100 * (total_streams_2024 - total_streams_2017) / total_streams_2017,
    change  = if_else(
      is.finite(chg_num),
      paste0(
        if_else(chg_num >= 0, "+", "-"),
        formatC(abs(chg_num), format = "f", digits = 0, big.mark = ","),
        "%"
      ),
      NA_character_
    )
  ) %>%
  select(-chg_num)
streams_per_country_2017_vs_2024$total_streams_2017 <- round(streams_per_country_2017_vs_2024$total_streams_2017 / 1000000, 1)
streams_per_country_2017_vs_2024$total_streams_2024 <- round(streams_per_country_2017_vs_2024$total_streams_2024 / 1000000, 1)

# Renaming countries
streams_per_country_2017_vs_2024 <- streams_per_country_2017_vs_2024 %>%
  rename(country_code = country)
country_names <- data.frame(
  country_code = c("br", "ca", "ch", "co", "de", "dk", "es", "fi", "fr", "gb", "hk", 
                   "id", "is", "it", "mx", "my", "nl", "no", "ph", "pl", "pt", "se", 
                   "sg", "tr", "tw", "us", "total"),
  country_name = c("Brazil", "Canada", "Switzerland", "Colombia", "Germany", "Denmark", "Spain", 
                   "Finland", "France", "Great Britain", "Hong Kong", "Indonesia", "Iceland",
                   "Italy", "Mexico", "Malaysia", "Netherlands", "Norway", "Philippines", "Poland", 
                   "Portugal", "Sweden", "Singapore", "Turkey", "Taiwan", "United States", "Total"))
streams_per_country_2017_vs_2024 <- merge(country_names, streams_per_country_2017_vs_2024,, by = "country_code", all.x = TRUE)
streams_per_country_2017_vs_2024$country_code <- NULL

# Creating table
streams_per_country_table <- streams_per_country_2017_vs_2024 %>%
  arrange(country_name == "Total", country_name) %>%
  flextable() %>%
  set_header_labels(
    country_name = "Country",
    total_streams_2017 = "2017",
    total_streams_2024 = "2024",
    change = "Delta"
  ) %>%
  bold(part = "header", bold = TRUE) %>%
  bold(i = ~ country_name == "Total", part = "body", bold = TRUE) %>%
  set_table_properties(layout = "fixed") %>%
  width(j = ~ country_name, width = 2.2) %>%
  width(j = ~ total_streams_2017 + total_streams_2024, width = 1.6) %>%
  width(j = ~ change, width = 1.2) %>%
  align(j = ~ country_name + total_streams_2017 + total_streams_2024 + change,
        align = "left", part = "body") %>%
  align(j = ~ country_name + total_streams_2017 + total_streams_2024 + change,
        align = "left", part = "header") %>%
  padding(j = ~ country_name + change, padding.left = 0, part = "all") %>%
  padding(padding.right = 8, part = "all") %>%
  font(part = "all", fontname = "Times New Roman")
save_as_image(streams_per_country_table, "Streams_per_Country_2017_vs_2024.png")


# Playlists characteristics (Tables 3 & 4) -----------------------------------------------

# Helper
make_playlist_row <- function(playlist_name, playlist_df, charts_df,
                              followers_m, start,
                              exclude_years = character(),
                              exclude_dates = character()) {
  charts_filter <- charts_df %>%
    filter(!substr(add_date, 1, 4) %in% exclude_years,
           !add_date %in% exclude_dates)
  
  num_songs <- n_distinct(playlist_df$song_id)
  
  mean_spell_duration <- charts_filter %>%
    filter(!is.na(drop_date)) %>%
    distinct(song_id, .keep_all = TRUE) %>%
    summarise(v = mean(days_in, na.rm = TRUE)) %>%
    pull(v) %>% round(1)
  
  mean_spell_per_song <- charts_filter %>%
    filter(!is.na(drop_date)) %>%
    distinct(song_id, days_in) %>%
    count(song_id, name = "spells") %>%
    summarise(v = mean(spells)) %>%
    pull(v) %>% round(3)
  
  per_song <- charts_filter %>%
    filter(!is.na(playlist_rank), !is.na(drop_date)) %>%
    group_by(song_id) %>%
    summarise(total_streams_per_song = sum(streams, na.rm = TRUE), .groups = "drop")
  
  median_streams <- per_song %>%
    summarise(v = median(total_streams_per_song, na.rm = TRUE)) %>%
    pull(v) / 1e6 %>% round(1)
  
  mean_streams <- per_song %>%
    summarise(v = mean(total_streams_per_song, na.rm = TRUE)) %>%
    pull(v) / 1e6 %>% round(1)
  
  tibble(
    playlist_name = playlist_name,
    start = start,
    number_of_songs = num_songs,
    followers_in_million = followers_m,
    mean_spell_duration = mean_spell_duration,
    mean_spell_per_song = mean_spell_per_song,
    median_streams = median_streams,
    mean_streams = mean_streams
  )
}

# Year-specific playlists characteristics
specs_2017 <- list(
  list(name="Today's Top Hits", df=Todays_Top_Hits_2017, charts=TTH_Charts_2017,
       followers=18.5, start="5/3/17", exclude_dates = c("2017-04-28","2017-04-30")),
  list(name="Rap Caviar", df=Rap_Caviar_2017, charts=RC_Charts_2017,
       followers=8.6, start="3/3/17", exclude_dates = c("2017-03-03")),
  list(name="Viva Latino", df=Viva_Latino_2017, charts=VL_Charts_2017,
       followers=6.9, start="5/3/17", exclude_dates = c("2017-04-28")),
  list(name="Baila Reggaeton", df=Baila_Reggaeton_2017, charts=BR_Charts_2017,
       followers=6.3, start="4/16/17", exclude_dates = c("2017-04-14"))
)

specs_2024 <- list(
  list(name = "Today's Top Hits", df = Todays_Top_Hits_2024, charts = TTH_Charts_2024,
       followers = 35.1, start = "1/1/24", exclude_years = c("2022","2023")),
  list(name = "Rap Caviar", df = Rap_Caviar_2024, charts = RC_Charts_2024,
       followers = 16.1, start = "1/1/24", exclude_years = c("2023")),
  list(name = "¡Viva Latino!", df = Viva_Latino_2024, charts = VL_Charts_2024,
       followers = 15.4, start = "1/1/24", exclude_years = c("2023")),
  list(name = "Baila Reggaeton", df = Baila_Reggaeton_2024, charts = BR_Charts_2024,
       followers = 10.7, start = "1/1/24", exclude_years = c("2022","2023"))
)

# Preparing lists for loop
specs_by_year <- list(
  `2017` = specs_2017,
  `2024` = specs_2024)
dfs_by_year <- list()
tables_by_year <- list()

# Loop for table creation
for (year in names(specs_by_year)) {
  
  df <- purrr::map_df(
    specs_by_year[[year]],
    ~ make_playlist_row(
      .x$name, .x$df, .x$charts,
      followers_m   = .x$followers,
      start         = .x$start,
      exclude_years = .x$exclude_years %||% character(0),
      exclude_dates = .x$exclude_dates %||% character(0)
    )
  ) %>%
    mutate(
      across(c(median_streams, mean_streams, followers_in_million), ~ round(.x, 1)),
      number_of_songs = as.integer(number_of_songs)
    )
  
  tbl <- flextable(df) %>%
    set_header_labels(
      playlist_name = "Playlist Name",
      start = "Start",
      number_of_songs = "No. of\nSongs",
      followers_in_million = "Followers\n(millions)",
      mean_spell_duration = "Mean Spell\nDuration",
      mean_spell_per_song = "Mean Spells\nper Song",
      median_streams = "Median Streams\n(millions)",
      mean_streams   = "Mean Streams\n(millions)"
    ) %>%
    font(part = "all", fontname = "Times New Roman") %>%
    bold(part = "header") %>%
    align(part = "header", align = "center") %>%
    colformat_num(j = c("median_streams","mean_streams","followers_in_million"), digits = 1) %>%
    colformat_num(j = "number_of_songs", digits = 0, big.mark = ",") %>%
    colformat_num(j = "mean_spell_duration", digits = 1) %>%
    colformat_num(j = "mean_spell_per_song", digits = 3) %>%
    autofit() %>%
    align(align = "left", part = "header") %>%  # force left everywhere
    align(align = "left", part = "body")
  
  save_as_image(tbl, paste0("Playlist_Characteristics_", year, ".png"))
}


# Editorial Global Playlists (Figures 2 & 3, Appendix 1-6) --------

# Helper

## y-breaks
br_fun <- function(lims) {
  b <- pretty(lims, n = 7)
  sort(unique(c(b, 0)))
}

## fetch the right dataframe by playlist code + year
get_df <- function(playlist, year) {
  base <- switch(playlist,
                 "TTH" = "TTH_Charts",
                 "RC"  = "RC_Charts",
                 "VL"  = "VL_Charts",
                 "BR"  = "BR_Charts"
  )
  nm1 <- paste0(base, "_", year)
  df <- get0(nm1, inherits = TRUE)
  if (is.null(df)) df <- get0(base, inherits = TRUE)
  if (is.null(df)) stop("Data frame not found: ", nm1, " or ", base)
  df
}

## playlist-specific add-date exclusions (just 2017)
add_exclude <- list(
  `2017` = list(TTH = c("2017-04-28"),
                RC  = c("2017-03-03"),
                VL  = c("2017-04-28"),
                BR  = c("2017-04-14")),
  `2024` = list(TTH = character(0), RC = character(0), VL = character(0), BR = character(0))
)

## prepare -> estimate -> extract -> plot
run_event <- function(df, event = c("add","drop"), add_excl = character(0)) {
  
  ### prepare
  event <- match.arg(event)
  
  if (event == "add") {
    df$time_add <- as.numeric(df$time_add)
    df_evt <- df %>%
      mutate(days_evt = time_add + 2) %>%
      group_by(song_id) %>%
      filter(n_distinct(add_date) == 1) %>%
      ungroup() %>%
      filter(days_evt >= -30 & days_evt <= 30,
             !as.character(add_date) %in% add_excl) %>%
      mutate(days_evt = factor(days_evt),
             days_evt = relevel(days_evt, ref = "0"))
    
  } else {
    df$time_drop <- as.numeric(df$time_drop)
    df_evt <- df %>%
      mutate(days_evt = time_drop + 1) %>%
      group_by(song_id) %>%
      filter(n_distinct(add_date) == 1) %>%
      ungroup() %>%
      filter(days_evt >= -30 & days_evt <= 30,
             !is.na(drop_date)) %>%
      mutate(days_evt = factor(days_evt),
             days_evt = relevel(days_evt, ref = "0"))
  }
  
  ### estimate
  model <- feols(
    normalized_streams ~ factor(days_evt) | song_id^country + day_of_week,
    data = df_evt, cluster = ~ song_id^country
  )
  
  ### extract
  coefs <- coef(model); ses <- se(model)
  idx <- grepl("^factor\\(days_evt\\)", names(coefs))
  tidy <- tibble(
    term = names(coefs)[idx],
    coefficient = unname(coefs[idx]),
    standard_error = unname(ses[idx])
  ) %>%
    mutate(days_evt = as.numeric(sub("factor\\(days_evt\\)", "", term))) %>%
    select(days_evt, coefficient, standard_error) %>%
    bind_rows(tibble(days_evt = 0, coefficient = 0, standard_error = 0)) %>%
    arrange(days_evt) %>%
    mutate(l95 = coefficient - 1.96 * standard_error,
           u95 = coefficient + 1.96 * standard_error)
  
  ### plot
  plot <- ggplot(tidy, aes(x = days_evt, y = coefficient)) +
    annotate("rect", xmin = 0, xmax = 3, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "black") +
    geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.15) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    geom_line(size = 0.5) +
    geom_point(shape = 21, size = 2, stroke = 1, fill = NA) +
    scale_x_continuous(breaks = seq(min(tidy$days_evt), max(tidy$days_evt), by = 2)) +
    labs(x = paste("days around", event),
         y = "normalized streams") +
    theme_minimal(base_size = 14, base_family = "Times New Roman") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  list(model = model, data = tidy, plot = plot)
}

## helper to save results as png-table
save_event_table_png <- function(event_res, file, title = NULL,
                                 base_family = "Times New Roman", decimals = 3) {
  ### days_evt, coefficient, standard_error, l95, u95
  tab <- event_res$data %>%
    arrange(days_evt) %>%
    transmute(
      `τ (day)` = days_evt,
      `β̂`       = round(coefficient, decimals),
      `SE`       = na_if(round(standard_error, decimals), 0),  # leave τ=0 blank if 0
      `95% CI`   = if_else(is.na(l95) | is.na(u95), "",
                           paste0("[", round(l95, decimals), ", ", round(u95, decimals), "]"))
    )
  
  tbl <- gridExtra::tableGrob(
    tab, rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core   = list(fg_params = list(fontsize = 10, fontfamily = base_family),
                    padding  = unit(c(6, 6), "pt")),
      colhead= list(fg_params = list(fontsize = 11, fontface = "bold", fontfamily = base_family),
                    padding  = unit(c(8, 8), "pt"))
    )
  )

  ggsave(filename = file, plot = tbl, width = 8, height = 24, dpi = 300, limitsize = FALSE)
  invisible(file)
}

# Main loops
for (year in c(2017, 2024)) {
  for (playlist in c("TTH","RC","VL","BR")) {
    
    df <- get_df(playlist, year)
    
    res <- list()
    for (event in c("add","drop")) {
      res[[event]] <- run_event(
        df,
        event = event,
        add_excl = if (event == "add") add_exclude[[as.character(year)]][[playlist]] else character(0)
      )
      
      ## saving table with estimates
      png_file <- sprintf("%s_%d_%s_table.png", playlist, year, event)
      title    <- sprintf("%s %d — %s (Event-time coefficients)", playlist, year, toupper(event))
      save_event_table_png(res[[event]], png_file, title = title)
    }
    
    ## symmetric y-limits and y-ticks on both panels
    Y_add  <- max(abs(c(res$add$data$l95,  res$add$data$u95)),  na.rm = TRUE)
    Y_drop <- max(abs(c(res$drop$data$l95, res$drop$data$u95)), na.rm = TRUE)
    Y <- 1.05 * max(Y_add, Y_drop)
    p_add  <- res$add$plot  + coord_cartesian(ylim = c(-Y, Y)) + scale_y_continuous(breaks = br_fun)
    p_drop <- res$drop$plot + coord_cartesian(ylim = c(-Y, Y)) + scale_y_continuous(breaks = br_fun)
    
    ## combine and save plots
    fig <- p_add + p_drop
    ggsave(paste0(playlist, "_FE_Plot_", year, ".png"), fig, width = 10, height = 5, dpi = 300)
  }
}


# Global Top 50 Playlist (Figures 4 & 5) --------------------------------------------------

for (year in c(2017, 2024)) {

  Global_Top50_extended <- get(paste0("Top200_global_", year)) %>%
    mutate(position = as.numeric(position)) %>%
    arrange(song_id, date) %>%
    group_by(song_id) %>%
    mutate(playlist_rank = lag(position)) %>%
    ungroup() %>%
    filter(substr(date, 1, 4) == year) %>%
    arrange(date, playlist_rank) %>%
    group_by(date) %>%
    mutate(
      log_stream_ratio = log(streams / lag(streams))
    ) %>%
    ungroup()
  
  avg_log_stream_ratio <- Global_Top50_extended %>%
    filter(playlist_rank >= 40 & playlist_rank <= 60) %>%
    group_by(playlist_rank) %>%
    summarize(
      theta = mean(log_stream_ratio, na.rm = TRUE),
      se_log_ratio = sd(log_stream_ratio, na.rm = TRUE) / sqrt(n()),
      ci_lower = theta - 1.96 * se_log_ratio,
      ci_upper = theta + 1.96 * se_log_ratio,
      n = n()
    )
  
  plot <- ggplot(avg_log_stream_ratio, aes(x = playlist_rank)) +
    annotate("rect", xmin = 50, xmax = 51, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "black") +
    geom_line(aes(y = theta, colour = "Estimate"), linewidth = 1) +
    geom_point(aes(y = theta, colour = "Estimate"), size = 2) +
    geom_line(aes(y = ci_upper, colour = "95% CI", linetype = "95% CI"), linewidth = 0.8) +
    geom_line(aes(y = ci_lower, colour = "95% CI", linetype = "95% CI"), linewidth = 0.8) +
    scale_colour_manual(
      name = "",
      breaks = c("Estimate", "95% CI"),
      values = c("Estimate" = "black", "95% CI" = "grey60")) +
    scale_linetype_manual(
      values = c("Estimate" = "solid", "95% CI" = "longdash"),
      guide  = "none") +
    guides(colour = guide_legend(
      override.aes = list(
        linetype  = c("solid", "longdash"),
        shape     = c(16, NA),
        linewidth = c(1, 0.8)))) +
    labs(
      x = "Global Top 50 rank (r)",
      y = expression(paste("log change in streams (", theta[r], ")"))) +
    scale_x_continuous(
      breaks = seq(min(avg_log_stream_ratio$playlist_rank, na.rm = TRUE),
                   max(avg_log_stream_ratio$playlist_rank, na.rm = TRUE), by = 1),
      minor_breaks = NULL) +
    theme_minimal(base_size = 14, base_family = "Times New Roman") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.key.width  = unit(2.4, "lines"),
      legend.key.height = unit(0.6, "lines"),
      axis.title.x = element_text(margin = margin(t = 10)))
  ggsave(paste0("Top_50_global_", year, ".png"), plot, width = 10, height = 5, dpi = 300)
}


# New Music Friday (Figures 6-9, Tables 5 & 6, Appendix 7)--------------------------------------------------------

# Helper
get_obj <- function(prefix, year) {
  nm <- paste0(prefix, "_", year)
  x <- get0(nm, inherits = TRUE)
  if (is.null(x)) stop("Object not found: ", nm)
  x
}

nmf_shares_list <- list()

# Main loop
for (year in c(2017, 2024)) {
  prev_year <- year - 1
  
  ## Data: complete charts and NMF
  charts_all <- get_obj("Charts", year)
  nmf <- get_obj("New_Music_Friday", year)
  
  ## Charts in 'prev_year'
  charts_prev <- charts_all %>% filter(format(as.Date(date), "%Y") == as.character(prev_year))
  
  ## NMF (position ≤ 50, one obs per song/country/week)
  nmf <- nmf %>%
    filter(position <= 50) %>%
    mutate(
      week_start = as.Date(date) - wday(as.Date(date), week_start = 5) + 1,
      position_group = case_when(
        position <=  5 ~ "NMF Rank: 1-5",
        position <= 10 ~ "NMF Rank: 6-10",
        position <= 30 ~ "NMF Rank: 11-30",
        position <= 50 ~ "NMF Rank: 31-50"),
      domestic = country == origin,
      streams_prev = artist %in% charts_prev$artist
    ) %>%
    filter(format(week_start, "%Y") == as.character(year),
           as.Date(week_start) == as.Date(date)) %>%
    select(-any_of(c("date", "add_date", "drop_date"))) %>%
    distinct()
  
  # shares of domestic and indie songs on NMF
  shares_this_year <- nmf %>%
    summarise(
      N_rows          = n(),
      Share_domestic  = mean(domestic, na.rm = TRUE),
      Share_indie     = mean(label == "Indie", na.rm = TRUE)
    ) %>%
    mutate(year = year,
           Share_domestic = percent(Share_domestic, 0.1),
           Share_indie    = percent(Share_indie,  0.1)) %>%
    select(year, N_rows, Share_domestic, Share_indie)
  nmf_shares_list[[as.character(year)]] <- shares_this_year
  
  ## Charts in 'year' (Top-100 flag)
  charts_year <- charts_all %>%
    filter(format(as.Date(date), "%Y") == as.character(year)) %>%
    mutate(
      week_start = as.Date(date) - wday(as.Date(date), week_start = 5) + 1,
      position = as.numeric(position)
    ) %>%
    filter(format(week_start, "%Y") == as.character(year)) %>%
    mutate(in_top100 = position <= 100) %>%
    select(song_id, country, in_top100) %>%
    distinct() %>%
    group_by(song_id, country) %>%
    filter(!(any(in_top100 == TRUE) & in_top100 == FALSE)) %>%
    ungroup()
  
  ## Merge NMF with chart indicators (Top200/Top100)
  nmf <- nmf %>%
    left_join(charts_year %>% mutate(in_top200 = 1L), by = c("song_id", "country")) %>%
    mutate(
      in_top200 = ifelse(is.na(in_top200), 0L, 1L),
      in_top100 = ifelse(is.na(in_top100), 0L, in_top100),
      song_id = as.factor(song_id),
      country = as.factor(country)
    )
  
  ## Share of NMF songs in charts by NMF rank
  result <- nmf %>%
    filter(position <= 20) %>%
    group_by(position) %>%
    summarise(
      percent_in_top200 = mean(in_top200),
      percent_in_top100 = mean(in_top100),
      .groups = "drop"
    ) %>%
    pivot_longer(starts_with("percent"), names_to = "chart", values_to = "percent") %>%
    mutate(
      chart = factor(ifelse(chart == "percent_in_top200", "Top-200", "Top-100"),
                        levels = c("Top-200", "Top-100")),
      position = factor(position, levels = 1:20))
  
  plot1 <- ggplot(result, aes(x = position, y = percent, fill = chart)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "NMF rank (r)",
      y = "share of chart listings") +
    scale_fill_manual(values = c("darkgray", "lightgray")) +
    theme_minimal(base_size = 14, base_family = "Times New Roman") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.title.x = element_text(margin = margin(t = 10)))
  ggsave(paste0("NMF_Rank_and_Charts_", year, ".png"), plot1, width = 10, height = 5, dpi = 300)
  
  ## NMF Rank Effects - Overall (OLS vs Song FE)
  nmf <- nmf %>%
    mutate(position = factor(position)) %>%
    mutate(position = relevel(position, ref = "50"))
  
  model_OLS <- lm(in_top200 ~ position, data = nmf)
  model_FE <- feols(in_top200 ~ position | song_id, data = nmf)
  
  ols_coeff <- coef(model_OLS); ols_coeff <- ols_coeff[grep("^position", names(ols_coeff))]
  fe_coeff <- coef(model_FE);  fe_coeff <- fe_coeff[grep("^position", names(fe_coeff))]
  position_levels <- levels(nmf$position)
  
  result2 <- data.frame(
    position = c(position_levels[position_levels != "50"]),
    OLS = ols_coeff,
    FE = fe_coeff) %>%
    pivot_longer(c("OLS", "FE"), names_to = "model", values_to = "coefficient")
  
  plot2 <- ggplot(result2, aes(x = position, y = coefficient, color = model, linetype = model, group = model)) +
    geom_line(linewidth = 0.7) +
    scale_x_discrete(limits = position_levels[position_levels != "50"]) +
    scale_y_continuous(limits = c(-0.02, 0.9), breaks = seq(0, 0.9, by = 0.1)) +
    labs(
      x = "NMF rank (r)",
      y = expression("estimate (" * hat(beta)[r] * ")"),
      color = NULL,
      linetype = NULL) +
    theme_minimal(base_size = 14, base_family = "Times New Roman") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(margin = margin(t = 10))) +
    scale_color_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    guides(color = guide_legend(reverse = TRUE),
           linetype = guide_legend(reverse = TRUE))
  ggsave(paste0("NMF_Rank_Effects_Overall_", year, ".png"), plot2, width = 10, height = 5, dpi = 300)
  
  ## Regressions (grouped ranks)
  nmf$position_group <- relevel(factor(nmf$position_group), ref = "NMF Rank: 31-50")
  
  model_OLS_grp <- feols(in_top200 ~ position_group | country, data = nmf, cluster = ~ song_id + country)
  
  model_FE_grp  <- feols(in_top200 ~ position_group | country + song_id, data = nmf, cluster = ~ song_id + country)
  
  model_english <- feols(in_top200 ~ position_group | country + song_id,
                         data = nmf %>% filter(country %in% c("us", "gb", "ca")), cluster = ~ song_id + country)
  
  model_spanish <- feols(in_top200 ~ position_group | country + song_id,
                         data = nmf %>% filter(country %in% c("co", "es", "mx")), cluster = ~ song_id + country)
  
  model_domestic <- feols(in_top200 ~ position_group | country + song_id,
                          data = nmf %>% filter(domestic == FALSE), cluster = ~ song_id + country)
  
  model_indies_prev0 <- feols(in_top200 ~ position_group | country + song_id,
                              data = nmf %>% filter(label == "Indie", streams_prev == FALSE), cluster = ~ song_id + country)
  
  models <- list(
    "OLS" = model_OLS_grp,
    "Song FE" = model_FE_grp,
    "US, GB, CA" = model_english,
    "CO, ES, MX" = model_spanish,
    "No Domestic" = model_domestic)
  models[[paste0("Indies w/o ", prev_year, " streams")]] <- model_indies_prev0
  
  coef_labels <- c(
    "position_groupNMF Rank: 1-5" = "NMF Rank: 1-5",
    "position_groupNMF Rank: 6-10" = "NMF Rank: 6-10",
    "position_groupNMF Rank: 11-30" = "NMF Rank: 11-30",
    "position_groupNMF Rank: 31-50" = "NMF Rank: 31-50")
  model_numbers <- paste0("(", seq_along(models), ")")
  
  
  
  baseline_31_50 <- function(df) {
    mean(df$in_top200[df$position_group == "NMF Rank: 31-50"], na.rm = TRUE)
  }
  
  samples <- list(
    "OLS" = nmf,
    "Song FE" = nmf,
    "US, GB, CA" = filter(nmf, country %in% c("us","gb","ca")),
    "CO, ES, MX" = filter(nmf, country %in% c("co","es","mx")),
    "No Domestic" = filter(nmf, domestic == FALSE))
  samples[[paste0("Indies w/o ", prev_year, " streams")]] <-
    filter(nmf, label == "Indie", streams_prev == FALSE)
  
  bp <- vapply(samples, baseline_31_50, numeric(1))
  bp_fmt <- sprintf("%.3f", bp)
  
  add_rows_df <- data.frame(
    term = "NMF Rank: 31–50 (Ref.)",
    t(bp_fmt),
    check.names = FALSE)
  
  
  
  tbl <- modelsummary(models,
                      coef_map = coef_labels,
                      coef_omit = "Intercept",
                      gof_map = c("nobs", "r.squared"),
                      add_rows = add_rows_df,
                      stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                      output = "kableExtra")
  
  tbl <- tbl %>%
    add_header_above(c(" " = 1, setNames(rep(1, length(model_numbers)), model_numbers))) %>%
    kable_classic(full_width = FALSE, font_size = 14, html_font = "Times New Roman")
  
  html_file <- paste0("NMF_Fixed_Effects_Table_", year, ".html")
  png_file <- paste0("NMF_Fixed_Effects_Table_", year, ".png")
  save_kable(tbl, file = html_file)
  webshot(html_file, file = png_file, zoom = 2)
  file.remove(html_file)
}

# saving the NMF-shares table
nmf_shares <- bind_rows(nmf_shares_list) %>%
  arrange(year)

tbl_html <- nmf_shares %>%
  kable(format = "html",
        align = c("c","r","r","r"),
        col.names = c("Year","Listings","Domestic share","Indie share")) %>%
  kable_classic(full_width = FALSE, font_size = 14, html_font = "Times New Roman")

html_file <- "NMF_Shares_Domestic_Indie_2017vs2024.html"
png_file  <- "NMF_Shares_Domestic_Indie_2017vs2024.png"
save_kable(tbl_html, file = html_file)
webshot(html_file, file = png_file, zoom = 2)
file.remove(html_file)