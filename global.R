# nba-dashboard/global.R

print("--- global.R: Execution Started ---")

print("--- global.R: Loading Core Packages ---")
library(shiny)
library(bslib)          # For bs_theme, card, etc.
library(dplyr)          # For data manipulation
library(purrr)          # For pmap, %||%
library(httr)           # For web scrapers
library(rvest)          # For web scrapers
library(stringr)        # For string manipulation
library(glue)           # For string formatting (e.g., in scrapers)
library(shinycssloaders)# For spinners
library(scales)         # For number_format, percent_format
library(htmltools)      # For HTML tag manipulation
library(bsicons)     
library(fmsb)           # For Player Comparison radar charts
library(DT)             # For Player Comparison tables
library(tidyr)          # For Player Comparison pivot_longer & GOAT Discourse
library(readr)          # For GOAT Discourse & Historical Trends (read_csv)
library(lubridate)      # For GOAT Discourse & Historical Trends (dates)
library(plotly)         # For Historical Trends & GOAT Discourse (potentially)
library(echarts4r)      # For GOAT Discourse (sentiment donut)
library(tidytext)       # For GOAT Discourse (NLP)
library(textdata)       # For GOAT Discourse (sentiment lexicons)
library(wordcloud)      # For GOAT Discourse (comparison word clouds)
library(wordcloud2)     # For GOAT Discourse (individual word clouds)
library(zoo)            # For rollapplyr in calculate_team_features
library(ggplot2)        # For probability_bar_plot 

print("--- global.R: Core packages loaded ---")

FALLBACK_IMAGE_URL <- "nba-logo.png" 
print("--- global.R: Global variables defined ---")

print("--- global.R: Loading player stats CSV and URL mapping CSV ---")
nba_stats_df_path <- file.path(getwd(), "data", "24-25_nba-stats.csv") 
player_url_map_path <- file.path(getwd(), "data", "nba_player_url_mapping.csv")

if (file.exists(nba_stats_df_path)) {
  nba_stats_df_global <- read.csv(nba_stats_df_path, stringsAsFactors = FALSE, check.names = FALSE)
  if ("Data" %in% names(nba_stats_df_global)) {
    names(nba_stats_df_global)[names(nba_stats_df_global) == "Data"] <- "GameDate"
  }
  if ("GameDate" %in% names(nba_stats_df_global)) {
    nba_stats_df_global$GameDate <- tryCatch(as.Date(nba_stats_df_global$GameDate), error = function(e) nba_stats_df_global$GameDate)
  }
  numeric_cols_to_check <- c("MP", "FG", "FGA", "FG%", "3P", "3PA", "3P%", "FT", "FTA", "FT%", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc")
  for (col in numeric_cols_to_check) {
    if (col %in% names(nba_stats_df_global)) {
      nba_stats_df_global[[col]] <- suppressWarnings(as.numeric(as.character(nba_stats_df_global[[col]])))
    }
  }
  print(paste("Loaded nba_stats_df_global with", nrow(nba_stats_df_global), "rows."))
} else {
  warning(paste("CRITICAL: nba_stats_df_global CSV (24-25_nba-stats.csv) not found at", nba_stats_df_path))
  nba_stats_df_global <- data.frame() 
}

if (file.exists(player_url_map_path)) {
  player_url_map_global <- read.csv(player_url_map_path, stringsAsFactors = FALSE)
  print(paste("Loaded player_url_map_global with", nrow(player_url_map_global), "rows."))
} else {
  warning(paste("CRITICAL: player_url_map_global CSV not found at", player_url_map_path))
  player_url_map_global <- data.frame() 
}

#___________TEAM AESTHETICS DATA_________
team_aesthetics_global <- data.frame(
  stringsAsFactors = FALSE,
  Tm = c("LAL", "BOS", "GSW", "BKN", "MIL", "PHI", "DEN", "PHX", "DAL", "MIA", "NYK", "CHI", "POR", "TOR", "UTA", "LAC", "ATL", "CHA", "CLE", "DET", "HOU", "IND", "MEM", "MIN", "NOP", "OKC", "ORL", "SAC", "SAS", "WAS"),
  PrimaryColor = c("#552583", "#008348", "#006BB6", "#000000", "#00471B", "#ED174C", "#0E2240", "#1D1160", "#0053BC", "#98002E", "#006BB6", "#CE1141", "#E03A3E", "#CE1141", "#002B5C", "#C8102E", "#E03A3E", "#00788C", "#6F263D", "#C8102E", "#CE1141", "#002D62", "#5D76A9", "#0C2340", "#002B5C", "#007AC1", "#0077C0", "#5A2B81", "#000000", "#002B5C"),
  SecondaryColor = c("#FDB927", "#FFFFFF", "#FDB927", "#FFFFFF", "#EEE1C6", "#006BB6", "#FEC524", "#E56020", "#002B5C", "#F9A01B", "#F58426", "#000000", "#000000", "#000000", "#F9A01B", "#1D428A", "#C1D32F", "#1D1160", "#FDBB30", "#006BB6", "#000000", "#FDBB30", "#12173F", "#236192", "#C8102E", "#EF3B24", "#C4CED3", "#63727A", "#000000", "#E31837"),
  LogoURL = c("https://cdn.nba.com/logos/nba/1610612747/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612738/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612744/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612751/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612749/global/L/logo.svg","https://upload.wikimedia.org/wikipedia/en/thumb/0/0e/Philadelphia_76ers_logo.svg/1200px-Philadelphia_76ers_logo.svg.png","https://cdn.nba.com/logos/nba/1610612743/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612756/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612742/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612748/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612752/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612741/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612757/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612761/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612762/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612746/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612737/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612766/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612739/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612765/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612745/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612754/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612763/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612750/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612740/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612760/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612753/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612758/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612759/global/L/logo.svg","https://cdn.nba.com/logos/nba/1610612764/global/L/logo.svg"),
  TextColor = c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#000000", "#FFFFFF")
)
print("--- global.R: team_aesthetics_global defined ---")


data_fetchers_file <- file.path(getwd(), "R", "utils", "data_fetchers.R")
if (file.exists(data_fetchers_file)) {
  print(paste("global.R: Sourcing utility file:", data_fetchers_file))
  tryCatch(source(data_fetchers_file, local = FALSE), 
           error = function(e) warning(paste("global.R: ERROR sourcing", data_fetchers_file, ":", e$message)))
} else {
  warning(paste("global.R: CRITICAL - data_fetchers.R not found in R/utils/"))
}
print("--- global.R: Utility functions sourced (attempted) ---")



#_______SOURCING MODULES_______
module_overview_file <- file.path(getwd(), "modules", "mod_overview.R")
if (file.exists(module_overview_file)) {
  print(paste("global.R: Sourcing module: mod_overview from", module_overview_file))
  tryCatch({
    source(module_overview_file, local = FALSE)
    if (!exists("overviewTabUI", mode = "function") || !exists("overviewTabServer", mode = "function")) {
      warning("global.R: overviewTabUI() or overviewTabServer() NOT found after sourcing mod_overview.R.")
    }
  }, error = function(e) {
    warning(paste("global.R: CRITICAL ERROR sourcing mod_overview.R:", e$message))
  })
} else {
  stop("CRITICAL: mod_overview.R not found. App cannot start.")
}


module_player_deep_dive_file <- file.path(getwd(), "modules", "mod_player_deep_dive.R")
if (file.exists(module_player_deep_dive_file)) {
  print(paste("global.R: Sourcing module: mod_player_deep_dive from", module_player_deep_dive_file))
  tryCatch({
    source(module_player_deep_dive_file, local = FALSE)
    if (!exists("playerDeepDiveTabUI", mode = "function") || !exists("playerDeepDiveTabServer", mode = "function")) {
      warning("global.R: playerDeepDiveTabUI() or playerDeepDiveTabServer() NOT found after sourcing mod_player_deep_dive.R.")
    }
  }, error = function(e) {
    warning(paste("global.R: CRITICAL ERROR sourcing mod_player_deep_dive.R:", e$message))
  })
} else {
  stop("CRITICAL: mod_player_deep_dive.R not found. App cannot start.")
}
module_player_comparison_file <- file.path(getwd(), "modules", "mod_player_comparison.R")
if (file.exists(module_player_comparison_file)) {
  print(paste("global.R: Sourcing module: mod_player_comparison from", module_player_comparison_file))
  tryCatch({
    source(module_player_comparison_file, local = FALSE)
    if (!exists("playerComparisonTabUI", mode = "function") || !exists("playerComparisonTabServer", mode = "function")) {
      warning("global.R: playerComparisonTabUI() or playerComparisonTabServer() NOT found after sourcing mod_player_comparison.R.")
    }
  }, error = function(e) {
    warning(paste("global.R: CRITICAL ERROR sourcing mod_player_comparison.R:", e$message))
  })
} else {
  stop("CRITICAL: mod_player_comparison.R not found. App cannot start.")
}


module_historical_trends_file <- file.path(getwd(), "modules", "mod_historical_trends.R")
if (file.exists(module_historical_trends_file)) {
  print(paste("global.R: Sourcing module: mod_historical_trends from", module_historical_trends_file))
  tryCatch({
    source(module_historical_trends_file, local = FALSE) 
    if (!exists("mod_historical_trends_ui", mode = "function") || !exists("mod_historical_trends_server", mode = "function")) {
      warning("global.R: mod_historical_trends_ui() or mod_historical_trends_server() NOT found after sourcing.")
    }
  }, error = function(e) {
    warning(paste("global.R: CRITICAL ERROR sourcing mod_historical_trends.R:", e$message))
  })
} else {
  warning("global.R: mod_historical_trends.R not found. This tab will not be available.")
}


module_goat_discourse_file <- file.path(getwd(), "modules", "mod_goat_discourse.R")
if (file.exists(module_goat_discourse_file)) {
  print(paste("global.R: Sourcing module: mod_goat_discourse from", module_goat_discourse_file))
  tryCatch({
    source(module_goat_discourse_file, local = FALSE)
    if (!exists("mod_goat_discourse_ui", mode = "function") || !exists("mod_goat_discourse_server", mode = "function")) {
      warning("global.R: mod_goat_discourse_ui() or mod_goat_discourse_server() NOT found after sourcing mod_goat_discourse.R.")
    }
  }, error = function(e) {
    warning(paste("global.R: CRITICAL ERROR sourcing mod_goat_discourse.R:", e$message))
  })
} else {
  stop("CRITICAL: mod_goat_discourse.R not found. App cannot start if this module is required.")
}

module_about_file <- file.path(getwd(), "modules", "mod_about.R")
if (file.exists(module_about_file)) {
  print(paste("global.R: Sourcing module: mod_about from", module_about_file))
  tryCatch({
    source(module_about_file, local = FALSE)
    if (!exists("mod_about_ui", mode = "function") || !exists("mod_about_server", mode = "function")) {
      warning("global.R: mod_about_ui() or mod_about_server() NOT found after sourcing mod_about.R.")
    }
  }, error = function(e) {
    warning(paste("global.R: CRITICAL ERROR sourcing mod_about.R:", e$message))
  })
} else {
  warning("WARNING: mod_about.R file not found. 'About' tab will not be available.")
}

module_predictor_file <- file.path(getwd(), "modules", "mod_predictor.R")
if (file.exists(module_predictor_file)) {
  print(paste("global.R: Sourcing module: mod_predictor from", module_predictor_file))
  tryCatch({
    source(module_predictor_file, local = FALSE) 
    if (!exists("mod_predictor_ui", mode = "function") || !exists("mod_predictor_server", mode = "function")) {
      warning("global.R: mod_predictor_ui() or mod_predictor_server() NOT found after sourcing mod_predictor.R.")
    }
  }, error = function(e) {
    warning(paste("global.R: CRITICAL ERROR sourcing mod_predictor.R:", e$message))
  })
} else {
  warning("WARNING: mod_predictor.R file not found. 'NBA Predictor' tab will not be available.")
}

print("--- global.R: Modules sourced ---")
print("--- global.R: Execution Finished ---")