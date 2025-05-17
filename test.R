# Load the library
library(hoopR)
library(dplyr) # Often useful for viewing/manipulating data

# Get base player stats for the 2023-24 Regular Season
# The 'year' often corresponds to the start year of the season, e.g., 2024 for 2023-24
# Let's use 2024 based on typical API conventions
current_season <- 2024 # For the 2023-24 season

print(paste("Fetching NBA player stats for season:", current_season))

# Using a tryCatch block is good practice for API calls
all_player_stats_nba_api <- tryCatch({
  nba_leaguedashplayerstats(
    season = year_to_season(current_season), # Format like "2023-24"
    season_type = "Regular Season",
    measure_type = "Base" # Options: "Base", "Advanced", "Misc", "Scoring", "Usage", "Defense"
    # Add other parameters if needed (e.g., per_mode = "PerGame")
    # Check hoopR::nba_leaguedashplayerstats documentation for all options
  )
}, error = function(e) {
  message("Error fetching data: ", e$message)
  return(NULL)
})

# Check if data was fetched and display the first few rows
if (!is.null(all_player_stats_nba_api)) {
  # The result is often a list containing the data frame
  # Look for the element named 'LeagueDashPlayerStats' or similar
  print(names(all_player_stats_nba_api))
  
  # Assuming the main data frame is named 'LeagueDashPlayerStats'
  # Adjust the name if needed based on the printed names above
  if ("LeagueDashPlayerStats" %in% names(all_player_stats_nba_api)) {
    stats_df <- all_player_stats_nba_api[["LeagueDashPlayerStats"]]
    stats_df <- stats_df %>%
      # Select some key columns for display, convert types if needed
      select(PLAYER_NAME, TEAM_ABBREVIATION, AGE, GP, MIN, PTS, REB, AST, STL, BLK, FG_PCT, FG3_PCT, FT_PCT) %>%
      arrange(desc(PTS)) # Sort by points
    
    print(head(stats_df))
    print(paste("Successfully fetched stats for", nrow(stats_df), "players."))
  } else {
    message("Could not find the 'LeagueDashPlayerStats' data frame in the result.")
    # Print the structure to help debug
    # str(all_player_stats_nba_api)
  }
  
} else {
  message("Failed to retrieve player stats.")
}
