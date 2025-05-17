# --- START FILE: models/03_train_clutch_model_historical_PREDICTIVE.R ---
# <<< REVISED >>> Goal: Train a model to PREDICT player Field Goal Percentage
#                     for an upcoming game based on historical performance and context.
# VERSION: Includes fix for NA labels in XGBoost fit.

# Load Libraries
library(dplyr)
library(tidymodels)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2) # Keep for optional plotting
library(glue)    # For print messages
library(yardstick) # For metrics calculation
library(tidyr)     # Explicitly load for replace_na

# --- Load Data ---
# Define paths relative to project root
player_stats_path <- "data/PlayerStatistics.csv"
games_path <- "data/Games.csv"

print(paste("Loading player data from:", player_stats_path))
if (!file.exists(player_stats_path)) stop("PlayerStatistics.csv not found in data/ directory.")
nbaplayer_data_raw <- readr::read_csv(player_stats_path, show_col_types = FALSE)

print(paste("Loading game data from:", games_path))
if (!file.exists(games_path)) stop("Games.csv not found in data/ directory.")
nba_games_raw <- readr::read_csv(games_path, show_col_types = FALSE)

# --- Rebrand NBA Teams (Define once) ---
# Ensure this matches the map in global.R
rebranded_teams <- c(
  "Minneapolis Lakers" = "Los Angeles Lakers",
  "Ft. Wayne Zollner Pistons" = "Detroit Pistons",
  "Rochester Royals" = "Sacramento Kings",
  "Milwaukee Hawks" = "Atlanta Hawks",
  "Tri-Cities Blackhawks" = "Atlanta Hawks",
  "New Orleans Hornets" = "New Orleans Pelicans",
  "Charlotte Bobcats" = "Charlotte Hornets",
  "Seattle SuperSonics" = "Oklahoma City Thunder",
  "New Jersey Nets" = "Brooklyn Nets",
  "Vancouver Grizzlies" = "Memphis Grizzlies",
  "Syracuse Nationals" = "Philadelphia 76ers",
  "Philadelphia Warriors" = "Golden State Warriors",
  "San Diego Rockets" = "Houston Rockets",
  "Cincinnati Royals" = "Sacramento Kings",
  "Baltimore Bullets" = "Washington Wizards",
  "St. Louis Hawks" = "Atlanta Hawks",
  "San Francisco Warriors" = "Golden State Warriors",
  "Buffalo Braves" = "Los Angeles Clippers",
  "Kansas City-Omaha Kings" = "Sacramento Kings",
  "New Orleans Jazz" = "Utah Jazz",
  "New York Nets" = "Brooklyn Nets",
  "Washington Bullets" = "Washington Wizards",
  "Kansas City Kings" = "Sacramento Kings",
  "San Diego Clippers" = "Los Angeles Clippers"
)

# --- Prepare Player Data ---
print("Preparing Player Data...")
nbaplayer_data_prep <- nbaplayer_data_raw %>%
  mutate(
    gameDate = lubridate::ymd_hms(gameDate), # Parse date
    player_name = str_trim(paste(firstName, lastName)),
    # Rebrand player team and opponent team
    team_name = recode(str_trim(paste(playerteamCity, playerteamName)), !!!rebranded_teams, .default = str_trim(paste(playerteamCity, playerteamName))),
    opponent_name = recode(str_trim(paste(opponentteamCity, opponentteamName)), !!!rebranded_teams, .default = str_trim(paste(opponentteamCity, opponentteamName))),
    # Convert key stats to numeric safely
    across(c(fieldGoalsMade, fieldGoalsAttempted, freeThrowsMade, freeThrowsAttempted,
             points, assists, turnovers, plusMinusPoints, numMinutes),
           ~ suppressWarnings(as.numeric(.)))
  ) %>%
  # Select necessary columns for joining and feature engineering
  select(gameId, personId, player_name, team_name, opponent_name, gameDate, gameType,
         fieldGoalsMade, fieldGoalsAttempted, freeThrowsMade, freeThrowsAttempted,
         points, assists, turnovers, numMinutes) # Removed plusMinusPoints

# --- Prepare Game Data (for Home/Away Info) ---
print("Preparing Game Data...")
nba_games_prep <- nba_games_raw %>%
  mutate(
    # Rebrand the home team from the games data
    game_hometeam = recode(str_trim(paste(hometeamCity, hometeamName)), !!!rebranded_teams, .default = str_trim(paste(hometeamCity, hometeamName)))
  ) %>%
  # Select only gameId and the rebranded game_hometeam
  select(gameId, game_hometeam)

# --- Join Player and Game Data ---
print("Joining Player and Game Data...")
nbaplayer_data_joined <- nbaplayer_data_prep %>%
  left_join(nba_games_prep, by = "gameId") %>%
  filter(!is.na(personId), !is.na(gameDate)) # Ensure key columns are not NA

# <<< REVISED >>> Feature Engineering for PREDICTIVE Clutch Model ---
print("Starting REVISED Feature Engineering for Predictive Clutch Model...")

nbaplayer_data_features <- nbaplayer_data_joined %>%
  arrange(personId, gameDate) %>% # Sort by player, then date is CRUCIAL for lag/cummean
  group_by(personId) %>%         # Group by player for rolling averages
  mutate(
    # --- Target Variable (FG% for the actual game outcome) ---
    # Ensure inputs to division are numeric before calculating
    fieldGoalsMade_num = suppressWarnings(as.numeric(fieldGoalsMade)),
    fieldGoalsAttempted_num = suppressWarnings(as.numeric(fieldGoalsAttempted)),
    clutch_shooting_percentage = ifelse(fieldGoalsAttempted_num > 0, fieldGoalsMade_num / fieldGoalsAttempted_num, 0),
    
    # --- Calculate historical averages *before* the current game using lag() ---
    # Use the numeric versions for calculations
    historical_avg_fg_pct = lag(cummean(ifelse(fieldGoalsAttempted_num > 0, fieldGoalsMade_num / fieldGoalsAttempted_num, NA_real_)), default = NA),
    historical_avg_ft_pct = lag(cummean(ifelse(suppressWarnings(as.numeric(freeThrowsAttempted)) > 0, suppressWarnings(as.numeric(freeThrowsMade)) / suppressWarnings(as.numeric(freeThrowsAttempted)), NA_real_)), default = NA),
    historical_avg_pts = lag(cummean(suppressWarnings(as.numeric(points))), default = NA),
    historical_avg_minutes = lag(cummean(suppressWarnings(as.numeric(numMinutes))), default = NA),
    
    # --- Calculate features based on the *previous* game ---
    momentum_last_game = lag(suppressWarnings(as.numeric(points)), default = 0), # Points in the previous game
    days_rest = as.numeric(difftime(gameDate, lag(gameDate), units = "days")), # Days since last game
    
    # --- Calculate Contextual Features (known pre-game) ---
    home = as.factor(ifelse(team_name == game_hometeam, 1, 0)), # Home status (Factor '1' or '0')
    gameType = factor(gameType)                                 # Game type (Factor)
  ) %>%
  ungroup() %>% # Ungroup after player-specific calculations
  
  # --- Handle NAs introduced by lag/cummean ---
  mutate(
    days_rest = tidyr::replace_na(days_rest, 2), # Impute missing rest days (e.g., with 2)
    # Impute historical averages - using 0 or a typical value might be okay for first games
    historical_avg_fg_pct = tidyr::replace_na(historical_avg_fg_pct, 0.45),
    historical_avg_ft_pct = tidyr::replace_na(historical_avg_ft_pct, 0.75),
    historical_avg_pts = tidyr::replace_na(historical_avg_pts, 0),
    historical_avg_minutes = tidyr::replace_na(historical_avg_minutes, 0)
    # Note: The recipe step_impute_median below provides another layer of imputation
  ) %>%
  
  # Ensure target variable is bounded [0, 1]
  mutate(clutch_shooting_percentage = pmin(pmax(clutch_shooting_percentage, 0), 1)) %>%
  
  # --- Select FINAL Predictors for the *Predictive* Model ---
  select(
    # IDs/Info (will get ID role in recipe)
    personId, player_name, team_name, opponent_name, gameDate,
    # Target
    clutch_shooting_percentage,
    # PREDICTORS
    home,                       # Factor '1' or '0'
    gameType,                   # Factor 'Regular Season'/'Playoffs'
    days_rest,                  # Numeric
    historical_avg_fg_pct,      # Numeric (calculated from past games)
    historical_avg_ft_pct,      # Numeric (calculated from past games)
    historical_avg_pts,         # Numeric (calculated from past games)
    historical_avg_minutes,     # Numeric (calculated from past games)
    momentum_last_game          # Numeric (points from previous game)
  ) %>%
  # --- !!! ADDED FILTERING STEP for outcome variable !!! ---
  filter(!is.na(clutch_shooting_percentage)) %>%
  # --- End Added Step ---
  distinct()

print(glue::glue("Finished REVISED Feature Engineering. Resulting dimensions after filtering NA outcomes: {nrow(nbaplayer_data_features)} rows, {ncol(nbaplayer_data_features)} columns."))

# Check if data frame is empty after processing
if(nrow(nbaplayer_data_features) == 0) {
  stop("No data remaining after cleaning, joining, feature engineering, AND filtering NA outcomes. Check filters and data.")
}

# --- Data Split ---
print("Splitting data into training and testing sets...")
set.seed(2025) # Ensure reproducibility
# Stratifying might be less effective if filtering removed many rows, but generally okay
clutch_split <- initial_split(nbaplayer_data_features, prop = 0.7, strata = clutch_shooting_percentage)
clutch_train <- training(clutch_split)
clutch_test <- testing(clutch_split)
print(glue::glue("Training set: {nrow(clutch_train)} rows. Testing set: {nrow(clutch_test)} rows."))

# Check if training data is empty after split
if(nrow(clutch_train) == 0) {
  stop("Training data is empty after splitting. Check the proportion and filtering steps.")
}

# <<< REVISED >>> Recipe for Predictive Model ---
print("Defining REVISED Recipe...")
clutch_recipe_revised <-
  # --- Use 'outcome ~ .' formula to include all columns initially ---
  recipe(clutch_shooting_percentage ~ ., data = clutch_train) %>%
  # --- Now, explicitly define the roles for non-predictors/outcome ---
  update_role(personId, player_name, team_name, opponent_name, gameDate, new_role = "ID") %>%
  # --- Keep the subsequent preprocessing steps ---
  step_impute_median(all_numeric_predictors()) %>% # Impute any remaining NAs in numeric predictors
  step_dummy(all_nominal_predictors()) %>%         # Convert factors (home, gameType) to dummies
  step_zv(all_predictors()) %>%                   # Remove zero-variance predictors
  step_normalize(all_numeric_predictors())        # Normalize numeric predictors

print("REVISED Recipe defined successfully.")


# --- Model Definition (Keep as before) ---
print("Defining XGBoost regression model...")
clutch_model <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")


# <<< REVISED >>> Workflow ---
print("Creating REVISED Workflow...")
clutch_wf_revised <- workflow() %>%
  add_recipe(clutch_recipe_revised) %>% # Use the revised recipe
  add_model(clutch_model)


# <<< REVISED >>> Fit REVISED Workflow ---
print("Fitting REVISED Clutch Model Workflow...")
# Add a tryCatch specifically around fit to provide more context if it fails again
tryCatch({
  clutch_fit_revised <- fit(clutch_wf_revised, data = clutch_train)
  print("REVISED Model fitting complete.")
}, error = function(e) {
  print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  print("ERROR occurred during the fit() step:")
  print(e$message)
  print("Inspect the 'clutch_train' data frame for potential issues.")
  print("Check for remaining NAs or unexpected values in predictors/outcome.")
  print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  # Optionally re-throw the error if you want the script to stop
  # stop("Model fitting failed.")
})

# Check if fit was successful before proceeding
if (!exists("clutch_fit_revised")) {
  stop("Model fitting object 'clutch_fit_revised' was not created due to errors.")
}


# --- Predict & Evaluate (Using REVISED Model) ---
print("Making predictions on the test set...")
clutch_preds_revised <- predict(clutch_fit_revised, clutch_test) %>%
  bind_cols(clutch_test %>% select(player_name, team_name, opponent_name, clutch_shooting_percentage)) # Select relevant columns from test set

print("Evaluating REVISED model performance...")
clutch_metrics_revised <- metrics(clutch_preds_revised, truth = clutch_shooting_percentage, estimate = .pred)

print("--- REVISED Clutch Model Evaluation Metrics ---")
print(clutch_metrics_revised)
# Extract specific metrics if needed
clutch_rmse_revised <- clutch_metrics_revised %>% filter(.metric == "rmse") %>% pull(.estimate)
clutch_rsq_revised <- clutch_metrics_revised %>% filter(.metric == "rsq") %>% pull(.estimate)
print(paste("REVISED Clutch RMSE:", round(clutch_rmse_revised, 3)))
print(paste("REVISED Clutch R²:", round(clutch_rsq_revised, 3)))


# --- Optional: Visualization: Predicted vs Actual (REVISED) ---
# plot_clutch_revised <- ggplot(clutch_preds_revised, aes(x = .pred, y = clutch_shooting_percentage)) +
#   geom_point(alpha = 0.3, color = "darkorange") + # Adjusted color/alpha
#   geom_abline(color = "blue", linetype = "dashed") +
#   coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + # Set axis limits for percentage
#   labs(
#     title = "REVISED Model: Predicted vs Actual Clutch Shooting Percentage",
#     subtitle = "Predicting Game FG% based on historical data",
#     x = "Predicted Clutch %",
#     y = "Actual Clutch %"
#   ) +
#   theme_minimal() +
#   theme(aspect.ratio=1) # Make plot square
#
# print("Generating plot (may take a moment)...")
# # Consider saving plot instead of printing if running non-interactively
# # ggsave("clutch_revised_pred_vs_actual.png", plot = plot_clutch_revised, width = 6, height = 6)
# print(plot_clutch_revised)


# <<< REVISED >>> Save the REVISED Fitted Workflow ---
print("Saving REVISED Clutch Model...")
# Assign the REVISED fitted object
fitted_object_to_save <- clutch_fit_revised

# Define a NEW filename to distinguish from the original explanatory model
output_filename <- "clutch_model_predictive_fit.rds" # <<< CHANGED FILENAME

# Define the target directory (remains the same)
target_dir_name <- "ml_results"
target_dir_path <- file.path(target_dir_name)

# Ensure Directory Exists (remains the same)
if (!dir.exists(target_dir_path)) {
  print(paste("Creating directory:", target_dir_path))
  dir.create(target_dir_path, recursive = TRUE, showWarnings = FALSE)
}

# Define the full save path (uses new filename)
save_path <- file.path(target_dir_path, output_filename)

# Save the R object (using revised object and path)
if (exists("fitted_object_to_save")) {
  print(paste("Saving REVISED object to:", save_path))
  tryCatch({
    saveRDS(fitted_object_to_save, file = save_path)
    print(paste("Successfully saved REVISED model:", basename(save_path), "to", target_dir_name))
  }, error = function(e) {
    warning(paste("ERROR saving REVISED object:", e$message))
  })
} else {
  # This warning should now only appear if the fit() step explicitly failed and stopped
  warning(paste("REVISED object 'fitted_object_to_save' (clutch_fit_revised) not found. File not saved to", save_path))
}

print("--- Finished REVISED Script: 03_train_clutch_model_historical_PREDICTIVE.R ---")
# --- END FILE: models/03_train_clutch_model_historical_PREDICTIVE.R ---