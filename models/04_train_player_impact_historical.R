# --- START FILE: models/04_train_player_impact_historical_PREDICTIVE.R ---
# <<< REVISED >>> Goal: Train a model to PREDICT player plusMinusPoints
#                     for an upcoming game based on historical performance and context.

# Load Libraries
library(dplyr)
library(tidymodels)
library(lubridate)
library(readr)
library(stringr)
library(glue)
library(yardstick) # For evaluation metrics

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
    across(c(fieldGoalsMade, fieldGoalsAttempted, threePointersMade, threePointersAttempted,
             freeThrowsMade, freeThrowsAttempted, points, assists, reboundsTotal,
             reboundsOffensive, reboundsDefensive, steals, blocks, turnovers,
             foulsPersonal, plusMinusPoints, numMinutes),
           ~ suppressWarnings(as.numeric(.)))
  ) %>%
  # Select necessary columns for joining and feature engineering
  select(gameId, personId, player_name, team_name, opponent_name, gameDate, gameType,
         fieldGoalsMade, fieldGoalsAttempted, threePointersMade, threePointersAttempted,
         freeThrowsMade, freeThrowsAttempted,
         points, assists, reboundsTotal, steals, blocks, turnovers, plusMinusPoints, numMinutes)

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
  filter(!is.na(personId), !is.na(gameDate), !is.na(plusMinusPoints)) # Ensure key columns & target are not NA

# <<< REVISED >>> Feature Engineering for PREDICTIVE Player Impact Model ---
print("Starting REVISED Feature Engineering for Predictive Player Impact Model...")

nbaplayer_data_features <- nbaplayer_data_joined %>%
  # Select columns needed for target and calculating historical features
  select(gameId, personId, player_name, team_name, opponent_name, gameDate, gameType,
         plusMinusPoints, # Target variable
         fieldGoalsMade, fieldGoalsAttempted, threePointersMade, threePointersAttempted,
         freeThrowsMade, freeThrowsAttempted,
         points, assists, reboundsTotal, steals, blocks, turnovers, numMinutes,
         game_hometeam
  ) %>%
  arrange(personId, gameDate) %>% # Sort is crucial for lag/cummean
  group_by(personId) %>%
  mutate(
    # --- Calculate historical averages *before* the current game ---
    historical_avg_pm = lag(cummean(plusMinusPoints), default = NA), # Historical +/-
    historical_avg_pts = lag(cummean(points), default = NA),
    historical_avg_ast = lag(cummean(assists), default = NA),
    historical_avg_reb = lag(cummean(reboundsTotal), default = NA),
    historical_avg_stl = lag(cummean(steals), default = NA),
    historical_avg_blk = lag(cummean(blocks), default = NA),
    historical_avg_tov = lag(cummean(turnovers), default = NA),
    historical_avg_minutes = lag(cummean(numMinutes), default = NA),
    historical_avg_fg_pct = lag(cummean(ifelse(fieldGoalsAttempted > 0, fieldGoalsMade / fieldGoalsAttempted, NA_real_)), default = NA),
    historical_avg_3p_pct = lag(cummean(ifelse(threePointersAttempted > 0, threePointersMade / threePointersAttempted, NA_real_)), default = NA),
    historical_avg_ft_pct = lag(cummean(ifelse(freeThrowsAttempted > 0, freeThrowsMade / freeThrowsAttempted, NA_real_)), default = NA),
    
    # --- Calculate features based on the *previous* game ---
    momentum_last_game_pm = lag(plusMinusPoints, default = 0), # +/- from previous game
    days_rest = as.numeric(difftime(gameDate, lag(gameDate), units = "days")),
    
    # --- Calculate Contextual Features ---
    home_game = as.factor(ifelse(team_name == game_hometeam, 1, 0)),
    gameType = factor(gameType)
  ) %>%
  ungroup() %>%
  
  # --- Handle NAs (impute or drop) ---
  mutate(
    days_rest = tidyr::replace_na(days_rest, 2),
    # Impute historical averages (using 0 might be reasonable for +/- and counting stats)
    historical_avg_pm = tidyr::replace_na(historical_avg_pm, 0),
    historical_avg_pts = tidyr::replace_na(historical_avg_pts, 0),
    historical_avg_ast = tidyr::replace_na(historical_avg_ast, 0),
    historical_avg_reb = tidyr::replace_na(historical_avg_reb, 0),
    historical_avg_stl = tidyr::replace_na(historical_avg_stl, 0),
    historical_avg_blk = tidyr::replace_na(historical_avg_blk, 0),
    historical_avg_tov = tidyr::replace_na(historical_avg_tov, 0),
    historical_avg_minutes = tidyr::replace_na(historical_avg_minutes, 0),
    # Impute percentages with defaults
    historical_avg_fg_pct = tidyr::replace_na(historical_avg_fg_pct, 0.45),
    historical_avg_3p_pct = tidyr::replace_na(historical_avg_3p_pct, 0.35),
    historical_avg_ft_pct = tidyr::replace_na(historical_avg_ft_pct, 0.75)
  ) %>%
  
  # --- Select FINAL Predictors for the *Predictive* Model ---
  select(
    # IDs/Info
    personId, player_name, team_name, opponent_name, gameDate,
    # Target
    plusMinusPoints,
    # PREDICTORS (Features known *before* the game)
    home_game,
    gameType,
    days_rest,
    momentum_last_game_pm,
    historical_avg_pm,
    historical_avg_pts,
    historical_avg_ast,
    historical_avg_reb,
    historical_avg_stl,
    historical_avg_blk,
    historical_avg_tov,
    historical_avg_minutes,
    historical_avg_fg_pct,
    historical_avg_3p_pct,
    historical_avg_ft_pct
    # Add opponent defense rating etc. here if implemented
  ) %>%
  distinct()

print(glue::glue("Finished REVISED Feature Engineering for Impact Model. Resulting dimensions: {nrow(nbaplayer_data_features)} rows, {ncol(nbaplayer_data_features)} columns."))

if(nrow(nbaplayer_data_features) == 0) {
  stop("No data remaining after REVISED Impact Model feature engineering.")
}

# --- Data Splitting ---
print("Splitting data into training and testing sets...")
set.seed(42) # Keep seed consistent if desired
# Stratifying on plusMinusPoints might be useful if it's very skewed
data_split <- initial_split(nbaplayer_data_features, prop = 0.7, strata = plusMinusPoints)
data_train <- training(data_split)
data_test <- testing(data_split)
print(glue::glue("Training set: {nrow(data_train)} rows. Testing set: {nrow(data_test)} rows."))


# <<< REVISED >>> Recipe for Predictive Impact Model ---
print("Defining REVISED Recipe for Impact Model...")
impact_recipe_revised <-
  # Use 'outcome ~ .' formula
  recipe(plusMinusPoints ~ ., data = data_train) %>%
  # Update roles for ID variables
  update_role(personId, player_name, team_name, opponent_name, gameDate, new_role = "ID") %>%
  # Preprocessing steps:
  step_impute_median(all_numeric_predictors()) %>% # Impute any remaining NAs
  step_dummy(all_nominal_predictors()) %>%         # Handle home_game, gameType
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

print("REVISED Impact Recipe defined.")


# --- Model Definition (Keep as before) ---
print("Defining XGBoost regression model...")
impact_model <- boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost")


# <<< REVISED >>> Workflow ---
print("Creating REVISED Impact Workflow...")
impact_wf_revised <- workflow() %>%
  add_recipe(impact_recipe_revised) %>% # Use revised recipe
  add_model(impact_model)


# <<< REVISED >>> Fit REVISED Workflow ---
print("Fitting REVISED Player Impact Model Workflow...")
impact_fit_revised <- fit(impact_wf_revised, data = data_train)
print("REVISED Impact Model fitting complete.")


# --- Predict & Evaluate (Using REVISED Model) ---
print("Making predictions on the test set...")
impact_predictions_revised <- predict(impact_fit_revised, data_test) %>%
  bind_cols(data_test %>% select(player_name, team_name, opponent_name, plusMinusPoints)) # Select relevant test cols

print("Evaluating REVISED Impact model performance...")
impact_metrics_revised <- metrics(impact_predictions_revised, truth = plusMinusPoints, estimate = .pred)

print("--- REVISED Impact Model Evaluation Metrics ---")
print(impact_metrics_revised)
# Extract specific metrics
impact_rmse_revised <- impact_metrics_revised %>% filter(.metric == "rmse") %>% pull(.estimate)
impact_rsq_revised <- impact_metrics_revised %>% filter(.metric == "rsq") %>% pull(.estimate)
impact_mae_revised <- impact_metrics_revised %>% filter(.metric == "mae") %>% pull(.estimate)
print(paste("REVISED Impact RMSE:", round(impact_rmse_revised, 3)))
print(paste("REVISED Impact R²:", round(impact_rsq_revised, 3)))
print(paste("REVISED Impact MAE:", round(impact_mae_revised, 3)))


# --- Optional: Visualization (Predicted vs Actual) ---
# plot_impact_revised <- ggplot(impact_predictions_revised, aes(x = .pred, y = plusMinusPoints)) +
#   geom_point(alpha = 0.1, color = "firebrick") + # Use low alpha due to potentially many points
#   geom_abline(color = "blue", linetype = "dashed") +
#   labs(
#     title = "REVISED Model: Predicted vs Actual Player Plus/Minus",
#     subtitle = "Predicting Game +/- based on historical data",
#     x = "Predicted Plus/Minus",
#     y = "Actual Plus/Minus"
#   ) +
#   theme_minimal()
# print("Generating plot...")
# print(plot_impact_revised)


# <<< REVISED >>> Save the REVISED Fitted Workflow ---
print("Saving REVISED Impact Model...")
fitted_object_to_save <- impact_fit_revised
# Use a distinct filename
output_filename <- "player_impact_predictive_fit.rds" # <<< CHANGED FILENAME

target_dir_name <- "ml_results"
target_dir_path <- file.path(target_dir_name)

if (!dir.exists(target_dir_path)) {
  print(paste("Creating directory:", target_dir_path))
  dir.create(target_dir_path, recursive = TRUE, showWarnings = FALSE)
}

save_path <- file.path(target_dir_path, output_filename)

if (exists("fitted_object_to_save")) {
  print(paste("Saving REVISED object to:", save_path))
  tryCatch({
    saveRDS(fitted_object_to_save, file = save_path)
    print(paste("Successfully saved REVISED model:", basename(save_path), "to", target_dir_name))
  }, error = function(e) {
    warning(paste("ERROR saving REVISED object:", e$message))
  })
} else {
  warning(paste("REVISED object 'fitted_object_to_save' (impact_fit_revised) not found. File not saved to", save_path))
}

print("--- Finished REVISED Script: 04_train_player_impact_historical_PREDICTIVE.R ---")
# --- END FILE: models/04_train_player_impact_historical_PREDICTIVE.R ---