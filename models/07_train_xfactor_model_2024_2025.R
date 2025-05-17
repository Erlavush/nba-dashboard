# --- START FILE: models/07_train_xfactor_model_2024_2025.R ---
# NBA XFactor Player Predictor (Original Explanatory Version)
# ---
# WARNING: This model trains using post-game stats (including components
#          of the target variable) as predictors. It explains correlations
#          within a game but is NOT suitable for pre-game prediction due
#          to data leakage. It also uses only 2024-25 season data.
# ---

# Load Libraries
library(dplyr)
library(tidymodels)
library(lubridate)
library(readr)
library(stringr)
library(ggplot2)
library(zoo)     # For rollapply
library(vip)     # For variable importance
library(yardstick) # For evaluation metrics
library(glue)    # For print messages

# --- Load Data ---
player_stats_path <- "data/PlayerStatistics.csv"
games_path <- "data/Games.csv"

print(paste("Loading player data from:", player_stats_path))
if (!file.exists(player_stats_path)) stop("PlayerStatistics.csv not found in data/ directory.")
nbaplayer_data_raw <- readr::read_csv(player_stats_path, show_col_types = FALSE)

print(paste("Loading game data from:", games_path))
if (!file.exists(games_path)) stop("Games.csv not found in data/ directory.")
nba_games_raw <- readr::read_csv(games_path, show_col_types = FALSE)

# --- Rebrand NBA Teams ---
rebranded_teams <- c(
  "Minneapolis Lakers" = "Los Angeles Lakers", "Ft. Wayne Zollner Pistons" = "Detroit Pistons",
  "Rochester Royals" = "Sacramento Kings", "Milwaukee Hawks" = "Atlanta Hawks",
  "Tri-Cities Blackhawks" = "Atlanta Hawks", "New Orleans Hornets" = "New Orleans Pelicans",
  "Charlotte Bobcats" = "Charlotte Hornets", "Seattle SuperSonics" = "Oklahoma City Thunder",
  "New Jersey Nets" = "Brooklyn Nets", "Vancouver Grizzlies" = "Memphis Grizzlies",
  "Syracuse Nationals" = "Philadelphia 76ers", "Philadelphia Warriors" = "Golden State Warriors",
  "San Diego Rockets" = "Houston Rockets", "Cincinnati Royals" = "Sacramento Kings",
  "Baltimore Bullets" = "Washington Wizards", "St. Louis Hawks" = "Atlanta Hawks",
  "San Francisco Warriors" = "Golden State Warriors", "Buffalo Braves" = "Los Angeles Clippers",
  "Kansas City-Omaha Kings" = "Sacramento Kings", "New Orleans Jazz" = "Utah Jazz",
  "New York Nets" = "Brooklyn Nets", "Washington Bullets" = "Washington Wizards",
  "Kansas City Kings" = "Sacramento Kings", "San Diego Clippers" = "Los Angeles Clippers"
)

# --- Prepare and Filter Player Data (2024-25 Season ONLY) ---
print("Preparing and Filtering Player Data (2024-25 Season)...")
nbaplayer_data_prep <- nbaplayer_data_raw %>%
  mutate(
    gameDate = lubridate::ymd_hms(gameDate),
    player_name = str_trim(paste(firstName, lastName)),
    team_name = recode(str_trim(paste(playerteamCity, playerteamName)), !!!rebranded_teams, .default = str_trim(paste(playerteamCity, playerteamName))),
    opponent_name = recode(str_trim(paste(opponentteamCity, opponentteamName)), !!!rebranded_teams, .default = str_trim(paste(opponentteamCity, opponentteamName))),
    # Convert stats to numeric
    across(c(fieldGoalsMade, fieldGoalsAttempted, freeThrowsMade, freeThrowsAttempted,
             points, assists, reboundsTotal, plusMinusPoints, steals, blocks, turnovers,
             numMinutes),
           ~ suppressWarnings(as.numeric(.)))
  ) %>%
  filter(gameDate >= ymd("2024-10-01")) %>% # <<< CRITICAL FILTER: Only 2024-25 data
  # Select necessary columns for features and joins
  select(gameId, personId, player_name, team_name, opponent_name, gameDate, gameType,
         fieldGoalsMade, fieldGoalsAttempted, freeThrowsMade, freeThrowsAttempted,
         points, assists, reboundsTotal, plusMinusPoints, steals, blocks, turnovers,
         numMinutes)

# Check if data exists after filtering
if(nrow(nbaplayer_data_prep) == 0) {
  stop("No player data found for the 2024-2025 season filter (gameDate >= 2024-10-01). Cannot proceed.")
}
print(glue::glue("Filtered 2024-25 player data: {nrow(nbaplayer_data_prep)} rows."))

# --- Prepare Game Data (for Home Status) ---
print("Preparing Game Data...")
nba_games_prep <- nba_games_raw %>%
  mutate(
    gameDate_game = lubridate::ymd_hms(gameDate), # Rename to avoid conflict if needed
    game_hometeam = recode(str_trim(paste(hometeamCity, hometeamName)), !!!rebranded_teams, .default = str_trim(paste(hometeamCity, hometeamName)))
  ) %>%
  select(gameId, game_hometeam)

# --- Join Player and Game Data ---
print("Joining Player and Game Data...")
nbaplayer_data_joined <- nbaplayer_data_prep %>%
  left_join(nba_games_prep, by = "gameId")

# --- Calculate Vs Opponent & Team Matchup Stats (from Joined 2024-25 Data ONLY) ---
print("Calculating Vs Opponent Stats (2024-25 Only)...")
# Calculate Player averages vs specific opponents IN THIS SEASON
player_vs_opp_stats_24_25 <- nbaplayer_data_joined %>%
  group_by(personId, player_name, opponent_name) %>%
  summarise(
    # avg_pts_vs_opp_24_25 = mean(points, na.rm = TRUE), # Example if needed
    # avg_ast_vs_opp_24_25 = mean(assists, na.rm = TRUE), # Example if needed
    # avg_reb_vs_opp_24_25 = mean(reboundsTotal, na.rm = TRUE), # Example if needed
    avg_pm_vs_opp_24_25 = mean(plusMinusPoints, na.rm = TRUE),
    avg_fgpct_vs_opp_24_25 = mean(ifelse(fieldGoalsAttempted > 0, fieldGoalsMade / fieldGoalsAttempted, 0), na.rm = TRUE),
    games_vs_opp_24_25 = n(),
    .groups = 'drop'
  ) %>%
  # Select only the stats intended as features
  select(personId, opponent_name, avg_fgpct_vs_opp_24_25, avg_pm_vs_opp_24_25)

# Calculate Team averages vs specific opponents IN THIS SEASON (using player data)
team_vs_opp_stats_24_25 <- nbaplayer_data_joined %>%
  group_by(team_name, opponent_name) %>%
  summarise(
    team_avg_pm_vs_opp_24_25 = mean(plusMinusPoints, na.rm = TRUE),
    .groups = 'drop'
  )

# --- Feature Engineering (Main Block - Original Explanatory Version) ---
print("Starting Feature Engineering (Original Explanatory Version)...")
nbaplayer_data_features <- nbaplayer_data_joined %>%
  # Join the pre-calculated vs-opponent stats (2024-25 only)
  left_join(player_vs_opp_stats_24_25, by = c("personId", "opponent_name")) %>%
  # Join the pre-calculated team-matchup stats (2024-25 only)
  left_join(team_vs_opp_stats_24_25, by = c("team_name", "opponent_name")) %>%
  # Arrange by player and date for rolling calculations
  arrange(personId, gameDate) %>%
  group_by(personId) %>% # Group by player for rolling/lagged stats
  mutate(
    # --- Basic Calculated Stats (Post-Game) ---
    fieldGoalsPercentage = ifelse(fieldGoalsAttempted > 0, fieldGoalsMade / fieldGoalsAttempted, 0),
    freeThrowsPercentage = ifelse(freeThrowsAttempted > 0, freeThrowsMade / freeThrowsAttempted, 0),
    is_home = as.factor(ifelse(team_name == game_hometeam, 1, 0)), # Correct home status (Factor)
    gameType = factor(gameType),
    
    # --- Concept-based Features (Mostly Post-Game) ---
    clutch_performance = ifelse(numMinutes >= 20 & abs(plusMinusPoints) >= 5, 1, 0), # Uses post-game +/- and minutes
    recent_form_pra = (points + assists + reboundsTotal) / 3, # Uses post-game stats
    
    # --- Rolling Consistency Metric (Predictive) ---
    # Calculate rolling SD of points over last 10 games (adjust window as needed)
    consistency_metric_roll10 = zoo::rollapply(points, width = 10, FUN = sd, fill = NA, align = "right", partial = TRUE),
    
    # --- Custom Scores (Calculated per game - Target and Related Score) ---
    # TARGET VARIABLE
    xfactor_score = plusMinusPoints + (0.4 * points) + (0.3 * assists) + (0.3 * reboundsTotal),
    # Related score using post-game stats
    shine_score = (0.4 * points + 0.2 * assists + 0.2 * reboundsTotal + 0.1 * steals + 0.1 * blocks - 0.1 * turnovers),
    
    # --- Historical/Rolling Averages for Player (Includes Target Leakage) ---
    # Rolling average X-Factor score over last 10 games <<< DATA LEAKAGE for prediction
    avg_xfactor_roll10 = zoo::rollapply(xfactor_score, width = 10, FUN = mean, fill = NA, align = "right", partial = TRUE)
  ) %>%
  ungroup() %>% # Ungroup after player-specific rolling calculations
  
  # Select final features for the model (includes post-game stats and target components)
  select(
    # Target Variable
    xfactor_score,
    # Predictors (Many are post-game or leaky)
    player_name, team_name, opponent_name, # Keep for inspection, assign ID role in recipe
    personId, # Keep for inspection, assign ID role in recipe
    gameType,
    numMinutes, points, assists, reboundsTotal, # <<< LEAKAGE (Components of target)
    fieldGoalsPercentage, plusMinusPoints,      # <<< LEAKAGE (Component of target & post-game)
    steals, blocks, turnovers,                   # <<< POST-GAME
    is_home, # Predictive
    clutch_performance,         # Post-game
    recent_form_pra,            # Post-game
    consistency_metric_roll10,  # Predictive (based on past points)
    shine_score,                # Post-game
    avg_xfactor_roll10,         # <<< LEAKAGE (Rolling avg of target)
    # Joined Stats (calculated based on 2024-25 data only, post-game)
    avg_fgpct_vs_opp_24_25,
    avg_pm_vs_opp_24_25,
    team_avg_pm_vs_opp_24_25
  ) %>%
  # Drop rows with NAs introduced by rolling calculations or joins
  drop_na() %>%
  distinct() # Ensure unique rows

print(glue::glue("Finished Feature Engineering. Resulting dimensions after drop_na: {nrow(nbaplayer_data_features)} rows, {ncol(nbaplayer_data_features)} columns."))

# Check if data frame is empty after processing
if(nrow(nbaplayer_data_features) == 0) {
  stop("No data remaining after cleaning, joining, and feature engineering. Check input data, filter, and join logic.")
}

# --- Data Split ---
print("Splitting data...")
set.seed(2025)
# Stratify on the outcome variable for regression splits
x_split <- initial_split(nbaplayer_data_features, prop = 0.7, strata = xfactor_score)
x_train <- training(x_split)
x_test <- testing(x_split)
print(glue::glue("Training set: {nrow(x_train)} rows. Testing set: {nrow(x_test)} rows."))


# --- Recipe for Feature Engineering ---
print("Defining Recipe...")
x_recipe <- recipe(xfactor_score ~ ., data = x_train) %>%
  # Assign ID roles
  update_role(personId, player_name, team_name, opponent_name, new_role = "ID") %>%
  # Preprocessing
  step_impute_median(all_numeric_predictors()) %>% # Impute NAs from rolling calcs/joins first
  step_dummy(all_nominal_predictors()) %>% # Handle gameType, is_home
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())
print("Recipe defined.")

# --- Model: XGBoost for Regression ---
print("Defining Model...")
x_model <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# --- Workflow ---
print("Creating Workflow...")
x_wf <- workflow() %>%
  add_recipe(x_recipe) %>%
  add_model(x_model)

# --- Fit Model ---
print("Fitting X-Factor model...")
x_fit <- fit(x_wf, data = x_train)
print("Model fitting complete.")

# --- Predict ---
print("Making predictions on test set...")
x_preds <- predict(x_fit, x_test) %>%
  bind_cols(x_test %>% select(player_name, team_name, opponent_name, xfactor_score, shine_score))

# --- Rank Top X-Factor Players (Predicted) ---
# Note: Predictions might seem accurate due to leakage, but aren't truly predictive
top_xfactors <- x_preds %>%
  arrange(desc(.pred)) %>%
  select(player_name, team_name, opponent_name, actual_xfactor = xfactor_score, predicted_xfactor = .pred, shine_score) %>%
  head(10)

print("Top 10 'Predicted' X-Factor Players (Explanatory Model):")
print(top_xfactors)

# --- Visualization ---
# plot_xfactor <- ggplot(x_preds, aes(x = .pred, y = xfactor_score)) +
#   geom_point(alpha = 0.5, color = "purple") +
#   geom_abline(color = "red", linetype = "dashed") +
#   geom_smooth(method = "lm", se = FALSE, color = "lightblue") + # Add linear trend line
#   labs(
#     title = "Predicted vs Actual X-Factor Score (2024-25 Data - Explanatory Model)",
#     x = "Predicted X-Factor Score",
#     y = "Actual X-Factor Score"
#   ) +
#   theme_minimal()
# print("Generating plot...")
# print(plot_xfactor)

# --- Model Evaluation ---
print("--- X-Factor Model Evaluation (Explanatory Performance) ---")
regression_metrics <- metric_set(rmse, rsq, mae)
eval_results_xfactor <- x_preds %>%
  regression_metrics(truth = xfactor_score, estimate = .pred)

print(eval_results_xfactor)


# --- Save the Fitted Workflow ---
print("Saving fitted workflow...")
# Assign the object you want to save
fitted_object_to_save <- x_fit

# Define the specific filename for the object being saved
output_filename <- "xfactor_model_fit.rds" # Original filename for the explanatory model

# Define the target directory
target_dir_name <- "ml_results"
target_dir_path <- file.path(target_dir_name) # Path relative to project root

# Ensure Directory Exists
if (!dir.exists(target_dir_path)) {
  print(paste("Creating directory:", target_dir_path))
  dir.create(target_dir_path, recursive = TRUE, showWarnings = FALSE)
}

# Define the full save path
save_path <- file.path(target_dir_path, output_filename)

# Save the R object
if (exists("fitted_object_to_save")) {
  print(paste("Saving object to:", save_path))
  tryCatch({
    saveRDS(fitted_object_to_save, file = save_path)
    print(paste("Successfully saved:", basename(save_path), "to", target_dir_name))
  }, error = function(e) {
    warning(paste("ERROR saving object:", e$message))
  })
} else {
  warning(paste("Object 'fitted_object_to_save' not found. File not saved to", save_path))
}

print("--- Finished Script: 07_train_xfactor_model_2024_2025.R ---")
# --- END FILE: models/07_train_xfactor_model_2024_2025.R ---