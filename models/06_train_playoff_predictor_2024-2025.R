# --- START FILE: models/06_train_playoff_predictor_valid_approach_demo.R ---
# Playoff Series Predictor - DEMONSTRATION OF A VALID APPROACH USING PROXY DATA
# ---
# !!! WARNING: This script uses calculated regular season stats from Games.csv  !!!
# !!!          and SIMULATES historical series outcomes based on those stats.  !!!
# !!!          It DOES NOT use actual historical playoff series results.        !!!
# !!!          The resulting model is for DEMONSTRATION purposes only.          !!!
# ---

# Load Libraries
library(dplyr)
library(tidymodels)
library(lubridate)
library(readr)
library(stringr)
library(purrr)   # For map functions
library(vip)
library(tidyr)   # For expand_grid and pivot_longer

# --- Configuration ---
set.seed(2025) # For reproducibility
start_season_analyze <- 2000 # Analyze seasons from this year onwards
test_set_fraction <- 0.25    # Fraction of hypothetical series for testing

# --- Load & Prepare Game Data ---
games_path <- "data/Games.csv"
print(paste("Loading game data from:", games_path))
if (!file.exists(games_path)) stop("Games.csv not found in data/ directory.")
nba_games_raw <- read_csv(games_path, show_col_types = FALSE)

# --- Rebrand NBA Teams (Define once) ---
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

# --- Calculate Season-Level Team Stats from Regular Season Games ---
print("Calculating season-level team stats from historical games...")
season_team_stats <- nba_games_raw %>%
  filter(gameType == "Regular Season") %>% # Use only regular season games for stats
  mutate(
    gameDate = ymd_hms(gameDate),
    season = year(gameDate) + ifelse(month(gameDate) >= 10, 1, 0),
    home_team_rebranded = recode(str_trim(paste(hometeamCity, hometeamName)), !!!rebranded_teams, .default = str_trim(paste(hometeamCity, hometeamName))),
    away_team_rebranded = recode(str_trim(paste(awayteamCity, awayteamName)), !!!rebranded_teams, .default = str_trim(paste(awayteamCity, awayteamName))),
    home_won = ifelse(homeScore > awayScore, 1, 0),
    away_won = ifelse(awayScore > homeScore, 1, 0)
  ) %>%
  filter(season >= start_season_analyze) %>%
  # Reshape data: one row per team per game
  pivot_longer(
    cols = c("home_team_rebranded", "away_team_rebranded"),
    names_to = "location",
    values_to = "team_name"
  ) %>%
  mutate(
    won = ifelse(location == "home_team_rebranded", home_won, away_won),
    points_for = ifelse(location == "home_team_rebranded", homeScore, awayScore),
    points_against = ifelse(location == "home_team_rebranded", awayScore, homeScore)
  ) %>%
  # Ensure scores are numeric before aggregation
  mutate(across(c(homeScore, awayScore, points_for, points_against), as.numeric)) %>%
  group_by(season, team_name) %>%
  summarise(
    games_played = n(),
    wins = sum(won, na.rm = TRUE),
    losses = games_played - wins,
    win_rate = wins / games_played,
    avg_pts_for = mean(points_for, na.rm = TRUE),
    avg_pts_against = mean(points_against, na.rm = TRUE),
    avg_pt_diff = avg_pts_for - avg_pts_against,
    .groups = "drop"
  ) %>%
  # Filter out seasons where teams played very few games (e.g., due to partial data or lockouts)
  filter(games_played > 40) # Adjust threshold as needed

print(paste("Calculated stats for", n_distinct(season_team_stats$team_name), "teams across relevant seasons."))

# --- Create Hypothetical Series Matchup Data ---
print("Creating hypothetical series matchup data...")
# Get unique teams per season
teams_per_season <- season_team_stats %>% distinct(season, team_name)

# Generate all unique pairs of teams within each season
hypothetical_series_data <- teams_per_season %>%
  group_by(season) %>%
  # Create combinations of 2 teams for each season
  summarise(tidyr::expand_grid(team1 = team_name, team2 = team_name), .groups = "drop") %>%
  filter(team1 < team2) %>% # Ensure unique pairs (avoid TeamA vs TeamB and TeamB vs TeamA)
  # Join season stats for Team 1
  left_join(season_team_stats, by = c("season", "team1" = "team_name")) %>%
  rename_with(~ paste0(.x, "_t1"), .cols = c(games_played, wins, losses, win_rate, avg_pts_for, avg_pts_against, avg_pt_diff)) %>%
  # Join season stats for Team 2
  left_join(season_team_stats, by = c("season", "team2" = "team_name")) %>%
  rename_with(~ paste0(.x, "_t2"), .cols = c(games_played, wins, losses, win_rate, avg_pts_for, avg_pts_against, avg_pt_diff)) %>%
  # Remove rows where stats might be missing for a team in a season
  drop_na()

print(paste("Generated", nrow(hypothetical_series_data), "hypothetical series matchups."))

# --- Feature Engineering: Calculate Differentials & Simulate Outcome ---
print("Calculating differential features and simulating series outcome...")
series_features <- hypothetical_series_data %>%
  mutate(
    # Calculate differential features
    win_rate_diff = win_rate_t1 - win_rate_t2,
    avg_pt_diff_diff = avg_pt_diff_t1 - avg_pt_diff_t2,
    avg_pts_for_diff = avg_pts_for_t1 - avg_pts_for_t2,
    avg_pts_against_diff = avg_pts_against_t1 - avg_pts_against_t2,
    
    # --- SIMULATED TARGET VARIABLE ---
    # Assume team with better regular season win rate wins the hypothetical series
    # Make it a factor for classification
    team1_wins_series = as.factor(ifelse(win_rate_t1 > win_rate_t2, 1, 0))
    # Add home court simulation (higher win rate = home court) - used as predictor
    # home_court_adv_t1 = ifelse(win_rate_t1 > win_rate_t2, 1, 0) # Binary home court
  ) %>%
  # Select only the features for the model and the target
  select(
    # Predictors (differentials)
    season, # Keep season potentially as a predictor or for splitting
    win_rate_diff,
    avg_pt_diff_diff,
    avg_pts_for_diff,
    avg_pts_against_diff,
    # home_court_adv_t1,
    # Target
    team1_wins_series,
    # IDs (for reference, not used in model)
    team1, team2
  )

# --- Data Splitting (Chronological or Random) ---
# Option 1: Random Split (Stratified)
# series_split <- initial_split(series_features, prop = 1 - test_set_fraction, strata = team1_wins_series)
# series_train <- training(series_split)
# series_test <- testing(series_split)

# Option 2: Chronological Split (Recommended for time-sensitive data)
print("Splitting data chronologically...")
unique_seasons <- sort(unique(series_features$season))
n_seasons <- length(unique_seasons)
n_test_seasons <- ceiling(n_seasons * test_set_fraction)
last_train_season <- unique_seasons[n_seasons - n_test_seasons]

series_train <- series_features %>% filter(season <= last_train_season)
series_test <- series_features %>% filter(season > last_train_season)
print(paste("Training on seasons up to", last_train_season, "(", nrow(series_train), "series )"))
print(paste("Testing on seasons after", last_train_season, "(", nrow(series_test), "series )"))

# Check if train/test sets are empty
if(nrow(series_train) == 0 || nrow(series_test) == 0) {
  stop("Training or testing set is empty after chronological split. Adjust start_season_analyze or test_set_fraction.")
}


# --- Recipe ---
# Using only differential features (excluding season for simplicity here)
series_recipe <- recipe(team1_wins_series ~ win_rate_diff + avg_pt_diff_diff + avg_pts_for_diff + avg_pts_against_diff,
                        data = series_train) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>% # Handle potential NAs if any slipped through
  step_normalize(all_numeric_predictors())

# --- Model Definition ---
series_model <- boost_tree(trees = 500, tree_depth = 4, learn_rate = 0.01) %>% # Example params
  set_engine("xgboost") %>%
  set_mode("classification")

# --- Workflow ---
series_wf <- workflow() %>%
  add_recipe(series_recipe) %>%
  add_model(series_model)

# --- Fit the Model (Training on *simulated* outcomes) ---
print("Fitting model (WARNING: Training on simulated outcomes based on win rate)...")
series_fit <- fit(series_wf, data = series_train)
print("Model fitting complete.")

# --- Predict ---
series_preds <- predict(series_fit, series_test) %>%
  bind_cols(predict(series_fit, series_test, type = "prob")) %>%
  bind_cols(series_test %>% select(team1_wins_series, team1, team2, season)) # Include IDs/season

# --- Evaluate (Metrics reflect ability to predict the *simulated* outcome) ---
print(paste("--- Model Evaluation (Predicting Simulated Outcome for Seasons >", last_train_season, ") ---"))

class_metrics <- metric_set(accuracy, roc_auc, mn_log_loss)
# Ensure levels are correctly ordered for probability columns (e.g., .pred_0 corresponds to level "0")
eval_results <- series_preds %>%
  class_metrics(truth = team1_wins_series, estimate = .pred_class, .pred_0)

print(eval_results)

print("--- Confusion Matrix (Predicting Simulated Outcome) ---")
conf_mat_result <- series_preds %>%
  conf_mat(truth = team1_wins_series, estimate = .pred_class)
print(conf_mat_result)

# --- Feature Importance (Shows importance for predicting the *simulated* target) ---
print("--- Feature Importance (for predicting simulated outcome) ---")
tryCatch({
  vip_plot <- vip(extract_fit_engine(series_fit), num_features = 10)
  print(vip_plot)
}, error = function(e){
  warning("Could not generate variable importance plot. Error: ", e$message)
})


# --- Save the Fitted Workflow ---
# Assign the object you want to save
fitted_object_to_save <- series_fit

# Define the specific filename
# Append "_demo" to signify it's based on the demonstration approach
output_filename <- "playoff_predictor_fit_demo.rds"

# --- Define the target directory ---
target_dir_name <- "ml_results"
target_dir_path <- file.path(target_dir_name) # Path relative to project root

# --- Ensure Directory Exists ---
if (!dir.exists(target_dir_path)) {
  print(paste("Creating directory:", target_dir_path))
  dir.create(target_dir_path, recursive = TRUE, showWarnings = FALSE)
}

# --- Define the full save path ---
save_path <- file.path(target_dir_path, output_filename)

# --- Save the R object ---
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
# --- END FILE: models/06_train_playoff_predictor_valid_approach_demo.R ---