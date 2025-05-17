# --- START FILE: models/08_train_game_predictor_current_season.R ---
# NBA Current Season (2024-2025) Game Predictor

# Import Necessary Libraries
library(dplyr)
library(tidymodels)
library(tidyverse) # Includes readr, lubridate, etc.
library(lubridate)
library(zoo)       # For rollapply
library(glue)      # For print messages
library(vip)       # For variable importance plots
library(yardstick) # For additional metrics
library(scales)    # For scales::percent() and scales::number()

# --- Configuration ---
# Define the target season for this model
TARGET_MODEL_SEASON_END_YEAR <- 2025
# --- IMPORTANT: PLEASE ADJUST THESE DATES TO THE ACTUAL 2024-2025 NBA SEASON SCHEDULE ---
SEASON_START_DATE <- as.Date("2024-10-01") # Approximate start
SEASON_END_DATE   <- as.Date("2025-06-30") # Approximate end (can include playoffs for more data)
# --- END IMPORTANT ---

print(glue::glue("--- Training Game Predictor for Current Season: Ending {TARGET_MODEL_SEASON_END_YEAR} ---"))
print(glue::glue("--- Using data from {SEASON_START_DATE} to {SEASON_END_DATE} ---"))

# --- Load Data ---
nba_data_path <- "data/Games.csv"
print(paste("Loading game data from:", nba_data_path))
if (!file.exists(nba_data_path)) {
  stop(glue::glue("Games.csv not found in data/ directory. Path checked: {getwd()}/{nba_data_path}"))
}
nba_data_raw <- readr::read_csv(nba_data_path, show_col_types = FALSE)

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

# --- Data Cleaning & Feature Engineering (for 2024-2025 Season) ---
print("Cleaning data and engineering features for the 2024-2025 season...")
nba_data_current_season <- nba_data_raw %>%
  mutate(
    gameDate = as.Date(ymd_hms(gameDate)),
    winner = as.factor(ifelse(homeScore > awayScore, 1, 0)), # Ensure winner is factor with levels 0 and 1
    hometeam = recode(str_trim(paste(hometeamCity, hometeamName)), !!!rebranded_teams, .default = str_trim(paste(hometeamCity, hometeamName))),
    awayteam = recode(str_trim(paste(awayteamCity, awayteamName)), !!!rebranded_teams, .default = str_trim(paste(awayteamCity, awayteamName))),
    season = year(gameDate + months(2)), # Heuristic for season end year
    month = month(gameDate, label = TRUE),
    day_of_week = wday(gameDate, label = TRUE)
  ) %>%
  filter(gameDate >= SEASON_START_DATE & gameDate <= SEASON_END_DATE) %>%
  select(gameDate, season, month, day_of_week, hometeam, awayteam, homeScore, awayScore, winner)

if(nrow(nba_data_current_season) == 0) {
  stop(glue::glue("No game data found for the 2024-2025 season (between {SEASON_START_DATE} and {SEASON_END_DATE}). Cannot proceed."))
}
print(glue::glue("Found {nrow(nba_data_current_season)} games for the 2024-2025 season."))

# 1. Average Points Scored (within the current season)
team_avg_points_current <- nba_data_current_season %>%
  group_by(hometeam) %>%
  summarise(home_avg_points_current = mean(homeScore, na.rm = TRUE), .groups = "drop")
away_avg_points_current <- nba_data_current_season %>%
  group_by(awayteam) %>%
  summarise(away_avg_points_current = mean(awayScore, na.rm = TRUE), .groups = "drop")

nba_data_processed <- nba_data_current_season %>%
  left_join(team_avg_points_current, by = "hometeam") %>%
  left_join(away_avg_points_current, by = "awayteam")

# 2. Team Win Rates (within the current season)
team_win_rates_current <- nba_data_processed %>%
  group_by(hometeam, season) %>%
  summarise(home_win_rate_current = mean(as.numeric(as.character(winner)), na.rm = TRUE), .groups = "drop") # winner is 0 or 1
away_win_rates_current <- nba_data_processed %>%
  group_by(awayteam, season) %>%
  summarise(away_win_rate_current = mean(1 - as.numeric(as.character(winner)), na.rm = TRUE), .groups = "drop")

nba_data_processed <- nba_data_processed %>%
  left_join(team_win_rates_current, by = c("hometeam", "season")) %>%
  left_join(away_win_rates_current, by = c("awayteam", "season"))

# 3. Last 5 Games Win Rate (within the current season)
nba_data_processed <- nba_data_processed %>%
  mutate(winner_numeric = as.numeric(as.character(winner))) %>% # Ensure numeric for rollapply
  group_by(hometeam, season) %>%
  arrange(gameDate) %>%
  mutate(home_last5_win_rate_current = zoo::rollapply(winner_numeric, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE)) %>%
  ungroup() %>%
  group_by(awayteam, season) %>%
  arrange(gameDate) %>%
  mutate(away_last5_win_rate_current = zoo::rollapply(1 - winner_numeric, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE)) %>%
  ungroup() %>%
  select(-winner_numeric) %>%
  mutate(
    home_avg_points_current = tidyr::replace_na(home_avg_points_current, mean(home_avg_points_current, na.rm = TRUE) %||% 0),
    away_avg_points_current = tidyr::replace_na(away_avg_points_current, mean(away_avg_points_current, na.rm = TRUE) %||% 0),
    home_win_rate_current = tidyr::replace_na(home_win_rate_current, 0.5),
    away_win_rate_current = tidyr::replace_na(away_win_rate_current, 0.5),
    home_last5_win_rate_current = ifelse(is.na(home_last5_win_rate_current), home_win_rate_current, home_last5_win_rate_current),
    away_last5_win_rate_current = ifelse(is.na(away_last5_win_rate_current), away_win_rate_current, away_last5_win_rate_current)
  ) %>%
  drop_na() # Drop any remaining rows if critical predictors are still NA

if(nrow(nba_data_processed) < 20) { # Arbitrary small number, adjust as needed
  warning(glue::glue("Very few rows ({nrow(nba_data_processed)}) remaining after feature engineering for current season. Model training might be unreliable or fail."))
  if(nrow(nba_data_processed) == 0) stop("No data left after feature engineering. Stopping.")
}
print(glue::glue("Feature engineering complete. Processed data has {nrow(nba_data_processed)} rows."))


# --- Data Splitting ---
set.seed(13)
data_split_current <- initial_split(nba_data_processed, prop = 0.70, strata = winner)
data_train_current <- training(data_split_current)
data_test_current  <- testing(data_split_current)

print(glue::glue("Data split: {nrow(data_train_current)} training samples, {nrow(data_test_current)} testing samples."))
if(nrow(data_train_current) == 0 || nrow(data_test_current) == 0) {
  stop("Training or testing data is empty after split. Check data processing and split proportion.")
}

# --- Machine Learning Recipe ---
ml_rec_current <- recipe(winner ~ hometeam + awayteam + month + day_of_week +
                           home_win_rate_current + away_win_rate_current +
                           home_last5_win_rate_current + away_last5_win_rate_current +
                           home_avg_points_current + away_avg_points_current,
                         data = data_train_current) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# --- Define & Train Model ---
ml_model_current <- rand_forest(trees = 500) |> # Consider fewer trees for smaller data if needed
  set_mode("classification") |>
  set_engine("ranger", importance = "permutation")

ml_wf_current <- workflow() |>
  add_recipe(ml_rec_current) |>
  add_model(ml_model_current)

print("Fitting the current season game predictor model...")
fitted_wf_current <- tryCatch({
  fit(ml_wf_current, data = data_train_current)
}, error = function(e) {
  warning(glue::glue("Error during model fitting: {e$message}. This might be due to insufficient data or issues in the recipe/data after filtering for current season."))
  NULL
})

if (is.null(fitted_wf_current)) {
  stop("Model fitting failed. Review warnings and data.")
}
print("Model fitting complete.")

# --- Enhanced Model Evaluation Section ---
print("--- Evaluating Current Season Model Performance ---")

# Predict probabilities and class on the test set
predictions_test_current <- predict(fitted_wf_current, data_test_current, type = "prob") %>%
  bind_cols(predict(fitted_wf_current, data_test_current, type = "class")) %>%
  bind_cols(data_test_current %>% select(winner)) # Add true outcome

# Define metrics set
classification_metrics <- metric_set(accuracy, roc_auc, sens, yardstick::spec, precision, f_meas, mn_log_loss)

# Calculate metrics
eval_results_current <- predictions_test_current %>%
  classification_metrics(truth = winner, estimate = .pred_class, event_level = "second", .pred_1)

# --- START: Modified Print Output for Metrics ---
print("Overall Classification Metrics:")
eval_results_current_formatted <- eval_results_current %>%
  mutate(
    .estimate_formatted = case_when(
      .metric %in% c("accuracy", "roc_auc", "sens", "spec", "precision", "f_meas") ~ scales::percent(.estimate, accuracy = 0.1),
      .metric == "mn_log_loss" ~ scales::number(.estimate, accuracy = 0.001),
      TRUE ~ as.character(.estimate) # Fallback for any other metrics
    )
  ) %>%
  select(.metric, .estimator, Formatted_Estimate = .estimate_formatted)

# Print the formatted table
print(as.data.frame(eval_results_current_formatted)) # as.data.frame for cleaner console printing
# --- END: Modified Print Output for Metrics ---

# Confusion Matrix
print("Confusion Matrix:")
conf_matrix_plot_current <- predictions_test_current %>%
  conf_mat(truth = winner, estimate = .pred_class) %>%
  autoplot(type = "heatmap")
print(conf_matrix_plot_current)

# ROC Curve
roc_curve_plot_current <- predictions_test_current %>%
  roc_curve(truth = winner, .pred_1, event_level = "second") %>%
  autoplot()
print("ROC Curve:")
print(roc_curve_plot_current)

# Variable Importance Plot
print("Variable Importance Plot:")
tryCatch({
  importance_plot_current <- vip(extract_fit_engine(fitted_wf_current), num_features = 15, geom = "col") +
    labs(title = "Feature Importance - Current Season Game Predictor") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(importance_plot_current)
}, error = function(e) {
  warning(paste("Could not generate variable importance plot:", e$message))
})
# --- End Enhanced Model Evaluation Section ---


# --- Save the Fitted Workflow ---
fitted_workflow_object_current <- fitted_wf_current
output_filename_current <- "game_predictor_current_season_wf.rds"
target_dir_name <- "ml_results"
target_dir_path <- file.path(target_dir_name)
if (!dir.exists(target_dir_path)) {
  print(paste("Creating directory:", target_dir_path))
  dir.create(target_dir_path, recursive = TRUE, showWarnings = FALSE)
}
save_path_current <- file.path(target_dir_path, output_filename_current)
if (exists("fitted_workflow_object_current") && !is.null(fitted_workflow_object_current)) {
  print(paste("Saving current season model object to:", save_path_current))
  tryCatch({
    saveRDS(fitted_workflow_object_current, file = save_path_current)
    print(paste("Successfully saved:", basename(save_path_current), "to", target_dir_name))
  }, error = function(e) {
    warning(paste("ERROR saving current season model object:", e$message))
  })
} else {
  warning(paste("Object 'fitted_workflow_object_current' not found or is NULL. File not saved to", save_path_current))
}

print(glue::glue("--- Finished Training Game Predictor for Current Season: Ending {TARGET_MODEL_SEASON_END_YEAR} ---"))
# --- END FILE: models/08_train_game_predictor_current_season.R ---