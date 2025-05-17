# --- START FILE: models/01_train_game_predictor_2010_2025.R ---
# NBA 2010-2025 Data Game Predictor
# Import Necessary Libraries
library(dplyr)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(zoo)

# CORRECT path when WD is the project root (nba-dashboard)
nba_data_path <- "data/Games.csv"
nbaplayer_data_path <- "data/PlayerStatistics.csv" # Not used in this script

print(paste("Loading data from:", nba_data_path))
if (!file.exists(nba_data_path)) stop("Games.csv not found in data/ directory.")
nba_data_raw <- readr::read_csv(nba_data_path, show_col_types = FALSE)
# nbaplayer_data <- readr::read_csv(nbaplayer_data_path, show_col_types = FALSE) # Not used, can be commented out or removed if confirmed

# Rebrand NBA Teams
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

# Data Cleaning & Feature Engineering
nba_data <- nba_data_raw %>%  # <<< FIX: Start the pipe with nba_data_raw
  mutate(
    gameDate = as.Date(ymd_hms(gameDate)),
    winner = as.factor(ifelse(homeScore > awayScore, 1, 0)),
    hometeam = recode(paste(hometeamCity, hometeamName), !!!rebranded_teams),
    awayteam = recode(paste(awayteamCity, awayteamName), !!!rebranded_teams),
    season = year(gameDate) + ifelse(month(gameDate) >= 10, 1, 0),
    month = month(gameDate, label = TRUE),
    day_of_week = wday(gameDate, label = TRUE)
  ) %>%
  filter(gameDate >= as.Date("2010-10-27")) %>%
  select(gameDate, season, month, day_of_week, hometeam, awayteam, homeScore, awayScore, winner)

# Compute Average Points Scored per Game by Team
team_avg_points <- nba_data %>%
  group_by(hometeam) %>%
  summarise(home_avg_points = mean(homeScore, na.rm = TRUE), .groups = "drop")

away_avg_points <- nba_data %>%
  group_by(awayteam) %>%
  summarise(away_avg_points = mean(awayScore, na.rm = TRUE), .groups = "drop")

# Merge Average Points with nba_data
nba_data <- nba_data %>%
  left_join(team_avg_points, by = "hometeam") %>%
  left_join(away_avg_points, by = "awayteam")

# Compute Team Win Rates by Season
team_win_rates <- nba_data %>%
  group_by(hometeam, season) %>%
  summarise(home_win_rate = mean(as.numeric(winner) - 1), .groups = "drop") # Assuming winner is factor 1/0 (0=AwayWin, 1=HomeWin)
# Note: If winner is 1 for home win, 0 for away win, then mean(as.numeric(winner) - 1) is correct for home win rate.
# If winner was coded differently (e.g. 1=HomeWin, 2=AwayWin), adjust calculation.

away_win_rates <- nba_data %>%
  group_by(awayteam, season) %>%
  summarise(away_win_rate = mean(1 - (as.numeric(winner) - 1)), .groups = "drop") # Away win rate is 1 - home win rate

# Merge Win Rates
nba_data <- nba_data %>%
  left_join(team_win_rates, by = c("hometeam", "season")) %>%
  left_join(away_win_rates, by = c("awayteam", "season"))

# Compute Last 5 Games Win Rate within Season (Fixing leakage by calculating on past games)
# Ensure winner is numeric (1 for home win, 0 for away win) for rollapply
nba_data <- nba_data %>%
  mutate(winner_numeric = as.numeric(winner) - 1) %>% # Convert factor (1,0) to numeric (1,0) or adjust if needed
  group_by(hometeam, season) %>%
  arrange(gameDate) %>%
  mutate(home_last5_win_rate = zoo::rollapply(winner_numeric, 5, mean, fill = NA, align = "right", partial = TRUE)) %>%
  ungroup() %>%
  group_by(awayteam, season) %>%
  arrange(gameDate) %>%
  mutate(away_last5_win_rate = zoo::rollapply(1 - winner_numeric, 5, mean, fill = NA, align = "right", partial = TRUE)) %>%
  ungroup() %>%
  select(-winner_numeric) %>% # Remove temporary numeric winner column
  drop_na() # Drop rows where rolling average couldn't be computed (e.g., first few games)

# Data Splitting (70% Train, 30% Test)
set.seed(13)
data_split <- initial_split(nba_data, prop = 0.70, strata = winner)
data_train <- training(data_split)
data_test <- testing(data_split)

# Machine Learning Recipe (Add home_avg_points and away_avg_points as predictors)
ml_rec <- recipe(winner ~ hometeam + awayteam + season + month + day_of_week +
                   home_win_rate + away_win_rate + home_last5_win_rate +
                   away_last5_win_rate + home_avg_points + away_avg_points,
                 data = data_train) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

# Define & Train Model (Random Forest)
ml_model <- rand_forest() |>
  set_mode("classification") |>
  set_engine("ranger")

ml_wf <- workflow() |>
  add_recipe(ml_rec) |>
  add_model(ml_model) |>
  fit(data_train)

# Model Evaluation
result_final <- ml_wf |>
  predict(data_test) |>
  bind_cols(data_test)

# Accuracy
accuracy_result <- accuracy(result_final, truth = winner, estimate = .pred_class)
print(accuracy_result)

# Confusion Matrix
conf_matrix <- conf_mat(result_final, truth = winner, estimate = .pred_class)
print(conf_matrix)

# --- Save the Fitted Workflow (or other object) to ml_results ---

# Assign the object you want to save
# Example: Saving the fitted workflow
fitted_workflow_object <- ml_wf # Or clutch_fit, or any other result object

# Define the specific filename for the object being saved
output_filename <- "game_predictor_2025_wf.rds" # Or "clutch_model_fit.rds", "test_predictions.rds", etc.

# --- Define the target directory ---
# Standardize to save directly into the top-level ml_results directory
target_dir_name <- "ml_results"
target_dir_path <- file.path(target_dir_name) # Path relative to project root

# --- Ensure Directory Exists ---
# Create the directory *if it doesn't exist*
# recursive = TRUE ensures parent directories are also created if needed
if (!dir.exists(target_dir_path)) {
  print(paste("Creating directory:", target_dir_path))
  # showWarnings = FALSE prevents a warning if the directory already exists
  dir.create(target_dir_path, recursive = TRUE, showWarnings = FALSE)
}

# --- Define the full save path ---
save_path <- file.path(target_dir_path, output_filename)

# --- Save the R object ---
# Check if the object to save actually exists
if (exists("fitted_workflow_object")) { # Make sure the variable name here matches the object you assigned above
  print(paste("Saving object to:", save_path))
  tryCatch({
    saveRDS(fitted_workflow_object, file = save_path) # Use the correct variable name
    print(paste("Successfully saved:", basename(save_path), "to", target_dir_name))
  }, error = function(e) {
    warning(paste("ERROR saving object:", e$message))
  })
} else {
  warning(paste("Object 'fitted_workflow_object' not found. File not saved to", save_path)) # Update variable name in warning
}
# --- END FILE: models/01_train_game_predictor_2010_2025.R ---