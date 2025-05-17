# --- START FILE: models/05_train_player_pts_rebs_ass_CORRECTED_RECIPE.R ---
# NBA Historical Player Points-Rebounds-Assist Predictor
# <<< REVISED to fix recipe definitions >>>

# Import Necessary Libraries
library(dplyr)
library(tidymodels)
library(tidyverse) # Includes readr, tidyr, stringr, etc.
library(lubridate)
library(rlang)     # Needed for sym()
library(yardstick) # For metrics() function
library(glue)      # For print messages

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
    # Convert gameDate once
    gameDate = lubridate::ymd_hms(gameDate),
    player_name = str_trim(paste(firstName, lastName)),
    # Apply Rebranding
    team_name = recode(str_trim(paste(playerteamCity, playerteamName)), !!!rebranded_teams, .default = str_trim(paste(playerteamCity, playerteamName))),
    opponent_name = recode(str_trim(paste(opponentteamCity, opponentteamName)), !!!rebranded_teams, .default = str_trim(paste(opponentteamCity, opponentteamName))),
    # Convert key stats to numeric safely
    across(c(fieldGoalsMade, fieldGoalsAttempted, threePointersMade, threePointersAttempted,
             freeThrowsMade, freeThrowsAttempted, points, assists, reboundsTotal),
           ~ suppressWarnings(as.numeric(.)))
  ) %>%
  # Select necessary columns
  select(gameId, personId, player_name, team_name, opponent_name, gameDate,
         fieldGoalsMade, fieldGoalsAttempted, threePointersMade, threePointersAttempted,
         freeThrowsMade, freeThrowsAttempted,
         points, assists, reboundsTotal) %>% # Only keep needed stats
  filter(!is.na(personId), !is.na(gameDate)) # Filter out rows with missing critical info early

# --- Prepare Game Data ---
print("Preparing Game Data...")
nba_games_prep <- nba_games_raw %>%
  mutate(
    # Convert gameDate once
    gameDate = lubridate::ymd_hms(gameDate),
    # Rebrand home team
    game_hometeam = recode(str_trim(paste(hometeamCity, hometeamName)), !!!rebranded_teams, .default = str_trim(paste(hometeamCity, hometeamName)))
  ) %>%
  # Select necessary columns, rename gameDate to avoid conflict
  select(gameId, game_hometeam)

# --- Join Player and Game Data ---
print("Joining Player and Game Data...")
nbaplayer_data_joined <- nbaplayer_data_prep %>%
  left_join(nba_games_prep, by = "gameId")

# --- Feature Engineering ---
print("Starting Feature Engineering...")
nbaplayer_data_features <- nbaplayer_data_joined %>%
  # Arrange for time-based calculations (days_rest, historical avg)
  arrange(personId, gameDate) %>%
  # Group by player for player-specific historical calculations
  group_by(personId) %>%
  mutate(
    # Derive home status AFTER join
    home = as.factor(ifelse(team_name == game_hometeam, 1, 0)),
    
    # Calculate percentages safely (handle 0 attempts) - used as predictors
    fieldGoalPercentage = ifelse(fieldGoalsAttempted > 0, fieldGoalsMade / fieldGoalsAttempted, 0),
    threePointPercentage = ifelse(threePointersAttempted > 0, threePointersMade / threePointersAttempted, 0),
    freeThrowPercentage = ifelse(freeThrowsAttempted > 0, freeThrowsMade / freeThrowsAttempted, 0),
    
    # Calculate days_rest correctly ONCE
    days_rest = as.numeric(difftime(gameDate, lag(gameDate), units = "days")),
    
    # Calculate player's historical avg performance (e.g., points) up to that game
    # Using lag(cummean(...)) to avoid using current game's data in the average
    player_historical_avg_pts = lag(cummean(points), default = NA) # Avg points *before* current game
  ) %>%
  # Group by player AND opponent for vs-opponent averages
  group_by(personId, opponent_name) %>%
  mutate(
    player_hist_avg_pts_vs_opp = lag(cummean(points), default = NA) # Player's rolling avg vs THIS opponent *before* current game
  ) %>%
  # Ungroup to finish group operations
  ungroup() %>%
  # Select final features for the models (including targets and predictors)
  select(
    # IDs (will get ID role later)
    personId, player_name, team_name, opponent_name, gameDate,
    
    # Target Variables
    points,
    reboundsTotal,
    assists,
    
    # Predictors (as used in recipes below)
    home,                 # Factor '1' or '0'
    fieldGoalPercentage,  # Game-specific FG%
    threePointPercentage, # Game-specific 3P%
    freeThrowPercentage,  # Game-specific FT%
    days_rest,            # Numeric
    player_historical_avg_pts, # Numeric (lagged cumulative mean)
    player_hist_avg_pts_vs_opp # Numeric (lagged cumulative mean vs opp)
  ) %>%
  # Remove rows with NA in predictors needed by *all* models or in targets
  # days_rest, player_historical_avg_pts will have NAs for first game(s)
  # player_hist_avg_pts_vs_opp will have NAs for first game(s) vs opponent
  # Also removes rows where points, reboundsTotal, or assists are NA
  drop_na(points, reboundsTotal, assists, home, days_rest,
          player_historical_avg_pts, player_hist_avg_pts_vs_opp,
          fieldGoalPercentage, threePointPercentage, freeThrowPercentage)

print(glue::glue("Finished Feature Engineering. Resulting dimensions after drop_na: {nrow(nbaplayer_data_features)} rows, {ncol(nbaplayer_data_features)} columns."))

# Check if data frame is empty after processing
if(nrow(nbaplayer_data_features) == 0) {
  stop("No data remaining after cleaning, joining, feature engineering, and drop_na(). Check input data and join logic.")
}

# --- Data Splitting (70% Train, 30% Test) ---
print("Splitting data into training and testing sets...")
set.seed(13)
data_split <- initial_split(nbaplayer_data_features, prop = 0.70)
data_train <- training(data_split)
data_test <- testing(data_split)
print(glue::glue("Training set: {nrow(data_train)} rows. Testing set: {nrow(data_test)} rows."))


# --- Create separate models for each outcome ---

# --- 1. Points prediction model ---
print("Defining Points Prediction Model...")
# Recipe for Points
points_rec <- recipe(points ~ ., data = data_train) |>  # <<< CORRECTED: Use points ~ .
  update_role(personId, player_name, team_name, opponent_name, gameDate,
              reboundsTotal, assists, # <<< ADDED: Mark other outcomes as non-predictors
              new_role = "ID") |>
  # Predictors used: home, fieldGoalPercentage, threePointPercentage, freeThrowPercentage,
  #                  days_rest, player_historical_avg_pts, player_hist_avg_pts_vs_opp
  step_impute_median(all_numeric_predictors()) |> # Impute NAs in remaining numeric predictors
  step_dummy(all_nominal_predictors()) |>         # Handle 'home' factor
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())
print("Points Recipe Defined.")

# Model definition (can be reused)
regression_model <- rand_forest(mode = "regression") |>
  set_engine("ranger", importance = "permutation") # Optional: calculate importance

# Points Workflow
points_wf <- workflow() |>
  add_recipe(points_rec) |>
  add_model(regression_model)

print("Fitting Points Workflow...")
points_fit <- fit(points_wf, data_train)
print("Points Workflow Fitted.")


# --- 2. Rebounds prediction model ---
print("Defining Rebounds Prediction Model...")
# Recipe for Rebounds
rebounds_rec <- recipe(reboundsTotal ~ ., data = data_train) |> # <<< CORRECTED: Use reboundsTotal ~ .
  update_role(personId, player_name, team_name, opponent_name, gameDate,
              points, assists, # <<< ADDED: Mark other outcomes
              # <<< ADDED: Mark predictors NOT used for rebounds as non-predictors
              fieldGoalPercentage, threePointPercentage, freeThrowPercentage,
              player_hist_avg_pts_vs_opp,
              new_role = "ID") |>
  # Predictors used: home, days_rest, player_historical_avg_pts
  step_impute_median(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())
print("Rebounds Recipe Defined.")

# Rebounds Workflow (reuse model spec)
rebounds_wf <- workflow() |>
  add_recipe(rebounds_rec) |>
  add_model(regression_model)

print("Fitting Rebounds Workflow...")
rebounds_fit <- fit(rebounds_wf, data_train)
print("Rebounds Workflow Fitted.")


# --- 3. Assists prediction model ---
print("Defining Assists Prediction Model...")
# Recipe for Assists
assists_rec <- recipe(assists ~ ., data = data_train) |> # <<< CORRECTED: Use assists ~ .
  update_role(personId, player_name, team_name, opponent_name, gameDate,
              points, reboundsTotal, # <<< ADDED: Mark other outcomes
              # <<< ADDED: Mark predictors NOT used for assists as non-predictors
              fieldGoalPercentage, threePointPercentage, freeThrowPercentage,
              new_role = "ID") |>
  # Predictors used: home, days_rest, player_historical_avg_pts, player_hist_avg_pts_vs_opp
  step_impute_median(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())
print("Assists Recipe Defined.")

# Assists Workflow (reuse model spec)
assists_wf <- workflow() |>
  add_recipe(assists_rec) |>
  add_model(regression_model)

print("Fitting Assists Workflow...")
assists_fit <- fit(assists_wf, data_train)
print("Assists Workflow Fitted.")


# --- Model Evaluation ---
# Helper function for prediction and evaluation
evaluate_model <- function(fitted_wf, test_data, outcome_var) {
  outcome_sym <- rlang::sym(outcome_var)
  print(paste("--- Evaluating:", outcome_var, "---"))
  # Ensure the test data contains all necessary columns BEFORE prediction
  # The workflow's recipe will handle selecting the right predictors internally
  predictions <- predict(fitted_wf, test_data) %>%
    bind_cols(test_data %>% select(!!outcome_sym)) # Select the specific outcome for comparison
  
  metrics_result <- metrics(predictions, truth = !!outcome_sym, estimate = .pred)
  print(metrics_result)
  return(predictions) # Return predictions if needed elsewhere
}

# Evaluate each model
print("Evaluating models on test data...")
points_preds <- evaluate_model(points_fit, data_test, "points")
rebounds_preds <- evaluate_model(rebounds_fit, data_test, "reboundsTotal")
assists_preds <- evaluate_model(assists_fit, data_test, "assists")


# --- Save the Fitted PRA Workflows (Points, Rebounds, Assists) to ml_results ---
print("Saving fitted workflows...")

# --- Assign the objects to save ---
points_object_to_save <- points_fit
rebounds_object_to_save <- rebounds_fit
assists_object_to_save <- assists_fit

# --- Define the target directory ---
target_dir_name <- "ml_results"
target_dir_path <- file.path(target_dir_name)

# --- Ensure Directory Exists (Run Once) ---
if (!dir.exists(target_dir_path)) {
  print(paste("Creating directory:", target_dir_path))
  dir.create(target_dir_path, recursive = TRUE, showWarnings = FALSE)
}

# --- Define Helper Function for Saving ---
save_object_to_target <- function(object_to_save, filename, target_dir, dir_name_for_print) {
  object_name <- deparse(substitute(object_to_save))
  save_path <- file.path(target_dir, filename)
  
  if (exists(object_name, envir = parent.frame())) {
    actual_object <- get(object_name, envir = parent.frame())
    print(paste("Saving object '", object_name, "' to: ", save_path, sep=""))
    tryCatch({
      saveRDS(actual_object, file = save_path)
      print(paste("  Successfully saved:", basename(save_path), "to", dir_name_for_print))
    }, error = function(e) {
      warning(paste("  ERROR saving object '", object_name, "': ", e$message, sep=""))
    })
  } else {
    warning(paste("Object '", object_name, "' not found. File '", filename, "' not saved.", sep=""))
  }
}

# --- Save each PRA model using the helper function ---
save_object_to_target(points_object_to_save,   "pra_points_wf.rds",   target_dir_path, target_dir_name)
save_object_to_target(rebounds_object_to_save, "pra_rebounds_wf.rds", target_dir_path, target_dir_name)
save_object_to_target(assists_object_to_save,  "pra_assists_wf.rds",  target_dir_path, target_dir_name)

print("--- Finished Script: 05_train_player_pts_rebs_ass_CORRECTED_RECIPE.R ---")
# --- END FILE: models/05_train_player_pts_rebs_ass_CORRECTED_RECIPE.R ---