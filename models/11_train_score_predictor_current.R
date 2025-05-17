# --- START FILE: models/11_train_score_predictor_current.R (Tuning, Rolling Features, Faster Setup) ---
# NBA Current Season Score Predictor (Home Score & Away Score)

# Import Necessary Libraries
library(dplyr)
library(tidymodels)
library(tidyverse) # Includes readr, lubridate, etc.
library(lubridate)
library(zoo)       # For rollapply
library(glue)      # For print messages
library(vip)       # For variable importance plots
library(yardstick) # For regression metrics
library(scales)    # For formatting output if needed
# library(doParallel) # For parallel processing during tuning

# --- Configuration ---
# These dates MUST align with how you define "Current Season" in your app and other scripts
# For 2024-2025 season
CURRENT_SEASON_START_DATE <- as.Date("2024-10-01") # PLEASE ADJUST TO ACTUAL START
CURRENT_SEASON_END_DATE   <- as.Date("2025-07-01") # PLEASE ADJUST TO ACTUAL END (e.g., after Finals)

N_CV_FOLDS_CURRENT <- 3 # Reduced folds for faster tuning on smaller dataset
ROLLING_AVG_SCORE_WINDOW_CURRENT <- 5 
RF_TREES_CURRENT <- 100 # Reduced trees for faster fitting

print(glue::glue("--- Training IMPROVED Score Predictors for Current Season ({year(CURRENT_SEASON_START_DATE)}-{year(CURRENT_SEASON_END_DATE)}) ---"))
print(glue::glue("--- Using {N_CV_FOLDS_CURRENT}-fold CV, {RF_TREES_CURRENT} trees, and rolling window of {ROLLING_AVG_SCORE_WINDOW_CURRENT} ---"))

# --- Optional: Setup parallel processing ---
# num_cores <- parallel::detectCores(logical = FALSE)
# if (num_cores > 1) {
#   cl <- makePSOCKcluster(num_cores - 1)
#   registerDoParallel(cl)
#   print(glue::glue("Registered parallel backend with {getDoParWorkers()} cores."))
# } else {
#   print("Single core detected, parallel processing not enabled.")
# }
# --- End Optional Parallel Processing ---

# --- Load Data ---
nba_data_path <- "data/Games.csv"
print(paste("Loading game data from:", nba_data_path))
if (!file.exists(nba_data_path)) stop(glue::glue("Games.csv not found. Path: {getwd()}/{nba_data_path}"))
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

# --- Data Cleaning & Initial Feature Engineering for Current Season ---
print("Cleaning data and initial feature engineering for Current Season scores...")
nba_data_current_s <- nba_data_raw %>%
  mutate(
    gameDate = as.Date(ymd_hms(gameDate)),
    homeScore_target = as.numeric(homeScore),
    awayScore_target = as.numeric(awayScore),
    hometeam = recode(str_trim(paste(hometeamCity, hometeamName)), !!!rebranded_teams, .default = str_trim(paste(hometeamCity, hometeamName))),
    awayteam = recode(str_trim(paste(awayteamCity, awayteamName)), !!!rebranded_teams, .default = str_trim(paste(awayteamCity, awayteamName))),
    season = year(gameDate + months(2)), 
    month = month(gameDate, label = TRUE),
    day_of_week = wday(gameDate, label = TRUE),
    winner_temp = ifelse(homeScore > awayScore, 1, 0)
  ) %>%
  filter(gameDate >= CURRENT_SEASON_START_DATE & gameDate <= CURRENT_SEASON_END_DATE, # CRITICAL FILTER
         !is.na(homeScore_target), !is.na(awayScore_target), !is.na(gameDate)) %>%
  select(gameId, gameDate, season, month, day_of_week, hometeam, awayteam, 
         homeScore_target, awayScore_target, winner_temp)

if(nrow(nba_data_current_s) == 0) stop(glue::glue("No game data found for the Current Season ({CURRENT_SEASON_START_DATE} to {CURRENT_SEASON_END_DATE}). Cannot proceed."))
print(glue::glue("Found {nrow(nba_data_current_s)} games for Current Season initial processing."))

# --- Advanced Feature Engineering (specific to current season data) ---
print("Performing advanced feature engineering for Current Season data...")
# 1. Offensive/Defensive Strengths (Averages within this Current Season ONLY)
home_team_off_str_cs <- nba_data_current_s %>% group_by(hometeam) %>% summarise(h_offensive_strength_period = mean(homeScore_target, na.rm = TRUE), .groups = "drop")
away_team_off_str_cs <- nba_data_current_s %>% group_by(awayteam) %>% summarise(a_offensive_strength_period = mean(awayScore_target, na.rm = TRUE), .groups = "drop")
home_team_def_str_cs <- nba_data_current_s %>% group_by(hometeam) %>% summarise(h_defensive_strength_period = mean(awayScore_target, na.rm = TRUE), .groups = "drop")
away_team_def_str_cs <- nba_data_current_s %>% group_by(awayteam) %>% summarise(a_defensive_strength_period = mean(homeScore_target, na.rm = TRUE), .groups = "drop")

nba_data_features_cs <- nba_data_current_s %>%
  left_join(home_team_off_str_cs, by = "hometeam") %>%
  left_join(away_team_off_str_cs, by = "awayteam") %>%
  left_join(home_team_def_str_cs, by = "hometeam") %>%
  left_join(away_team_def_str_cs, by = "awayteam")

# 2. Season Win Rates (will be win rates for this Current Season ONLY)
home_team_win_rates_cs <- nba_data_features_cs %>% group_by(hometeam, season) %>% summarise(h_win_rate_season = mean(winner_temp, na.rm = TRUE), .groups = "drop")
away_team_win_rates_cs <- nba_data_features_cs %>% group_by(awayteam, season) %>% summarise(a_win_rate_season = mean(1 - winner_temp, na.rm = TRUE), .groups = "drop")
nba_data_features_cs <- nba_data_features_cs %>%
  left_join(home_team_win_rates_cs, by = c("hometeam", "season")) %>%
  left_join(away_team_win_rates_cs, by = c("awayteam", "season"))

# 3. Rolling Last N Game Win Rates (within this Current Season)
nba_data_features_cs <- nba_data_features_cs %>%
  group_by(hometeam, season) %>% arrange(gameDate) %>%
  mutate(h_lastN_win_rate_season = zoo::rollapply(winner_temp, width = ROLLING_AVG_SCORE_WINDOW_CURRENT, FUN = mean, fill = NA, align = "right", partial = TRUE)) %>%
  ungroup() %>%
  group_by(awayteam, season) %>% arrange(gameDate) %>%
  mutate(a_lastN_win_rate_season = zoo::rollapply(1 - winner_temp, width = ROLLING_AVG_SCORE_WINDOW_CURRENT, FUN = mean, fill = NA, align = "right", partial = TRUE)) %>%
  ungroup()

# 4. Rolling Last N Game Average Scores (within this Current Season)
nba_data_features_cs <- nba_data_features_cs %>%
  group_by(hometeam, season) %>% arrange(gameDate) %>%
  mutate(
    h_avg_score_lastN = zoo::rollapply(homeScore_target, width = ROLLING_AVG_SCORE_WINDOW_CURRENT, FUN = mean, fill = NA, align = "right", partial = TRUE),
    h_avg_pts_allowed_lastN = zoo::rollapply(awayScore_target, width = ROLLING_AVG_SCORE_WINDOW_CURRENT, FUN = mean, fill = NA, align = "right", partial = TRUE)
  ) %>% ungroup() %>%
  group_by(awayteam, season) %>% arrange(gameDate) %>%
  mutate(
    a_avg_score_lastN = zoo::rollapply(awayScore_target, width = ROLLING_AVG_SCORE_WINDOW_CURRENT, FUN = mean, fill = NA, align = "right", partial = TRUE),
    a_avg_pts_allowed_lastN = zoo::rollapply(homeScore_target, width = ROLLING_AVG_SCORE_WINDOW_CURRENT, FUN = mean, fill = NA, align = "right", partial = TRUE)
  ) %>% ungroup()

# Clean up and impute
nba_data_features_cs <- nba_data_features_cs %>%
  select(-winner_temp) %>% 
  mutate( 
    h_offensive_strength_period = tidyr::replace_na(h_offensive_strength_period, mean(h_offensive_strength_period, na.rm=TRUE) %||% 105), # Default based on modern averages
    a_offensive_strength_period = tidyr::replace_na(a_offensive_strength_period, mean(a_offensive_strength_period, na.rm=TRUE) %||% 105),
    h_defensive_strength_period = tidyr::replace_na(h_defensive_strength_period, mean(h_defensive_strength_period, na.rm=TRUE) %||% 105),
    a_defensive_strength_period = tidyr::replace_na(a_defensive_strength_period, mean(a_defensive_strength_period, na.rm=TRUE) %||% 105),
    h_win_rate_season = tidyr::replace_na(h_win_rate_season, 0.5),
    a_win_rate_season = tidyr::replace_na(a_win_rate_season, 0.5),
    h_lastN_win_rate_season = ifelse(is.na(h_lastN_win_rate_season), h_win_rate_season, h_lastN_win_rate_season),
    a_lastN_win_rate_season = ifelse(is.na(a_lastN_win_rate_season), a_win_rate_season, a_lastN_win_rate_season),
    h_avg_score_lastN = ifelse(is.na(h_avg_score_lastN), h_offensive_strength_period, h_avg_score_lastN),
    a_avg_score_lastN = ifelse(is.na(a_avg_score_lastN), a_offensive_strength_period, a_avg_score_lastN),
    h_avg_pts_allowed_lastN = ifelse(is.na(h_avg_pts_allowed_lastN), h_defensive_strength_period, h_avg_pts_allowed_lastN),
    a_avg_pts_allowed_lastN = ifelse(is.na(a_avg_pts_allowed_lastN), a_defensive_strength_period, a_avg_pts_allowed_lastN)
  ) %>%
  drop_na() 

if(nrow(nba_data_features_cs) < 30) { # Increased threshold slightly for current season
  warning(glue::glue("Very few rows ({nrow(nba_data_features_cs)}) remaining for Current Season. Score model training might be highly unreliable or fail."))
  if(nrow(nba_data_features_cs) == 0) stop("No data left for Current Season. Stopping.")
}
print(glue::glue("Feature engineering complete. Processed Current Season data has {nrow(nba_data_features_cs)} rows."))

# --- Data Splitting ---
set.seed(1111) # Different seed
data_split_score_cs <- initial_split(nba_data_features_cs, prop = 0.70, strata = homeScore_target) # Reduced prop if data is very scarce
train_data_score_cs <- training(data_split_score_cs)
test_data_score_cs  <- testing(data_split_score_cs)
# For very small datasets, repeated CV might be better, or just a validation split.
# Using fewer folds for speed.
folds_cs <- vfold_cv(train_data_score_cs, v = N_CV_FOLDS_CURRENT, strata = homeScore_target) 

print(glue::glue("Data split: {nrow(train_data_score_cs)} training, {nrow(test_data_score_cs)} testing. Using {N_CV_FOLDS_CURRENT} CV folds for Current Season models."))
if(nrow(train_data_score_cs) < 20 || nrow(test_data_score_cs) < 10) {
  warning("Training or test set for Current Season is very small. Model results may not be stable.")
}


# --- Define TUNABLE Regression Model Specification (Faster Setup) ---
score_model_spec_rf_tune_cs <- rand_forest(
  trees = RF_TREES_CURRENT, # Reduced trees
  mtry = tune(),
  min_n = tune()
) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")

# --- Re-use Helper Function to Train, Tune, and Evaluate a Score Model ---
# (Ensure this function is defined as in 09_... or sourced)
train_tune_evaluate_single_score_model <- function(target_var_name, train_data, test_data, cv_folds, model_spec_tuneable, context_name) {
  target_sym <- rlang::sym(target_var_name)
  print(glue::glue("--- Processing model for {target_var_name} ({context_name}) ---"))
  
  other_score_target_col <- if (target_var_name == "homeScore_target") "awayScore_target" else "homeScore_target"
  
  if (target_var_name == "homeScore_target") {
    current_predictors <- c("hometeam", "awayteam", "season", "month", "day_of_week",
                            "h_offensive_strength_period", "a_defensive_strength_period", 
                            "h_win_rate_season", "a_win_rate_season", 
                            "h_lastN_win_rate_season", "a_lastN_win_rate_season",
                            "h_avg_score_lastN", "a_avg_pts_allowed_lastN")
  } else if (target_var_name == "awayScore_target") {
    current_predictors <- c("hometeam", "awayteam", "season", "month", "day_of_week",
                            "a_offensive_strength_period", "h_defensive_strength_period", 
                            "h_win_rate_season", "a_win_rate_season", 
                            "h_lastN_win_rate_season", "a_lastN_win_rate_season",
                            "a_avg_score_lastN", "h_avg_pts_allowed_lastN")
  } else {
    stop("Invalid target_var_name provided.")
  }
  
  score_recipe <- recipe(train_data) |> 
    update_role(!!target_sym, new_role = "outcome") |>
    update_role(all_of(current_predictors), new_role = "predictor") |>
    update_role(!!rlang::sym(other_score_target_col), gameId, gameDate, new_role = "ID") |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_corr(all_numeric_predictors(), threshold = 0.95) |> # Slightly looser corr for smaller data
    step_normalize(all_numeric_predictors())
  
  score_wf <- workflow(preprocessor = score_recipe, spec = model_spec_tuneable)
  
  # Minimal grid for very fast tuning, especially for small current season data
  rf_grid_cs <- grid_regular( 
    mtry(range = c(max(1L, floor(length(current_predictors)/5)), # Ensure mtry is at least 1
                   min(length(current_predictors), 15L))),      # Cap mtry
    min_n(range = c(5L, 25L)), # min_n might need to be higher for small data
    levels = 2 # Minimal levels (2x2 = 4 combinations)
  )
  
  print(glue::glue("Starting hyperparameter tuning for {target_var_name} ({N_CV_FOLDS_CURRENT}-fold CV, {nrow(rf_grid_cs)} candidates)..."))
  print(glue::glue("Base predictor count for mtry range reference: {length(current_predictors)}"))
  print("Tuning grid (Current Season):")
  print(rf_grid_cs)
  
  # Check if train_data is too small for the number of folds
  if(nrow(train_data) < N_CV_FOLDS_CURRENT * 2) { # Arbitrary check, ensure enough samples per fold
    warning(glue::glue("Training data for {target_var_name} ({nrow(train_data)} rows) is very small for {N_CV_FOLDS_CURRENT} folds. Reducing folds to 2 or skipping tuning."))
    # Option: Skip tuning and fit with default/pre-set hyperparameters
    # For now, let tune_grid attempt it, it might handle small N gracefully or error.
    # If errors occur, one strategy is to reduce N_CV_FOLDS_CURRENT or use validation_split()
  }
  
  tune_res <- tune_grid(
    score_wf,
    resamples = cv_folds, # cv_folds should be passed in, specific to this dataset
    grid = rf_grid_cs,
    metrics = metric_set(rmse, rsq),
    control = control_grid(save_pred = FALSE, verbose = TRUE, allow_par = TRUE)
  )
  
  print(glue::glue("Tuning complete for {target_var_name}."))
  print(glue::glue("Best {min(5, nrow(rf_grid_cs))} hyperparameter sets based on RMSE for {target_var_name}:"))
  show_best(tune_res, metric = "rmse", n = min(5, nrow(rf_grid_cs)))
  best_hp <- select_best(tune_res, metric = "rmse")
  print(glue::glue("Best hyperparameters for {target_var_name} based on RMSE:"))
  print(best_hp)
  
  final_wf <- finalize_workflow(score_wf, best_hp)
  
  print(glue::glue("Fitting final model for {target_var_name} with best hyperparameters..."))
  final_fitted_wf <- fit(final_wf, data = train_data) 
  print(glue::glue("Final model fitting complete for {target_var_name}."))
  
  print(glue::glue("Evaluating final model for {target_var_name} on test data..."))
  if (nrow(test_data) > 0) {
    predictions <- predict(final_fitted_wf, new_data = test_data) %>%
      bind_cols(test_data %>% select(!!target_sym))
    
    regression_metrics_set <- metric_set(rmse, rsq, mae)
    metrics_results <- predictions %>%
      regression_metrics_set(truth = !!target_sym, estimate = .pred)
    
    print(glue::glue("Metrics for FINAL {target_var_name} ({context_name}):"))
    print(metrics_results)
    
    plot <- ggplot(predictions, aes(x = .pred, y = !!target_sym)) +
      geom_point(alpha = 0.4, color = "purple") + # Changed color for current season
      geom_abline(color = "red", linetype = "dashed") +
      labs(title = glue::glue("FINAL Predicted vs Actual {target_var_name} ({context_name})"),
           x = "Predicted Score", y = "Actual Score") +
      theme_bw()
    print(plot)
    
    print(glue::glue("Variable Importance for FINAL {target_var_name} ({context_name}):"))
    tryCatch({
      vip_plot <- vip(extract_fit_engine(final_fitted_wf), num_features = 15, geom = "col") + # Reduced num_features
        labs(title = glue::glue("FINAL Feature Importance - {target_var_name} ({context_name})")) +
        theme_minimal(base_size = 10) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(vip_plot)
    }, error = function(e) {
      warning(paste("Could not generate VIP plot for FINAL", target_var_name, ":", e$message))
    })
  } else {
    print(glue::glue("Test data for {target_var_name} is empty. Skipping evaluation on test set."))
  }
  
  return(final_fitted_wf)
}

# --- Train Home Score Model for Current Season ---
fitted_current_home_score_wf <- train_tune_evaluate_single_score_model(
  target_var_name = "homeScore_target",
  train_data = train_data_score_cs,
  test_data = test_data_score_cs,
  cv_folds = folds_cs, # Use folds specific to current season data
  model_spec_tuneable = score_model_spec_rf_tune_cs,
  context_name = "Current Season"
)

# --- Train Away Score Model for Current Season ---
folds_away_cs <- vfold_cv(train_data_score_cs, v = N_CV_FOLDS_CURRENT, strata = awayScore_target)

fitted_current_away_score_wf <- train_tune_evaluate_single_score_model(
  target_var_name = "awayScore_target",
  train_data = train_data_score_cs,
  test_data = test_data_score_cs,
  cv_folds = folds_away_cs, 
  model_spec_tuneable = score_model_spec_rf_tune_cs,
  context_name = "Current Season"
)

# --- Save the Fitted Workflows ---
ml_results_dir <- "ml_results"
if (!dir.exists(ml_results_dir)) dir.create(ml_results_dir, recursive = TRUE)

if (!is.null(fitted_current_home_score_wf)) {
  saveRDS(fitted_current_home_score_wf, file.path(ml_results_dir, "score_predictor_current_home_tuned_wf.rds"))
  print("Current Season Home Score model (tuned) saved as score_predictor_current_home_tuned_wf.rds")
}
if (!is.null(fitted_current_away_score_wf)) {
  saveRDS(fitted_current_away_score_wf, file.path(ml_results_dir, "score_predictor_current_away_tuned_wf.rds"))
  print("Current Season Away Score model (tuned) saved as score_predictor_current_away_tuned_wf.rds")
}

print(glue::glue("--- Finished Training IMPROVED Score Predictors for Current Season ---"))

# --- Optional: Stop parallel backend ---
# if(exists("cl") && inherits(cl, "cluster")) {
#   tryCatch({
#     print("Stopping parallel backend.")
#     stopCluster(cl)
#   }, error = function(e) {
#     warning("Could not stop parallel cluster: ", e$message)
#   })
# }
# --- End Optional Parallel Processing Stop ---
# --- END FILE: models/11_train_score_predictor_current.R (Tuning, Rolling Features, Faster Setup) ---