# --- START FILE: models/09_train_score_predictor_modern.R (Corrected Recipe Usage in Helper, Tuning & Rolling Features) ---
# NBA Modern Era (2010-2025) Score Predictor (Home Score & Away Score)

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
# library(doParallel) # Uncomment for parallel processing during tuning

# --- Configuration ---
MODERN_ERA_START_DATE <- as.Date("2009-10-01") # Corresponds to season ending in 2010
MODERN_ERA_END_DATE   <- as.Date("2025-07-01") # Covers up to season ending in 2025
# Please adjust MODERN_ERA_END_DATE if your Games.csv has a different end point for "Modern"

N_CV_FOLDS <- 5 
ROLLING_AVG_SCORE_WINDOW <- 5 

print(glue::glue("--- Training IMPROVED Score Predictors for Modern Era ({year(MODERN_ERA_START_DATE)+1}-{year(MODERN_ERA_END_DATE)}) ---"))
print(glue::glue("--- Using {N_CV_FOLDS}-fold CV for tuning and rolling score window of {ROLLING_AVG_SCORE_WINDOW} ---"))

# --- Optional: Setup parallel processing for tuning ---
# num_cores <- parallel::detectCores(logical = FALSE)
# if (num_cores > 1) {
#   cl <- makePSOCKcluster(num_cores - 1)
#   registerDoParallel(cl)
#   print(glue::glue("Registered parallel backend with {getDoParWorkers()} cores."))
# } else {
#   print("Single core detected, parallel processing not enabled for tuning.")
# }
# --- End Optional Parallel Processing Setup ---

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

# --- Data Cleaning & Initial Feature Engineering ---
print("Cleaning data and initial feature engineering for Modern Era scores...")
nba_data_modern <- nba_data_raw %>%
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
  filter(gameDate >= MODERN_ERA_START_DATE & gameDate <= MODERN_ERA_END_DATE,
         !is.na(homeScore_target), !is.na(awayScore_target)) %>%
  select(gameId, gameDate, season, month, day_of_week, hometeam, awayteam, 
         homeScore_target, awayScore_target, winner_temp)

if(nrow(nba_data_modern) == 0) stop(glue::glue("No game data for Modern Era."))
print(glue::glue("Found {nrow(nba_data_modern)} games for Modern Era initial processing."))

# --- Advanced Feature Engineering ---
print("Performing advanced feature engineering (strengths, win rates, rolling scores)...")
home_team_offensive_strength <- nba_data_modern %>% group_by(hometeam) %>% summarise(h_offensive_strength_period = mean(homeScore_target, na.rm = TRUE), .groups = "drop")
away_team_offensive_strength <- nba_data_modern %>% group_by(awayteam) %>% summarise(a_offensive_strength_period = mean(awayScore_target, na.rm = TRUE), .groups = "drop")
home_team_defensive_strength <- nba_data_modern %>% group_by(hometeam) %>% summarise(h_defensive_strength_period = mean(awayScore_target, na.rm = TRUE), .groups = "drop")
away_team_defensive_strength <- nba_data_modern %>% group_by(awayteam) %>% summarise(a_defensive_strength_period = mean(homeScore_target, na.rm = TRUE), .groups = "drop")

nba_data_features <- nba_data_modern %>%
  left_join(home_team_offensive_strength, by = "hometeam") %>%
  left_join(away_team_offensive_strength, by = "awayteam") %>%
  left_join(home_team_defensive_strength, by = "hometeam") %>%
  left_join(away_team_defensive_strength, by = "awayteam")

home_team_win_rates_season <- nba_data_features %>% group_by(hometeam, season) %>% summarise(h_win_rate_season = mean(winner_temp, na.rm = TRUE), .groups = "drop")
away_team_win_rates_season <- nba_data_features %>% group_by(awayteam, season) %>% summarise(a_win_rate_season = mean(1 - winner_temp, na.rm = TRUE), .groups = "drop")
nba_data_features <- nba_data_features %>%
  left_join(home_team_win_rates_season, by = c("hometeam", "season")) %>%
  left_join(away_team_win_rates_season, by = c("awayteam", "season"))

nba_data_features <- nba_data_features %>%
  group_by(hometeam, season) %>% arrange(gameDate) %>%
  mutate(h_lastN_win_rate_season = zoo::rollapply(winner_temp, width = ROLLING_AVG_SCORE_WINDOW, FUN = mean, fill = NA, align = "right", partial = TRUE)) %>%
  ungroup() %>%
  group_by(awayteam, season) %>% arrange(gameDate) %>%
  mutate(a_lastN_win_rate_season = zoo::rollapply(1 - winner_temp, width = ROLLING_AVG_SCORE_WINDOW, FUN = mean, fill = NA, align = "right", partial = TRUE)) %>%
  ungroup()

nba_data_features <- nba_data_features %>%
  group_by(hometeam, season) %>% arrange(gameDate) %>%
  mutate(
    h_avg_score_lastN = zoo::rollapply(homeScore_target, width = ROLLING_AVG_SCORE_WINDOW, FUN = mean, fill = NA, align = "right", partial = TRUE),
    h_avg_pts_allowed_lastN = zoo::rollapply(awayScore_target, width = ROLLING_AVG_SCORE_WINDOW, FUN = mean, fill = NA, align = "right", partial = TRUE)
  ) %>% ungroup() %>%
  group_by(awayteam, season) %>% arrange(gameDate) %>%
  mutate(
    a_avg_score_lastN = zoo::rollapply(awayScore_target, width = ROLLING_AVG_SCORE_WINDOW, FUN = mean, fill = NA, align = "right", partial = TRUE),
    a_avg_pts_allowed_lastN = zoo::rollapply(homeScore_target, width = ROLLING_AVG_SCORE_WINDOW, FUN = mean, fill = NA, align = "right", partial = TRUE)
  ) %>% ungroup()

nba_data_features <- nba_data_features %>%
  select(-winner_temp) %>% 
  mutate( 
    h_offensive_strength_period = tidyr::replace_na(h_offensive_strength_period, mean(h_offensive_strength_period, na.rm=TRUE) %||% 105),
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

if(nrow(nba_data_features) < 50) {
  warning(glue::glue("Very few rows ({nrow(nba_data_features)}) remaining. Score model training might be unreliable."))
  if(nrow(nba_data_features) == 0) stop("No data left. Stopping.")
}
print(glue::glue("Feature engineering complete. Processed data has {nrow(nba_data_features)} rows."))

# --- Data Splitting ---
set.seed(99) 
data_split_score <- initial_split(nba_data_features, prop = 0.75, strata = homeScore_target)
train_data_score <- training(data_split_score)
test_data_score  <- testing(data_split_score)
folds <- vfold_cv(train_data_score, v = N_CV_FOLDS, strata = homeScore_target)

print(glue::glue("Data split: {nrow(train_data_score)} training, {nrow(test_data_score)} testing. Using {N_CV_FOLDS} CV folds."))

# --- Define TUNABLE Regression Model Specification ---
score_model_spec_rf_tune <- rand_forest(
  trees = 500, 
  mtry = tune(),
  min_n = tune()
) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")

# --- Helper Function to Train, Tune, and Evaluate a Score Model ---
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
  
  # *** CORRECTED RECIPE DEFINITION ***
  # Initialize recipe with the data, then define roles explicitly
  # This ensures all columns from train_data are initially known to the recipe.
  score_recipe <- recipe(train_data) |> 
    update_role(!!target_sym, new_role = "outcome") |>
    update_role(all_of(current_predictors), new_role = "predictor") |>
    # Ensure gameId, gameDate, and the other score target are treated as IDs
    # and are not accidentally used as predictors if not in current_predictors.
    update_role(!!rlang::sym(other_score_target_col), gameId, gameDate, new_role = "ID") |>
    # Preprocessing steps will now correctly operate on columns with 'predictor' role
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_corr(all_numeric_predictors(), threshold = 0.9) |> 
    step_normalize(all_numeric_predictors())
  # *** END CORRECTED RECIPE DEFINITION ***
  
  score_wf <- workflow(preprocessor = score_recipe, spec = model_spec_tuneable) # More explicit workflow creation
  
  # Define mtry range dynamically after recipe is prepped (more robust)
  # This is a bit more advanced but good practice.
  # For simplicity here, we'll use a fixed range, but be aware of this.
  # prepped_recipe <- prep(score_recipe)
  # num_effective_predictors <- bake(prepped_recipe, new_data = NULL) %>% 
  #                               select(-all_outcomes(), -any_of(c(other_score_target_col, "gameId", "gameDate"))) %>% 
  #                               ncol()
  # print(glue::glue("Number of effective predictors for mtry for {target_var_name}: {num_effective_predictors}"))
  # mtry_upper_bound <- max(1L, min(num_effective_predictors, 25L)) # Cap at 25 or actual num_predictors
  
  # Simplified grid for faster execution. Expand levels for more thorough tuning.
  rf_grid <- grid_regular( 
    mtry(range = c(max(1L, floor(length(current_predictors)/4)), # Adjusted lower bound
                   min(length(current_predictors), 20L))),      # Adjusted upper bound
    min_n(range = c(2L, 15L)), 
    levels = 2 # Reduced levels (2x2 = 4 combinations for quicker run)
  )
  
  print(glue::glue("Starting hyperparameter tuning for {target_var_name} ({N_CV_FOLDS}-fold CV, {nrow(rf_grid)} candidates)..."))
  print(glue::glue("Base predictor count for mtry range reference: {length(current_predictors)}"))
  print("Tuning grid:")
  print(rf_grid)
  
  tune_res <- tune_grid(
    score_wf,
    resamples = cv_folds,
    grid = rf_grid,
    metrics = metric_set(rmse, rsq),
    control = control_grid(save_pred = FALSE, verbose = TRUE, allow_par = TRUE) # allow_par for parallel
  )
  
  print(glue::glue("Tuning complete for {target_var_name}."))
  print(glue::glue("Best {min(5, nrow(rf_grid))} hyperparameter sets based on RMSE for {target_var_name}:"))
  show_best(tune_res, metric = "rmse", n = min(5, nrow(rf_grid)))
  best_hp <- select_best(tune_res, metric = "rmse")
  print(glue::glue("Best hyperparameters for {target_var_name} based on RMSE:"))
  print(best_hp)
  
  final_wf <- finalize_workflow(score_wf, best_hp)
  
  print(glue::glue("Fitting final model for {target_var_name} with best hyperparameters..."))
  final_fitted_wf <- fit(final_wf, data = train_data) 
  print(glue::glue("Final model fitting complete for {target_var_name}."))
  
  print(glue::glue("Evaluating final model for {target_var_name} on test data..."))
  predictions <- predict(final_fitted_wf, new_data = test_data) %>%
    bind_cols(test_data %>% select(!!target_sym))
  
  regression_metrics_set <- metric_set(rmse, rsq, mae)
  metrics_results <- predictions %>%
    regression_metrics_set(truth = !!target_sym, estimate = .pred)
  
  print(glue::glue("Metrics for FINAL {target_var_name} ({context_name}):"))
  print(metrics_results)
  
  plot <- ggplot(predictions, aes(x = .pred, y = !!target_sym)) +
    geom_point(alpha = 0.3, color = "darkgreen") +
    geom_abline(color = "red", linetype = "dashed") +
    labs(title = glue::glue("FINAL Predicted vs Actual {target_var_name} ({context_name})"),
         x = "Predicted Score", y = "Actual Score") +
    theme_bw()
  print(plot)
  
  print(glue::glue("Variable Importance for FINAL {target_var_name} ({context_name}):"))
  tryCatch({
    vip_plot <- vip(extract_fit_engine(final_fitted_wf), num_features = 20, geom = "col") +
      labs(title = glue::glue("FINAL Feature Importance - {target_var_name} ({context_name})")) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(vip_plot)
  }, error = function(e) {
    warning(paste("Could not generate VIP plot for FINAL", target_var_name, ":", e$message))
  })
  
  return(final_fitted_wf)
}

# --- Train Home Score Model for Modern Era (with Tuning) ---
fitted_modern_home_score_wf <- train_tune_evaluate_single_score_model(
  target_var_name = "homeScore_target",
  train_data = train_data_score,
  test_data = test_data_score,
  cv_folds = folds,
  model_spec_tuneable = score_model_spec_rf_tune,
  context_name = "Modern Era"
)

# --- Train Away Score Model for Modern Era (with Tuning) ---
folds_away <- vfold_cv(train_data_score, v = N_CV_FOLDS, strata = awayScore_target)

fitted_modern_away_score_wf <- train_tune_evaluate_single_score_model(
  target_var_name = "awayScore_target",
  train_data = train_data_score,
  test_data = test_data_score,
  cv_folds = folds_away, 
  model_spec_tuneable = score_model_spec_rf_tune,
  context_name = "Modern Era"
)

# --- Save the Fitted Workflows ---
ml_results_dir <- "ml_results"
if (!dir.exists(ml_results_dir)) dir.create(ml_results_dir, recursive = TRUE)

if (!is.null(fitted_modern_home_score_wf)) {
  saveRDS(fitted_modern_home_score_wf, file.path(ml_results_dir, "score_predictor_modern_home_tuned_wf.rds"))
  print("Modern Home Score model (tuned) saved as score_predictor_modern_home_tuned_wf.rds")
}
if (!is.null(fitted_modern_away_score_wf)) {
  saveRDS(fitted_modern_away_score_wf, file.path(ml_results_dir, "score_predictor_modern_away_tuned_wf.rds"))
  print("Modern Away Score model (tuned) saved as score_predictor_modern_away_tuned_wf.rds")
}

print(glue::glue("--- Finished Training IMPROVED Score Predictors for Modern Era ---"))

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
# --- END FILE: models/09_train_score_predictor_modern.R (Corrected Recipe Usage in Helper, Tuning & Rolling Features) ---