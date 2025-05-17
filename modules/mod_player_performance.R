# --- START FILE: modules/mod_player_performance.R ---
# Module for Player Performance Prediction (PRA, FG%, +/-)
# VERSION: Combined UI and Server with Sub-Tabs for PRA, Clutch, Impact models
# Includes fixes for NA labels and missing columns during prediction.

# Needed libraries (Ensure loaded in global.R):
# shiny, dplyr, tidymodels, shinycssloaders, glue, readr, tibble,
# lubridate, bslib, rlang, scales, stats, tidyr

# --- UI Function ---
#' Player Performance Module UI Function
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_player_performance_ui <- function(id){
  ns <- NS(id)
  tagList(
    # --- Header and Description ---
    h3("Player Performance Prediction"),
    p("Select a player, opponent, game location, and estimated days rest to predict performance metrics based on historical models."),
    
    # --- Input Controls Grouped in a wellPanel ---
    wellPanel(
      fluidRow(
        column(6,
               selectizeInput(ns("player_select"),
                              label = "Select Player:",
                              choices = NULL, # Populated by server
                              options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
        ),
        column(6,
               selectizeInput(ns("opponent_select"),
                              label = "Select Opponent Team:",
                              choices = NULL, # Populated by server
                              options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
        )
      ),
      fluidRow(
        column(6,
               radioButtons(ns("location_select"),
                            label = "Game Location:",
                            choices = c("Home" = "1", "Away" = "0"), # Values '1'/'0' match factor levels
                            selected = "1",
                            inline = TRUE)
        ),
        column(6,
               numericInput(ns("days_rest_input"),
                            label = "Days Rest Since Last Game:",
                            value = 2, min = 0, max = 30, step = 1)
        )
      ),
      fluidRow(
        column(12, align = "center",
               actionButton(ns("predict_perf"), "Predict Performance", icon = icon("calculator"), class = "btn-primary")
        )
      )
    ), # end wellPanel
    
    # --- Tabset Panel for Predictions ---
    tabsetPanel(
      id = ns("prediction_tabs"), # ID for the tabset
      type = "pills",           # Use pills style
      
      # --- Tab 1: Stats (PRA) ---
      tabPanel(
        title = "Stats (PRA)",
        value = ns("tab_pra"), # Optional value for the tab
        br(), # Add space below tab title
        withSpinner(uiOutput(ns("pra_output")), type = 6, color = "#1d428a") # Spinner + Output
      ), # End PRA TabPanel
      
      # --- Tab 2: Shooting (FG%) ---
      tabPanel(
        title = "Shooting (FG%)",
        value = ns("tab_shooting"),
        br(),
        withSpinner(uiOutput(ns("shooting_output")), type = 6, color = "#1d428a") # Spinner + Output
      ), # End Shooting TabPanel
      
      # --- Tab 3: Impact (+/-) ---
      tabPanel(
        title = "Impact (+/-)",
        value = ns("tab_impact"),
        br(),
        withSpinner(uiOutput(ns("impact_output")), type = 6, color = "#1d428a") # Spinner + Output
      ) # End Impact TabPanel
      
    ) # End tabsetPanel
    
  ) # end tagList
}


# --- Player Performance Module Server Function ---
#' @param id Internal parameters for {shiny}.
#' @param active_tab A reactive expression containing the value of input$main_nav
#' @param tab_value The specific value string for this module's tab panel
#' @noRd
mod_player_performance_server <- function(id, active_tab, tab_value){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # --- Reactive Values ---
    rv <- reactiveValues(
      models = list(),        # Store loaded models (points, rebounds, assists, clutch, impact)
      player_list = NULL,     # For player dropdown
      team_list = NULL,       # For opponent dropdown
      player_stats_df = NULL, # To store PlayerStatistics.csv data (processed)
      models_loaded = FALSE,  # Flag for deferred loading status
      loading_error = NULL,   # Store initialization errors
      current_predictions = NULL, # Store latest successful prediction list
      prediction_error = NULL     # Store errors from the prediction step
    )
    
    # --- Load Data & Models (Deferred) ---
    observeEvent(active_tab(), {
      req(active_tab() == tab_value, !rv$models_loaded)
      print(paste("Player Performance Module: Tab activated (", tab_value, "). Loading models and data..."))
      
      rv$loading_error <- NULL
      load_success <- TRUE
      
      tryCatch({
        # --- 1. Load Models ---
        model_dir <- "ml_results"
        # Define all models to load
        model_files <- c(points = "pra_points_wf.rds",
                         rebounds = "pra_rebounds_wf.rds",
                         assists = "pra_assists_wf.rds",
                         clutch = "clutch_model_predictive_fit.rds", # Predictive Clutch
                         impact = "player_impact_predictive_fit.rds" # Predictive Impact
        )
        
        for(model_name in names(model_files)) {
          model_path <- file.path(model_dir, model_files[[model_name]])
          if (!file.exists(model_path)) stop(glue::glue("Model file not found: {model_files[[model_name]]} in {model_dir}"))
          rv$models[[model_name]] <- readRDS(model_path)
          print(glue::glue("Player Performance Module: Loaded model '{model_name}'."))
        }
        if (length(rv$models) != length(model_files)) {
          stop("One or more performance models failed to load into the reactive value list.")
        }
        print("Player Performance Module: All required performance models loaded successfully.")
        
        # --- 2. Load Player Statistics Data (Expanded Columns) ---
        player_data_path <- "data/PlayerStatistics.csv"
        if (!file.exists(player_data_path)) stop("PlayerStatistics.csv not found in data/ directory.")
        
        raw_stats_df <- readr::read_csv(player_data_path, show_col_types = FALSE)
        
        if (!exists("rebranded_teams", envir = .GlobalEnv)) {
          stop("The 'rebranded_teams' map is not defined in the global environment (global.R).")
        }
        local_rebranded_teams <- get("rebranded_teams", envir = .GlobalEnv)
        
        # Define columns needed for ALL models' feature engineering + display
        required_raw_cols <- c("gameDate", "personId", "firstName", "lastName",
                               "playerteamCity", "playerteamName", # For player's team (needed for home calc later)
                               "opponentteamCity", "opponentteamName", # For opponent rebranding
                               "fieldGoalsMade", "fieldGoalsAttempted",
                               "threePointersMade", "threePointersAttempted",
                               "freeThrowsMade", "freeThrowsAttempted",
                               "points", "assists", "reboundsTotal",
                               "steals", "blocks", "turnovers",
                               "plusMinusPoints", "numMinutes", "gameType") # Added gameType
        
        missing_cols <- setdiff(required_raw_cols, names(raw_stats_df))
        if (length(missing_cols) > 0) {
          stop(glue::glue("Missing required columns in PlayerStatistics.csv: {paste(missing_cols, collapse=', ')}"))
        }
        
        rv$player_stats_df <- raw_stats_df %>%
          mutate(
            # Parse Date first, handle potential parsing failures
            gameDate = lubridate::ymd_hms(gameDate, quiet = TRUE), # quiet suppresses individual warnings
            player_name = str_trim(paste(firstName, lastName)),
            # Rebrand opponent team name
            opponent_name_rebranded = recode(str_trim(paste(opponentteamCity, opponentteamName)),
                                             !!!local_rebranded_teams,
                                             .default = str_trim(paste(opponentteamCity, opponentteamName))),
            # Rebrand player's team name (needed for home status check later)
            team_name_rebranded = recode(str_trim(paste(playerteamCity, playerteamName)),
                                         !!!local_rebranded_teams,
                                         .default = str_trim(paste(playerteamCity, playerteamName))),
            # Ensure gameType is a factor with expected levels
            gameType = factor(gameType, levels = c("Regular Season", "Playoffs")),
            # Convert all relevant stats to numeric safely
            across(all_of(c("fieldGoalsMade", "fieldGoalsAttempted", "threePointersMade", "threePointersAttempted",
                            "freeThrowsMade", "freeThrowsAttempted", "points", "assists", "reboundsTotal",
                            "steals", "blocks", "turnovers", "plusMinusPoints", "numMinutes")),
                   ~ suppressWarnings(as.numeric(.))),
            # Calculate percentages needed for historical averages, handling division by zero
            fieldGoalPercentage_calc = ifelse(fieldGoalsAttempted > 0, fieldGoalsMade / fieldGoalsAttempted, NA_real_),
            threePointPercentage_calc = ifelse(threePointersAttempted > 0, threePointersMade / threePointersAttempted, NA_real_),
            freeThrowPercentage_calc = ifelse(freeThrowsAttempted > 0, freeThrowsMade / freeThrowsAttempted, NA_real_)
          ) %>%
          # Select only necessary columns
          select(
            gameDate, personId, player_name, team_name_rebranded, opponent_name_rebranded, gameType,
            points, assists, reboundsTotal, steals, blocks, turnovers, plusMinusPoints, numMinutes,
            fieldGoalsMade, fieldGoalsAttempted, threePointersMade, threePointersAttempted,
            freeThrowsMade, freeThrowsAttempted,
            fieldGoalPercentage_calc, threePointPercentage_calc, freeThrowPercentage_calc
          ) %>%
          # Filter invalid rows (missing critical info or failed date parse)
          filter(!is.na(player_name), nchar(player_name) > 0, !is.na(personId), !is.na(gameDate))
        
        # Report date parsing issues if any occurred
        date_parse_failures <- sum(is.na(raw_stats_df$gameDate)) - sum(is.na(rv$player_stats_df$gameDate)) # Rough check
        if(date_parse_failures > 0) {
          warning(glue::glue("Player Performance Module: {date_parse_failures} rows likely failed gameDate parsing in PlayerStatistics.csv"))
        }
        
        print(glue::glue("Player Performance Module: Player stats data loaded and processed ({nrow(rv$player_stats_df)} rows)."))
        if (nrow(rv$player_stats_df) == 0) stop("Player statistics data is empty after loading/processing.")
        
        # --- 3. Prepare Lists for UI ---
        rv$player_list <- rv$player_stats_df %>%
          distinct(player_name) %>%
          arrange(player_name) %>%
          pull(player_name)
        
        all_rebranded_opponents <- unique(rv$player_stats_df$opponent_name_rebranded)
        known_teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets",
                         "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets",
                         "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
                         "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat",
                         "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks",
                         "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
                         "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
                         "Utah Jazz", "Washington Wizards")
        rv$team_list <- intersect(all_rebranded_opponents, known_teams) %>% sort()
        
        if (length(rv$player_list) == 0) stop("Failed to generate player list.")
        if (length(rv$team_list) == 0) stop("Failed to generate opponent team list.")
        
        # --- 4. Update UI Controls ---
        common_options <- list(placeholder = 'Type or select...', onInitialize = I('function() { this.setValue(""); }'))
        updateSelectizeInput(session, "player_select", choices = rv$player_list, selected = "", options = common_options, server = TRUE)
        updateSelectizeInput(session, "opponent_select", choices = rv$team_list, selected = "", options = common_options, server = TRUE)
        
        rv$models_loaded <- TRUE
        print("Player Performance Module: Initial loading complete and successful. UI updated.")
        
      }, error = function(e) {
        load_success <<- FALSE
        rv$loading_error <<- paste("Error during module initialization:", e$message)
        print(paste("Player Performance Module: ERROR -", rv$loading_error))
        updateSelectizeInput(session, "player_select", choices = c("! Error Loading !" = ""), selected = "")
        updateSelectizeInput(session, "opponent_select", choices = c("! Error Loading !" = ""), selected = "")
      })
      
      # Disable/Enable prediction button
      if (!load_success) {
        tryCatch(updateActionButton(session, "predict_perf", label = "Unavailable - Reload App", disabled = TRUE), error = function(e) {})
      } else {
        tryCatch(updateActionButton(session, "predict_perf", label = "Predict Performance", disabled = FALSE), error = function(e) {})
      }
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE) # End observeEvent(active_tab)
    
    
    # --- Prediction Logic ---
    prediction_results <- eventReactive(input$predict_perf, {
      req(rv$models_loaded,
          input$player_select, input$player_select != "",
          input$opponent_select, input$opponent_select != "",
          !is.null(input$location_select),
          !is.null(input$days_rest_input), is.numeric(input$days_rest_input)
      )
      print("Prediction triggered: Requirements met.")
      
      validate(
        need(input$player_select %in% rv$player_list, "Selected player not found."),
        need(input$opponent_select %in% rv$team_list, "Selected opponent not found."),
        need(input$days_rest_input >= 0, "Days rest cannot be negative.")
      )
      print("Prediction triggered: Input validation passed.")
      
      rv$prediction_error <- NULL
      rv$current_predictions <- NULL # Clear previous predictions
      print(glue::glue("Starting prediction for: Player='{input$player_select}', Opponent='{input$opponent_select}', Location='{input$location_select}', Rest={input$days_rest_input}"))
      
      # --- Feature Engineering ---
      pra_input_tibble <- NULL
      clutch_input_tibble <- NULL
      impact_input_tibble <- NULL
      feature_eng_success <- TRUE
      
      tryCatch({
        selected_player <- input$player_select
        selected_opponent <- input$opponent_select
        is_home <- as.factor(input$location_select) # Factor '1' or '0'
        days_rest <- as.numeric(input$days_rest_input)
        # Assume Regular Season game type for prediction context, ensure levels match training data
        gameType_levels <- levels(rv$player_stats_df$gameType) %||% c("Regular Season", "Playoffs")
        gameType_factor <- factor("Regular Season", levels = gameType_levels)
        
        # Filter Player's Historical Data
        player_hist_data <- rv$player_stats_df %>%
          filter(player_name == selected_player) %>%
          arrange(gameDate) # IMPORTANT: Ensure sorted by date
        
        if(nrow(player_hist_data) == 0) {
          stop(glue::glue("No historical data found for player: {selected_player}."))
        }
        print(glue::glue("Filtered historical data for {selected_player}: {nrow(player_hist_data)} games found."))
        
        # --- Calculate Overall Historical Averages & Percentages ---
        safe_weighted_mean <- function(value, weight, default) {
          valid_idx <- !is.na(value) & !is.na(weight) & weight > 0
          if (sum(valid_idx) == 0) return(default)
          stats::weighted.mean(value[valid_idx], w = weight[valid_idx], na.rm = TRUE)
        }
        
        avg_stats <- player_hist_data %>%
          summarise(
            hist_avg_pts = mean(points, na.rm = TRUE),
            hist_avg_ast = mean(assists, na.rm = TRUE),
            hist_avg_reb = mean(reboundsTotal, na.rm = TRUE),
            hist_avg_stl = mean(steals, na.rm = TRUE),
            hist_avg_blk = mean(blocks, na.rm = TRUE),
            hist_avg_tov = mean(turnovers, na.rm = TRUE),
            hist_avg_pm = mean(plusMinusPoints, na.rm = TRUE),
            hist_avg_minutes = mean(numMinutes, na.rm = TRUE),
            hist_avg_fg_pct = safe_weighted_mean(fieldGoalPercentage_calc, fieldGoalsAttempted, 0.450),
            hist_avg_3p_pct = safe_weighted_mean(threePointPercentage_calc, threePointersAttempted, 0.350),
            hist_avg_ft_pct = safe_weighted_mean(freeThrowPercentage_calc, freeThrowsAttempted, 0.750),
            .groups = "drop"
          ) %>%
          mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))
        
        avg_stats <- avg_stats %>%
          mutate(
            hist_avg_fg_pct = ifelse(hist_avg_fg_pct == 0, 0.450, hist_avg_fg_pct),
            hist_avg_3p_pct = ifelse(hist_avg_3p_pct == 0, 0.350, hist_avg_3p_pct),
            hist_avg_ft_pct = ifelse(hist_avg_ft_pct == 0, 0.750, hist_avg_ft_pct)
          )
        
        print("Calculated overall historical averages:")
        print(glimpse(avg_stats))
        
        # --- Calculate Historical Average Points vs Selected Opponent ---
        player_vs_opp_data <- player_hist_data %>%
          filter(opponent_name_rebranded == selected_opponent)
        
        avg_pts_vs_opp_final <- if(nrow(player_vs_opp_data) > 0) {
          mean(player_vs_opp_data$points, na.rm = TRUE) %||% avg_stats$hist_avg_pts
        } else {
          print(glue::glue("No games found vs {selected_opponent}. Using overall avg_pts."))
          avg_stats$hist_avg_pts
        }
        avg_pts_vs_opp_final <- tidyr::replace_na(avg_pts_vs_opp_final, 0)
        
        print(glue::glue("Avg pts vs {selected_opponent} (or fallback): {round(avg_pts_vs_opp_final, 1)}"))
        
        # --- Get Previous Game Stats (Momentum) ---
        prev_game_stats <- player_hist_data %>%
          slice_tail(n = 1) %>% # Get the last row (most recent game)
          select(momentum_last_game_pts = points, momentum_last_game_pm = plusMinusPoints)
        
        if (nrow(prev_game_stats) == 0) {
          prev_game_stats <- tibble(momentum_last_game_pts = 0, momentum_last_game_pm = 0)
        } else {
          prev_game_stats <- prev_game_stats %>%
            mutate(
              momentum_last_game_pts = tidyr::replace_na(momentum_last_game_pts, 0),
              momentum_last_game_pm = tidyr::replace_na(momentum_last_game_pm, 0)
            )
        }
        
        print("Previous game stats (momentum):")
        print(prev_game_stats)
        
        # --- Construct Input Tibbles ---
        placeholder_personId <- player_hist_data$personId[1] %||% NA_integer_
        placeholder_team_name <- player_hist_data$team_name_rebranded[1] %||% NA_character_
        placeholder_gameDate <- Sys.time() # Use current time as a dummy date
        
        # PRA Input Tibble (Model 05 - Points, Rebounds, Assists)
        # Needs all columns from original training data due to `recipe(... ~ .)`
        pra_input_tibble <- tibble::tibble(
          home = is_home,
          fieldGoalPercentage = avg_stats$hist_avg_fg_pct,
          threePointPercentage = avg_stats$hist_avg_3p_pct,
          freeThrowPercentage = avg_stats$hist_avg_ft_pct,
          days_rest = days_rest,
          player_historical_avg_pts = avg_stats$hist_avg_pts,
          player_hist_avg_pts_vs_opp = avg_pts_vs_opp_final,
          # --- Columns required by recipe structure but assigned ID role ---
          personId = placeholder_personId,
          player_name = selected_player,
          team_name = placeholder_team_name,
          opponent_name = selected_opponent,
          gameDate = placeholder_gameDate,
          # --- Other outcome columns (given ID role in points_rec, etc.) ---
          reboundsTotal = NA_real_, # Placeholder
          assists = NA_real_,       # Placeholder
          # --- Target variable (needed by recipe structure) ---
          points = NA_real_         # Placeholder for the outcome itself
        )
        print("PRA Input Tibble (for Points/Rebounds/Assists):")
        print(glimpse(pra_input_tibble))
        
        # Clutch Input Tibble (Model 03)
        clutch_input_tibble <- tibble::tibble(
          home = is_home,
          gameType = gameType_factor,
          days_rest = days_rest,
          historical_avg_fg_pct = avg_stats$hist_avg_fg_pct,
          historical_avg_ft_pct = avg_stats$hist_avg_ft_pct,
          historical_avg_pts = avg_stats$hist_avg_pts,
          historical_avg_minutes = avg_stats$hist_avg_minutes,
          momentum_last_game = prev_game_stats$momentum_last_game_pts,
          # --- Columns required by recipe structure but assigned ID role ---
          personId = placeholder_personId,
          player_name = selected_player,
          team_name = placeholder_team_name,
          opponent_name = selected_opponent,
          gameDate = placeholder_gameDate,
          # --- Target variable (needed by recipe structure) ---
          clutch_shooting_percentage = NA_real_
        )
        print("Clutch Input Tibble:")
        print(glimpse(clutch_input_tibble))
        
        # Impact Input Tibble (Model 04)
        impact_input_tibble <- tibble::tibble(
          home_game = is_home, # Model 04 used 'home_game'
          gameType = gameType_factor,
          days_rest = days_rest,
          momentum_last_game_pm = prev_game_stats$momentum_last_game_pm,
          historical_avg_pm = avg_stats$hist_avg_pm,
          historical_avg_pts = avg_stats$hist_avg_pts,
          historical_avg_ast = avg_stats$hist_avg_ast,
          historical_avg_reb = avg_stats$hist_avg_reb,
          historical_avg_stl = avg_stats$hist_avg_stl,
          historical_avg_blk = avg_stats$hist_avg_blk,
          historical_avg_tov = avg_stats$hist_avg_tov,
          historical_avg_minutes = avg_stats$hist_avg_minutes,
          historical_avg_fg_pct = avg_stats$hist_avg_fg_pct,
          historical_avg_3p_pct = avg_stats$hist_avg_3p_pct,
          historical_avg_ft_pct = avg_stats$hist_avg_ft_pct,
          # --- Columns required by recipe structure but assigned ID role ---
          personId = placeholder_personId,
          player_name = selected_player,
          team_name = placeholder_team_name,
          opponent_name = selected_opponent,
          gameDate = placeholder_gameDate,
          # --- Target variable (needed by recipe structure) ---
          plusMinusPoints = NA_real_
        )
        print("Impact Input Tibble:")
        print(glimpse(impact_input_tibble))
        
        print("Feature engineering successful.")
        
      }, error = function(e) {
        feature_eng_success <<- FALSE
        rv$prediction_error <<- paste("Error preparing input features:", e$message)
        print(paste("Player Performance Module: ERROR during feature engineering -", rv$prediction_error))
      }) # End tryCatch for feature engineering
      
      # --- Run Prediction ---
      if (feature_eng_success) {
        results <- list()
        prediction_success <- TRUE
        
        tryCatch({
          print("Predicting Points...")
          # Use the PRA tibble for all PRA models
          pred_pts <- predict(rv$models$points, pra_input_tibble)
          results$points <- pred_pts$.pred[1]
          
          print("Predicting Rebounds...")
          pred_reb <- predict(rv$models$rebounds, pra_input_tibble)
          results$rebounds <- pred_reb$.pred[1]
          
          print("Predicting Assists...")
          pred_ast <- predict(rv$models$assists, pra_input_tibble)
          results$assists <- pred_ast$.pred[1]
          
          print("Predicting Clutch (FG%)...")
          pred_clutch <- predict(rv$models$clutch, clutch_input_tibble)
          results$clutch_fg_pct <- pmin(pmax(pred_clutch$.pred[1], 0), 1)
          
          print("Predicting Impact (+/-)...")
          pred_impact <- predict(rv$models$impact, impact_input_tibble)
          results$impact_pm <- pred_impact$.pred[1]
          
          # Store results & Log
          rv$current_predictions <- results
          print("Predictions completed successfully:")
          print(results)
          return(results)
          
        }, error = function(e) {
          prediction_success <<- FALSE
          # Capture the specific error message
          rv$prediction_error <<- paste("Error during model prediction:", e$message)
          print(paste("Player Performance Module: ERROR during prediction -", rv$prediction_error))
          rv$current_predictions <- NULL
          return(NULL)
        }) # End tryCatch for prediction
        
      } else {
        # Feature engineering failed
        rv$current_predictions <- NULL
        return(NULL)
      }
      
    }) # end eventReactive(input$predict_perf)
    
    
    # --- Render Prediction Outputs ---
    
    # Helper function to create styled output box
    render_prediction_box <- function(label, value, format_func = function(x) round(x, 1)) {
      if (is.null(value)) {
        display_value <- "N/A"
      } else {
        display_value <- tryCatch(
          if(is.na(value)) "N/A" else format_func(value),
          error = function(e) { print(paste("Formatting error for", label, ":", e$message)); "Error" }
        )
      }
      div(class="stat-prediction-box", # Reuse CSS class from styles.css
          span(class="stat-prediction-label", label),
          span(class="stat-prediction-value", display_value)
      )
    }
    
    # Render PRA Output
    output$pra_output <- renderUI({
      if (!rv$models_loaded) { return(if (!is.null(rv$loading_error)) tags$div(class = "validation-error-message", rv$loading_error) else tags$p("Loading...", style="text-align: center; color: #aaaaaa;")) }
      preds <- prediction_results() # Trigger reactive
      # Display specific prediction error if it occurred
      if (!is.null(rv$prediction_error)) { return(tags$div(class = "validation-error-message", rv$prediction_error)) }
      # If no error but no preds yet (button not clicked)
      if (is.null(preds)) { return(tags$p("Select inputs and click 'Predict Performance'.", style="text-align: center; color: #aaaaaa;")) }
      # Check specifically for PRA results within preds
      req(exists("points", where=preds), exists("rebounds", where=preds), exists("assists", where=preds))
      
      tagList(
        h4(glue("Predicted Stats for {input$player_select} vs {input$opponent_select}"), style="text-align:center; margin-bottom:20px;"),
        fluidRow(
          column(4, render_prediction_box("POINTS", preds$points)),
          column(4, render_prediction_box("REBOUNDS", preds$rebounds)),
          column(4, render_prediction_box("ASSISTS", preds$assists))
        )
      )
    })
    
    # Render Shooting Output
    output$shooting_output <- renderUI({
      if (!rv$models_loaded) { return(if (!is.null(rv$loading_error)) tags$div(class = "validation-error-message", rv$loading_error) else tags$p("Loading...", style="text-align: center; color: #aaaaaa;")) }
      preds <- prediction_results() # Trigger reactive
      if (!is.null(rv$prediction_error)) { return(tags$div(class = "validation-error-message", rv$prediction_error)) }
      if (is.null(preds)) { return(tags$p("Select inputs and click 'Predict Performance'.", style="text-align: center; color: #aaaaaa;")) }
      req(exists("clutch_fg_pct", where=preds))
      
      tagList(
        h4(glue("Predicted Shooting for {input$player_select} vs {input$opponent_select}"), style="text-align:center; margin-bottom:20px;"),
        fluidRow(
          column(4, offset = 4, # Center the single box
                 render_prediction_box("FIELD GOAL %", preds$clutch_fg_pct, format_func = scales::percent_format(accuracy = 0.1))
          )
        )
      )
    })
    
    # Render Impact Output
    output$impact_output <- renderUI({
      if (!rv$models_loaded) { return(if (!is.null(rv$loading_error)) tags$div(class = "validation-error-message", rv$loading_error) else tags$p("Loading...", style="text-align: center; color: #aaaaaa;")) }
      preds <- prediction_results() # Trigger reactive
      if (!is.null(rv$prediction_error)) { return(tags$div(class = "validation-error-message", rv$prediction_error)) }
      if (is.null(preds)) { return(tags$p("Select inputs and click 'Predict Performance'.", style="text-align: center; color: #aaaaaa;")) }
      req(exists("impact_pm", where=preds))
      
      format_pm <- function(x) {
        val <- round(x, 1)
        if (is.na(val)) return("N/A")
        if (val > 0) return(paste0("+", val))
        return(as.character(val))
      }
      
      tagList(
        h4(glue("Predicted Impact for {input$player_select} vs {input$opponent_select}"), style="text-align:center; margin-bottom:20px;"),
        fluidRow(
          column(4, offset = 4, # Center the single box
                 render_prediction_box("PLUS / MINUS (+/-)", preds$impact_pm, format_func = format_pm)
          )
        )
      )
    })
    
    # Ensure outputs stay rendered even if tab is hidden temporarily
    outputOptions(output, "pra_output", suspendWhenHidden = FALSE)
    outputOptions(output, "shooting_output", suspendWhenHidden = FALSE)
    outputOptions(output, "impact_output", suspendWhenHidden = FALSE)
    
  }) # end moduleServer
}
# --- END FILE: modules/mod_player_performance.R ---