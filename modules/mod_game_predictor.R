# --- START FILE: modules/mod_game_predictor.R (FINAL - Incorporates 4 Models, Refined Stats, Plausible Scores, Fixes) ---
# modules/mod_game_predictor.R
# VERSION: Includes "Current Season" model, refined avg_stats, plausible scores, and error fixes.

# Needed libraries (Ensure loaded in global.R):
# shiny, dplyr, tidymodels, shinycssloaders, glue, scales, readr, lubridate, tibble, rlang

#' Game Predictor Module UI Function
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_game_predictor_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("NBA Game Predictor"),
    p("Select a model tab below, choose teams, and click 'Predict' to see the outcome based on that specific model."),
    
    tabsetPanel(
      id = ns("predictor_tabs"),
      type = "pills",
      
      # --- Tab 1: Modern Prediction (2010-2025) ---
      tabPanel(
        title = "Modern (2010-2025)",
        value = ns("tab_modern"),
        br(),
        fluidRow(
          column(5,
                 selectizeInput(ns("home_team_modern"),
                                label = "Select Home Team:",
                                choices = NULL,
                                options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
          ),
          column(5,
                 selectizeInput(ns("away_team_modern"),
                                label = "Select Away Team:",
                                choices = NULL,
                                options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
          ),
          column(2,
                 tags$div(style="margin-top: 25px;",
                          actionButton(ns("predict_modern"), "Predict", icon = icon("bolt"), class = "btn-primary btn-block")
                 )
          )
        ),
        hr(),
        fluidRow(
          column(12, align="center",
                 withSpinner(uiOutput(ns("output_modern")), type=6, color="#1d428a")
          )
        )
      ), # End TabPanel Modern
      
      # --- Tab 2: Historical Prediction (All-Time) ---
      tabPanel(
        title = "Historical (All-Time)",
        value = ns("tab_historical"),
        br(),
        fluidRow(
          column(5,
                 selectizeInput(ns("home_team_hist"),
                                label = "Select Home Team:",
                                choices = NULL,
                                options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
          ),
          column(5,
                 selectizeInput(ns("away_team_hist"),
                                label = "Select Away Team:",
                                choices = NULL,
                                options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
          ),
          column(2,
                 tags$div(style="margin-top: 25px;",
                          actionButton(ns("predict_hist"), "Predict", icon = icon("bolt"), class = "btn-primary btn-block")
                 )
          )
        ),
        hr(),
        fluidRow(
          column(12, align="center",
                 withSpinner(uiOutput(ns("output_hist")), type=6, color="#1d428a")
          )
        )
      ), # End TabPanel Historical
      
      # --- Tab 3: Current Season Prediction (2024-2025) ---
      tabPanel(
        title = "Current Season (2024-2025)",
        value = ns("tab_current_season"),
        br(),
        fluidRow(
          column(5,
                 selectizeInput(ns("home_team_current"),
                                label = "Select Home Team:",
                                choices = NULL,
                                options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
          ),
          column(5,
                 selectizeInput(ns("away_team_current"),
                                label = "Select Away Team:",
                                choices = NULL,
                                options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
          ),
          column(2,
                 tags$div(style="margin-top: 25px;",
                          actionButton(ns("predict_current"), "Predict", icon = icon("bolt"), class = "btn-primary btn-block")
                 )
          )
        ),
        hr(),
        fluidRow(
          column(12, align="center",
                 withSpinner(uiOutput(ns("output_current_season")), type=6, color="#1d428a")
          )
        )
      ), # End TabPanel Current Season
      
      # --- Tab 4: Playoff Series Prediction (Demo) ---
      tabPanel(
        title = "Playoff Series (Demo)",
        value = ns("tab_playoff_demo"),
        br(),
        fluidRow(
          column(5,
                 selectizeInput(ns("team1_playoff"),
                                label = "Select Team 1:",
                                choices = NULL,
                                options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
          ),
          column(5,
                 selectizeInput(ns("team2_playoff"),
                                label = "Select Team 2:",
                                choices = NULL,
                                options = list(placeholder = 'Loading...', onInitialize = I('function() { this.setValue(""); }')))
          ),
          column(2,
                 tags$div(style="margin-top: 25px;",
                          actionButton(ns("predict_playoff"), "Predict Series", icon = icon("bolt"), class = "btn-primary btn-block")
                 )
          )
        ),
        hr(),
        fluidRow(
          column(12, align="center",
                 withSpinner(uiOutput(ns("output_playoff")), type=6, color="#1d428a")
          )
        )
      ) # End TabPanel Playoff Demo
    ) # End tabsetPanel
  ) # End tagList
}


#' Game Predictor Module Server Function
#' @param id Internal parameters for {shiny}.
#' @param active_tab A reactive expression containing the value of input$main_nav
#' @param tab_value The specific value string for this module's tab panel
#' @noRd
mod_game_predictor_server <- function(id, active_tab, tab_value){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # --- Reactive Values ---
    rv <- reactiveValues(
      team_list = NULL,
      model_modern = NULL,
      model_historical = NULL,
      model_current_season = NULL,
      model_playoff_demo = NULL,
      avg_stats_historical_alltime = NULL,
      avg_stats_modern_era = NULL,
      avg_stats_current_season = NULL,
      models_loaded = FALSE,
      loading_error = NULL,
      result_modern = NULL, status_message_modern = "Select Game Predictor tab to load models...",
      result_hist = NULL, status_message_hist = "Select Game Predictor tab to load models...",
      result_current_season = NULL, status_message_current_season = "Select Game Predictor tab to load models...",
      result_playoff = NULL, status_message_playoff = "Select Game Predictor tab to load models..."
    )
    
    # --- Helper function to calculate team average stats for a given date range ---
    calculate_average_team_stats <- function(games_df, defined_teams, start_date = NULL, end_date = NULL, suffix = "") {
      filtered_games_df <- games_df
      if (!is.null(start_date)) filtered_games_df <- filtered_games_df %>% filter(gameDate_parsed >= as.Date(start_date))
      if (!is.null(end_date))   filtered_games_df <- filtered_games_df %>% filter(gameDate_parsed <= as.Date(end_date))
      
      if (nrow(filtered_games_df) == 0) {
        warning(glue::glue("No games found for stats calculation with suffix '{suffix}' between {start_date %||% 'any'} and {end_date %||% 'any'}."))
        return(
          tibble(team = defined_teams) %>%
            mutate(
              !!paste0("home_avg_points", suffix) := 0,
              !!paste0("away_avg_points", suffix) := 0,
              !!paste0("home_win_rate", suffix) := 0.5,
              !!paste0("away_win_rate", suffix) := 0.5
            )
        )
      }
      
      home_stats <- filtered_games_df %>%
        group_by(team = hometeam_rebranded) %>%
        summarise(
          !!paste0("home_avg_points", suffix) := mean(homeScore_num, na.rm = TRUE),
          !!paste0("home_win_rate", suffix) := mean(winner_calc == 1, na.rm = TRUE),
          .groups = "drop"
        )
      
      away_stats <- filtered_games_df %>%
        group_by(team = awayteam_rebranded) %>%
        summarise(
          !!paste0("away_avg_points", suffix) := mean(awayScore_num, na.rm = TRUE),
          !!paste0("away_win_rate", suffix) := mean(winner_calc == 0, na.rm = TRUE),
          .groups = "drop"
        )
      
      overall_home_avg_pts  = mean(home_stats[[paste0("home_avg_points", suffix)]], na.rm = TRUE) %||% 0
      overall_away_avg_pts  = mean(away_stats[[paste0("away_avg_points", suffix)]], na.rm = TRUE) %||% 0
      overall_home_win_rate = mean(home_stats[[paste0("home_win_rate", suffix)]], na.rm = TRUE) %||% 0.5
      overall_away_win_rate = mean(away_stats[[paste0("away_win_rate", suffix)]], na.rm = TRUE) %||% 0.5
      
      all_teams_df <- tibble(team = defined_teams)
      avg_stats_combined <- all_teams_df %>%
        left_join(home_stats, by = "team") %>%
        left_join(away_stats, by = "team") %>%
        mutate(
          !!rlang::sym(paste0("home_avg_points", suffix)) := tidyr::replace_na(!!rlang::sym(paste0("home_avg_points", suffix)), overall_home_avg_pts),
          !!rlang::sym(paste0("away_avg_points", suffix)) := tidyr::replace_na(!!rlang::sym(paste0("away_avg_points", suffix)), overall_away_avg_pts),
          !!rlang::sym(paste0("home_win_rate", suffix))   := tidyr::replace_na(!!rlang::sym(paste0("home_win_rate", suffix)), overall_home_win_rate),
          !!rlang::sym(paste0("away_win_rate", suffix))   := tidyr::replace_na(!!rlang::sym(paste0("away_win_rate", suffix)), overall_away_win_rate)
        )
      return(avg_stats_combined)
    }
    
    observeEvent(active_tab(), {
      req(active_tab() == tab_value, !rv$models_loaded)
      print(paste("Game Predictor Module: Tab activated (", tab_value, "). Starting initial load...", sep=""))
      rv$status_message_modern <- rv$status_message_hist <- rv$status_message_current_season <- rv$status_message_playoff <- "Loading models and data..."
      load_success <- TRUE
      rv$loading_error <- NULL
      
      print("Game Predictor Module: Defining team list...")
      defined_team_list <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards")
      rv$team_list <- defined_team_list
      print(paste("Game Predictor Module: Team list defined with", length(rv$team_list), "teams."))
      
      print("Game Predictor Module: Loading models...")
      model_dir <- "ml_results"
      models_to_load <- list(
        modern = "game_predictor_2025_wf.rds",
        historical = "game_predictor_hist_wf.rds",
        current_season = "game_predictor_current_season_wf.rds",
        playoff_demo = "playoff_predictor_fit_demo.rds"
      )
      for(model_key in names(models_to_load)){
        model_name_in_rv <- paste0("model_", model_key)
        tryCatch({
          model_path <- file.path(model_dir, models_to_load[[model_key]])
          if (!file.exists(model_path)) stop(glue::glue("Model file '{models_to_load[[model_key]]}' not found."))
          rv[[model_name_in_rv]] <- readRDS(model_path)
          print(glue::glue("Game Predictor Module: {model_key} model loaded successfully."))
        }, error = function(e) {
          load_success <<- FALSE
          rv$loading_error <<- paste(rv$loading_error %||% "", glue::glue("Error loading {model_key} model: {e$message}"))
          print(paste(glue::glue("Game Predictor Module: Error loading {model_key} model: {e$message}")))
        })
        if(!load_success) break
      }
      
      if (load_success) {
        print("Game Predictor Module: Loading and preparing base Games.csv data...")
        tryCatch({
          games_data_path <- "data/Games.csv"
          if (!file.exists(games_data_path)) stop("Games.csv not found in data/ directory.")
          if (!exists("rebranded_teams", envir = .GlobalEnv)) stop("The 'rebranded_teams' map is not defined in global.R.")
          global_rebranded_map <- get("rebranded_teams", envir = .GlobalEnv)
          
          nba_games_data_for_stats <- readr::read_csv(games_data_path, show_col_types = FALSE) %>%
            mutate(
              gameDate_parsed = as.Date(ymd_hms(gameDate)),
              hometeam_rebranded = recode(str_trim(paste(hometeamCity, hometeamName)), !!!global_rebranded_map, .default = str_trim(paste(hometeamCity, hometeamName))),
              awayteam_rebranded = recode(str_trim(paste(awayteamCity, awayteamName)), !!!global_rebranded_map, .default = str_trim(paste(awayteamCity, awayteamName))),
              homeScore_num = suppressWarnings(as.numeric(homeScore)),
              awayScore_num = suppressWarnings(as.numeric(awayScore))
            ) %>%
            filter(!is.na(gameDate_parsed), !is.na(homeScore_num), !is.na(awayScore_num)) %>%
            mutate(winner_calc = case_when(homeScore_num > awayScore_num ~ 1L, awayScore_num > homeScore_num ~ 0L, TRUE ~ NA_integer_)) %>%
            filter(!is.na(winner_calc), hometeam_rebranded %in% rv$team_list, awayteam_rebranded %in% rv$team_list)
          
          if (nrow(nba_games_data_for_stats) == 0) stop("No valid game data after initial processing for stats calculation.")
          
          rv$avg_stats_historical_alltime <- calculate_average_team_stats(nba_games_data_for_stats, rv$team_list, suffix = "_hist_all")
          print("Game Predictor Module: avg_stats_historical_alltime calculated.")
          
          MODERN_ERA_START_DATE <- as.Date("2009-10-01")
          MODERN_ERA_END_DATE   <- as.Date("2025-07-01")
          rv$avg_stats_modern_era <- calculate_average_team_stats(nba_games_data_for_stats, rv$team_list, MODERN_ERA_START_DATE, MODERN_ERA_END_DATE, suffix = "_modern")
          print("Game Predictor Module: avg_stats_modern_era calculated.")
          
          CURRENT_SEASON_START_DATE_PRED <- as.Date("2024-10-01")
          CURRENT_SEASON_END_DATE_PRED   <- as.Date("2025-06-30")
          rv$avg_stats_current_season <- calculate_average_team_stats(nba_games_data_for_stats, rv$team_list, CURRENT_SEASON_START_DATE_PRED, CURRENT_SEASON_END_DATE_PRED, suffix = "_current")
          print("Game Predictor Module: avg_stats_current_season calculated.")
          
        }, error = function(e) {
          load_success <<- FALSE
          rv$loading_error <<- paste(rv$loading_error %||% "", "Error processing Games.csv for average stats:", e$message)
          print(paste("Game Predictor Module: Error processing Games.csv for average stats:", e$message))
        })
      }
      
      rv$models_loaded <- TRUE
      all_models_loaded_check <- !is.null(rv$model_modern) && !is.null(rv$model_historical) && !is.null(rv$model_playoff_demo) && !is.null(rv$model_current_season)
      all_stats_loaded_check <- !is.null(rv$avg_stats_historical_alltime) && !is.null(rv$avg_stats_modern_era) && !is.null(rv$avg_stats_current_season)
      
      if (load_success && all_models_loaded_check && all_stats_loaded_check && !is.null(rv$team_list)) {
        rv$status_message_modern <- rv$status_message_hist <- rv$status_message_current_season <- rv$status_message_playoff <- "Select teams and click Predict."
        common_options <- list(placeholder = 'Type or select...', onInitialize = I('function() { this.setValue(""); }'))
        lapply(c("home_team_modern", "away_team_modern", "home_team_hist", "away_team_hist", "home_team_current", "away_team_current", "team1_playoff", "team2_playoff"), function(id_val) {
          updateSelectizeInput(session, id_val, choices = rv$team_list, selected = "", options = common_options, server = TRUE)
        })
        print("Game Predictor Module: Initial loading complete and successful. Dropdowns updated.")
      } else {
        error_details <- rv$loading_error %||% "Unknown loading error or missing data components during initialization."
        error_msg_html <- tags$div(class = "validation-error-message", tags$strong("Error initializing module:"), tags$p(error_details), tags$p("Predictions unavailable."))
        rv$status_message_modern <- rv$status_message_hist <- rv$status_message_current_season <- rv$status_message_playoff <- error_msg_html
        error_choices <- c("Error Loading Teams" = "")
        lapply(c("home_team_modern", "away_team_modern", "home_team_hist", "away_team_hist", "home_team_current", "away_team_current", "team1_playoff", "team2_playoff"), function(id_val) {
          updateSelectizeInput(session, id_val, choices = error_choices, selected = "")
        })
        print(paste("Game Predictor Module: Initial loading FAILED. Error:", error_details))
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    run_prediction <- function(home_team, away_team, model, avg_stats_df, stats_suffix, model_name_log, result_var_name, status_var_name) {
      rv[[status_var_name]] <- NULL
      rv[[result_var_name]] <- NULL
      pred_res_prob <- NULL; error_occurred <- FALSE
      print(glue::glue("{model_name_log} Predict: Home={home_team}, Away={away_team}"))
      
      tryCatch({
        print(glue::glue("{model_name_log} Predict: Preparing input data..."))
        home_team_stats <- avg_stats_df %>% filter(team == home_team)
        away_team_stats <- avg_stats_df %>% filter(team == away_team)
        
        if(nrow(home_team_stats) == 0 || nrow(away_team_stats) == 0) {
          stop(glue::glue("Could not find {tolower(model_name_log)} average stats for one or both selected teams."))
        }
        
        val_home_win_rate   = home_team_stats[[paste0("home_win_rate", stats_suffix)]]
        val_away_win_rate   = away_team_stats[[paste0("away_win_rate", stats_suffix)]]
        val_home_avg_pts    = home_team_stats[[paste0("home_avg_points", stats_suffix)]]
        val_away_avg_pts    = away_team_stats[[paste0("away_avg_points", stats_suffix)]]
        val_home_last5_rate = home_team_stats[[paste0("home_win_rate", stats_suffix)]] 
        val_away_last5_rate = away_team_stats[[paste0("away_win_rate", stats_suffix)]]
        
        new_data <- tibble::tibble(
          hometeam = home_team, 
          awayteam = away_team,
          season = lubridate::year(Sys.Date()) + ifelse(lubridate::month(Sys.Date()) >= 10, 1, 0),
          month = lubridate::month(Sys.Date(), label = TRUE),
          day_of_week = lubridate::wday(Sys.Date(), label = TRUE)
        )
        
        if (model_name_log == "Current Season") {
          new_data[["home_win_rate_current"]]      <- val_home_win_rate
          new_data[["away_win_rate_current"]]      <- val_away_win_rate
          new_data[["home_avg_points_current"]]    <- val_home_avg_pts
          new_data[["away_avg_points_current"]]    <- val_away_avg_pts
          new_data[["home_last5_win_rate_current"]]<- val_home_last5_rate
          new_data[["away_last5_win_rate_current"]]<- val_away_last5_rate
        } else { 
          new_data[["home_win_rate"]]        <- val_home_win_rate
          new_data[["away_win_rate"]]        <- val_away_win_rate
          new_data[["home_avg_points"]]      <- val_home_avg_pts
          new_data[["away_avg_points"]]      <- val_away_avg_pts
          new_data[["home_last5_win_rate"]]  <- val_home_last5_rate
          new_data[["away_last5_win_rate"]]  <- val_away_last5_rate
        }
        
        print(glue::glue("{model_name_log} Predict: Input data prepared with columns: {paste(names(new_data), collapse=', ')}"))
        
        print(glue::glue("{model_name_log} Predict: Making prediction..."))
        pred_prob_df <- predict(model, new_data = new_data, type = "prob")
        pred_res_prob <- pred_prob_df$.pred_1 
        print(glue::glue("{model_name_log} Predict: Prediction completed."))
        
      }, error = function(e) {
        error_occurred <<- TRUE
        rv[[status_var_name]] <<- tags$div(class = "validation-error-message", paste("Error during", model_name_log, "prediction:", e$message))
        print(paste(glue::glue("{model_name_log} Predict: ERROR - "), e$message))
      })
      
      if (!error_occurred && !is.null(pred_res_prob)) {
        # For plausible score
        actual_pred_home_score <- home_team_stats[[paste0("home_avg_points", stats_suffix)]]
        actual_pred_away_score <- away_team_stats[[paste0("away_avg_points", stats_suffix)]]
        
        rv[[result_var_name]] <- list(
          win_prob = pred_res_prob,
          home_score_pred = round(actual_pred_home_score %||% 0),
          away_score_pred = round(actual_pred_away_score %||% 0),
          home_team_name_for_display = home_team,
          away_team_name_for_display = away_team
        )
        print(paste(glue::glue("{model_name_log} Predict: Stored result: WinProb="), 
                    scales::percent(rv[[result_var_name]]$win_prob, accuracy = 0.1),
                    glue::glue(", Score={rv[[result_var_name]]$home_team_name_for_display} {rv[[result_var_name]]$home_score_pred} - {rv[[result_var_name]]$away_score_pred} {rv[[result_var_name]]$away_team_name_for_display}")))
      } else {
        rv[[result_var_name]] <- NULL
      }
    }
    
    observeEvent(input$predict_modern, {
      req(input$home_team_modern, input$away_team_modern, rv$models_loaded, !is.null(rv$model_modern), !is.null(rv$avg_stats_modern_era))
      if (input$home_team_modern == "" || input$away_team_modern == "") { rv$status_message_modern <- tags$div(class="validation-error-message", "Select teams."); rv$result_modern <- NULL; return() }
      if (input$home_team_modern == input$away_team_modern) { rv$status_message_modern <- tags$div(class="validation-error-message", "Select different teams."); rv$result_modern <- NULL; return() }
      run_prediction(input$home_team_modern, input$away_team_modern, rv$model_modern, rv$avg_stats_modern_era, "_modern", "Modern Era", "result_modern", "status_message_modern")
    })
    
    observeEvent(input$predict_hist, {
      req(input$home_team_hist, input$away_team_hist, rv$models_loaded, !is.null(rv$model_historical), !is.null(rv$avg_stats_historical_alltime))
      if (input$home_team_hist == "" || input$away_team_hist == "") { rv$status_message_hist <- tags$div(class="validation-error-message", "Select teams."); rv$result_hist <- NULL; return() }
      if (input$home_team_hist == input$away_team_hist) { rv$status_message_hist <- tags$div(class="validation-error-message", "Select different teams."); rv$result_hist <- NULL; return() }
      run_prediction(input$home_team_hist, input$away_team_hist, rv$model_historical, rv$avg_stats_historical_alltime, "_hist_all", "Historical", "result_hist", "status_message_hist")
    })
    
    observeEvent(input$predict_current, {
      req(input$home_team_current, input$away_team_current, rv$models_loaded, !is.null(rv$model_current_season), !is.null(rv$avg_stats_current_season))
      if (input$home_team_current == "" || input$away_team_current == "") { rv$status_message_current_season <- tags$div(class="validation-error-message", "Select teams."); rv$result_current_season <- NULL; return() }
      if (input$home_team_current == input$away_team_current) { rv$status_message_current_season <- tags$div(class="validation-error-message", "Select different teams."); rv$result_current_season <- NULL; return() }
      run_prediction(input$home_team_current, input$away_team_current, rv$model_current_season, rv$avg_stats_current_season, "_current", "Current Season", "result_current_season", "status_message_current_season")
    })
    
    observeEvent(input$predict_playoff, {
      req(input$team1_playoff, input$team2_playoff, rv$models_loaded, !is.null(rv$model_playoff_demo), !is.null(rv$avg_stats_historical_alltime))
      team1 <- input$team1_playoff; team2 <- input$team2_playoff
      if (team1 == "" || team2 == "") { rv$status_message_playoff <- tags$div(class="validation-error-message", "Select teams."); rv$result_playoff <- NULL; return() }
      if (team1 == team2) { rv$status_message_playoff <- tags$div(class="validation-error-message", "Select different teams."); rv$result_playoff <- NULL; return() }
      
      rv$status_message_playoff <- NULL; rv$result_playoff <- NULL
      pred_playoff_res_class <- NULL; error_occurred <- FALSE
      print(glue::glue("Playoff Demo Predict button clicked: Team1={team1}, Team2={team2}"))
      tryCatch({
        print("Playoff Demo Predict: Preparing input data...")
        stats_df_for_playoff <- rv$avg_stats_historical_alltime 
        suffix_for_playoff <- "_hist_all"
        
        team1_stats <- stats_df_for_playoff %>% filter(team == team1)
        team2_stats <- stats_df_for_playoff %>% filter(team == team2)
        if(nrow(team1_stats) == 0 || nrow(team2_stats) == 0) { stop("Could not find historical average stats for one or both selected teams for playoff demo.") }
        
        team1_overall_win_rate <- (team1_stats[[paste0("home_win_rate", suffix_for_playoff)]] + team1_stats[[paste0("away_win_rate", suffix_for_playoff)]]) / 2
        team2_overall_win_rate <- (team2_stats[[paste0("home_win_rate", suffix_for_playoff)]] + team2_stats[[paste0("away_win_rate", suffix_for_playoff)]]) / 2
        team1_avg_pts_for <- (team1_stats[[paste0("home_avg_points", suffix_for_playoff)]] + team1_stats[[paste0("away_avg_points", suffix_for_playoff)]]) / 2
        team1_avg_pts_against <- (team1_stats[[paste0("away_avg_points", suffix_for_playoff)]] + team1_stats[[paste0("home_avg_points", suffix_for_playoff)]]) / 2
        team2_avg_pts_for <- (team2_stats[[paste0("home_avg_points", suffix_for_playoff)]] + team2_stats[[paste0("away_avg_points", suffix_for_playoff)]]) / 2
        team2_avg_pts_against <- (team2_stats[[paste0("away_avg_points", suffix_for_playoff)]] + team2_stats[[paste0("home_avg_points", suffix_for_playoff)]]) / 2
        team1_avg_pt_diff <- team1_avg_pts_for - team1_avg_pts_against
        team2_avg_pt_diff <- team2_avg_pts_for - team2_avg_pts_against
        
        new_data_playoff <- tibble::tibble(
          win_rate_diff = team1_overall_win_rate - team2_overall_win_rate,
          avg_pt_diff_diff = team1_avg_pt_diff - team2_avg_pt_diff,
          avg_pts_for_diff = team1_avg_pts_for - team2_avg_pts_for,
          avg_pts_against_diff = team1_avg_pts_against - team2_avg_pts_against
        )
        print("Playoff Demo Predict: Input data prepared.")
        print("Playoff Demo Predict: Making prediction...")
        pred_playoff <- predict(rv$model_playoff_demo, new_data = new_data_playoff, type = "class")
        pred_playoff_res_class <- pred_playoff$.pred_class
        print("Playoff Demo Predict: Prediction completed.")
      }, error = function(e) {
        error_occurred <<- TRUE
        rv$status_message_playoff <<- tags$div(class = "validation-error-message", paste("Error during Playoff Demo prediction:", e$message))
        print(paste("Playoff Demo Predict: ERROR - ", e$message))
      })
      if (!error_occurred && !is.null(pred_playoff_res_class)) {
        winner_label <- ifelse(pred_playoff_res_class == "1", team1, team2)
        rv$result_playoff <- paste(winner_label, "to win the series (Demo)")
        print(paste("Playoff Demo Predict: Stored result:", rv$result_playoff))
      } else {
        rv$result_playoff <- NULL
      }
    })
    
    render_prediction_output <- function(result_val, status_msg_val, model_name_display, loading_error_val, specific_model_rv, specific_stats_rv) {
      if (!rv$models_loaded && is.null(loading_error_val)) {
        return(tags$p(status_msg_val %||% "Loading model...", style="text-align: center; color: #aaaaaa; margin-top: 20px;"))
      }
      if (!is.null(loading_error_val) && is.null(result_val)) {
        if(is.null(specific_model_rv) || is.null(specific_stats_rv)){
          return(tags$div(class = "validation-error-message",
                          glue::glue("{model_name_display} model or its specific stats failed to load. Predictions unavailable for this tab.")))
        }
      }
      if (!is.null(status_msg_val)) {
        is_html_error_message <- inherits(status_msg_val, "shiny.tag") || inherits(status_msg_val, "shiny.tag.list")
        if (is_html_error_message) {
          error_text_check <- tryCatch(as.character(status_msg_val), error = function(e) "")
          if (grepl(paste("Error during", model_name_display), error_text_check, ignore.case = TRUE)) {
            return(status_msg_val)
          }
        } else if (is.character(status_msg_val) && grepl(paste("Error during", model_name_display), status_msg_val, ignore.case = TRUE)) {
          return(tags$div(class = "validation-error-message", status_msg_val))
        }
      }
      if (is.null(result_val)) {
        default_msg <- "Select teams and click Predict."
        if (!is.null(specific_model_rv) && !is.null(specific_stats_rv)) {
          return(tags$p(default_msg, style="text-align: center; color: #aaaaaa; margin-top: 20px;"))
        } else if (is.null(loading_error_val)) {
          return(tags$p(status_msg_val %||% "Loading...", style="text-align: center; color: #aaaaaa; margin-top: 20px;"))
        }
        return(tags$p(default_msg, style="text-align: center; color: #aaaaaa; margin-top: 20px;"))
      }
      
      if (!is.list(result_val) || 
          !all(c("win_prob", "home_score_pred", "away_score_pred", "home_team_name_for_display", "away_team_name_for_display") %in% names(result_val))) {
        warning(glue::glue("Incomplete prediction data for {model_name_display} model. Result_val: {paste(capture.output(str(result_val)), collapse='\n')}"))
        return(tags$p("Prediction data is incomplete or in an unexpected format.", style="text-align: center; color: #ffcc00;"))
      }
      
      win_probability_display <- result_val$win_prob
      home_score_display <- result_val$home_score_pred
      away_score_display <- result_val$away_score_pred
      home_team_display_name <- result_val$home_team_name_for_display
      away_team_display_name <- result_val$away_team_name_for_display
      
      tags$div(style="text-align: center; padding: 15px;",
               tags$p(style="font-size: 1.1em; color: #cccccc;", 
                      glue::glue("Predicted Home Win Probability ({model_name_display} Model):")),
               tags$p(style="font-size: 1.8em; font-weight: bold; color: #ffffff; margin-bottom: 15px;", 
                      scales::percent(win_probability_display, accuracy = 0.1)),
               tags$hr(style="border-top: 1px solid #444444; margin: 15px auto; width: 70%;"),
               tags$p(style="font-size: 1.1em; color: #cccccc; margin-top: 15px;", 
                      "Plausible Score (based on period averages):"),
               tags$p(style="font-size: 1.6em; font-weight: bold; color: #ffffff;", 
                      glue::glue("{home_team_display_name} {home_score_display}  –  {away_score_display} {away_team_display_name}"))
      )
    }
    
    output$output_modern <- renderUI({ render_prediction_output(rv$result_modern, rv$status_message_modern, "Modern Era", rv$loading_error, rv$model_modern, rv$avg_stats_modern_era) })
    output$output_hist <- renderUI({ render_prediction_output(rv$result_hist, rv$status_message_hist, "Historical", rv$loading_error, rv$model_historical, rv$avg_stats_historical_alltime) })
    output$output_current_season <- renderUI({ render_prediction_output(rv$result_current_season, rv$status_message_current_season, "Current Season", rv$loading_error, rv$model_current_season, rv$avg_stats_current_season) })
    
    output$output_playoff <- renderUI({
      if (!rv$models_loaded && is.null(rv$loading_error)) { return(tags$p(rv$status_message_playoff %||% "Loading model...", style="text-align: center; color: #aaaaaa; margin-top: 20px;")) }
      if (!is.null(rv$loading_error) && is.null(rv$result_playoff)) {
        if(is.null(rv$model_playoff_demo)){ 
          return(tags$div(class = "validation-error-message", "Playoff Demo model failed to load. Predictions unavailable for this tab."))
        }
      }
      if (!is.null(rv$status_message_playoff)) {
        is_html_error_message_po <- inherits(rv$status_message_playoff, "shiny.tag") || inherits(rv$status_message_playoff, "shiny.tag.list")
        if (is_html_error_message_po) {
          error_text_check_po <- tryCatch(as.character(rv$status_message_playoff), error = function(e) "")
          if (grepl("Error during Playoff Demo prediction", error_text_check_po, ignore.case = TRUE)) {
            return(rv$status_message_playoff)
          }
        } else if (is.character(rv$status_message_playoff) && grepl("Error during Playoff Demo prediction", rv$status_message_playoff, ignore.case = TRUE)) {
          return(tags$div(class = "validation-error-message", rv$status_message_playoff))
        }
      }
      if (is.null(rv$result_playoff)) {
        default_msg <- "Select teams and click Predict Series."
        if (!is.null(rv$model_playoff_demo)) {
          return(tags$p(default_msg, style="text-align: center; color: #aaaaaa; margin-top: 20px;"))
        } else if (is.null(rv$loading_error)) {
          return(tags$p(rv$status_message_playoff %||% "Loading...", style="text-align: center; color: #aaaaaa; margin-top: 20px;"))
        }
        return(tags$p(default_msg, style="text-align: center; color: #aaaaaa; margin-top: 20px;"))
      }
      outcome_string <- rv$result_playoff
      tags$div(style="text-align: center; padding: 15px;",
               tags$p(style="font-size: 1.1em; color: #cccccc;", "Predicted Series Outcome (Demo):"),
               tags$p(style="font-size: 1.5em; font-weight: bold; color: #ffffff;", outcome_string)
      )
    })
    
  }) # End moduleServer
}
# --- END FILE: modules/mod_game_predictor.R (FINAL - Incorporates 4 Models, Refined Stats, Plausible Scores, Fixes) ---