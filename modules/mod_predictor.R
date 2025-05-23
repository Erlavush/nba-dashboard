
mod_predictor_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML(glue::glue(
        "
        #{ns('')} .value-box .value-box-area > .value {{ font-size: 1.6rem !important; font-weight: 600; }}
        #{ns('')} .value-box .value-box-area > .card-title {{ font-size: 0.85rem !important; font-weight: 500; }}
        #{ns('')} .value-box .value-box-showcase {{ font-size: 2.2rem !important; }}
        #{ns('predictor_content_wrapper')} {{ padding: 20px; }}
        #{ns('team_select_panel_pred')} {{
          display: flex; justify-content: space-between; align-items: center;
          margin-bottom: 25px; padding: 15px; background-color: #1A1527;
          border-radius: 6px; border: 1px solid #2D2640;
        }}
        #{ns('team_select_panel_pred')} > div {{ flex: 1; }}
        #{ns('vs_text_pred')} {{
          flex: 0 0 auto; padding: 0 25px; font-size: 1.8em;
          font-weight: bold; color: #00AEEF;
        }}
        #{ns('centered_controls_pred')} {{
          display: flex; flex-direction: column; align-items: center;
          gap: 20px; margin-bottom: 30px;
        }}
        #{ns('probability_plot_container_pred')} {{
          border: 1px solid #2D2640; border-radius: 8px; padding: 10px 0px 0px 0px; /* Reduced padding for plot */
          background-color: #100E17; margin-bottom: 20px; /* Reduced margin */
          box-shadow: 0 2px 4px rgba(0,0,0,0.1); min-height: 120px; /* Ensure space */
        }}
        #{ns('predicted_winner_text_container_pred')} {{
          text-align: center; margin-top: 5px; margin-bottom: 25px; /* Adjusted margins */
        }}
        #{ns('stat_cards_section_title_pred')} {{
          text-align:center; margin-bottom:10px; font-weight:600;
          color: #ffffff; font-size: 1.5rem;
        }}
        #{ns('sfe_description_pred')} {{
          font-size: 0.85em; color: #aaaaaa; text-align: center;
          margin-top: -15px; margin-bottom: 20px; max-width: 700px; margin-left: auto; margin-right: auto;
        }}
        #{ns('footer_text_pred')} {{
          text-align:center; margin-top:40px; padding-bottom:20px;
          color: #aaaaaa; font-style: italic; font-size: 0.9em;
        }}
        #{ns('')} .value-box {{
          background-color: #1A1527 !important; border: 1px solid #2D2640 !important;
        }}
        #{ns('')} .value-box .value-box-area > .value,
        #{ns('')} .value-box .value-box-area > .card-title {{
           color: #e0e0e0 !important;
        }}
        #{ns('')} .value-box .value-box-showcase .bs-icon {{
           color: #00AEEF !important;
        }}
        "
      )))
    ),
    
    div(id = ns('predictor_content_wrapper'),
        titlePanel(
          tags$div(
            style = "text-align: center; margin-bottom: 30px; color: #00AEEF;",
            icon("calculator", style="font-size: 1.8em; margin-right:15px; vertical-align: middle;"),
            tags$span("NBA Game Predictor", style="font-size: 2.0em; font-weight: 700; vertical-align: middle;")
          )
        ),
        
        div(class = "team-select-panel-pred", id = ns("team_select_panel_pred"),
            div(selectInput(ns("home_team_id_pred"), "Home Team:", width="100%", choices = NULL)),
            div(class="vs-text-pred", id=ns("vs_text_pred"), "VS"),
            div(selectInput(ns("away_team_id_pred"), "Away Team:", width="100%", choices = NULL))
        ),
        
        div(class="centered-controls-pred", id=ns("centered_controls_pred"),
            dateInput(ns("game_date_shiny_pred"), "Hypothetical Game Date:", width="300px",
                      value = Sys.Date() + 1, min = "1980-01-01"), 
            actionButton(ns("predict_button_pred"), "Predict Game Outcome", icon = icon("calculator"), class = "btn-primary btn-lg", style="width:300px;")
        ),
        
        div(class="probability-plot-container-pred", id=ns("probability_plot_container_pred"),
            shinycssloaders::withSpinner(
              plotOutput(ns("probability_bar_plot_pred"), height = "100px"), 
              type=6, color = "#00AEEF"
            )
        ),
        
        div(id = ns("predicted_winner_text_container_pred"),
            uiOutput(ns("predicted_winner_text_ui_pred"))
        ),
        tags$hr(style="margin-top: 25px; margin-bottom: 25px; border-color: #3A3052;"),
        
        h4(icon("table-list"), "Key Team Statistics (Leading into this game)",
           id=ns("stat_cards_section_title_pred")),
        p(id=ns("sfe_description_pred"), 
          strong("SFE (Season-to-Date Features):"), "Averages from games played by the team in the current hypothetical season, *before* the selected game date. If it's a team's first game of that season, SFE stats will reflect no prior in-season data (may show as 0 or N/A).",
          br(),strong("Roll-10:"), "Averages from the team's last 10 games played, regardless of season."
        ),
        fluidRow(
          column(6,
                 h5(textOutput(ns("home_team_card_title_pred")), style="text-align:center; color: #00AEEF; font-weight:bold; margin-bottom:15px;"),
                 shinycssloaders::withSpinner(uiOutput(ns("home_stat_cards_pred")), type=6, color="#00AEEF")
          ),
          column(6,
                 h5(textOutput(ns("away_team_card_title_pred")), style="text-align:center; color: #F4008A; font-weight:bold; margin-bottom:15px;"),
                 shinycssloaders::withSpinner(uiOutput(ns("away_stat_cards_pred")), type=6, color="#F4008A")
          )
        ),
        tags$footer(class="footer-text-pred",  id=ns("footer_text_pred"),
                    tags$small("Predictor based on historical data up to 2022-2023. Feature calculation uses data *before* selected date."))
    )
  )
}

# --- Server Function for Predictor Module ---
mod_predictor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv_predictor_data <- reactiveValues(
      model = NULL,
      expected_predictors = NULL,
      teams_for_dropdown = NULL,
      historical_games_raw = NULL,
      data_loaded = FALSE,
      error_message = NULL
    )
    
    observe({
      if (!rv_predictor_data$data_loaded) {
        tryCatch({
          print("MOD_PREDICTOR: Loading RDS files...")
          rv_predictor_data$model <- readRDS("data/logistic_model.rds")
          rv_predictor_data$expected_predictors <- readRDS("data/predictor_names.rds")
          rv_predictor_data$teams_for_dropdown <- readRDS("data/all_teams_list.rds")
          rv_predictor_data$historical_games_raw <- readRDS("data/games_imputed.rds")
          
          if (!is.null(rv_predictor_data$teams_for_dropdown)) {
            team_choices <- setNames(rv_predictor_data$teams_for_dropdown$team_id, 
                                     rv_predictor_data$teams_for_dropdown$team_name)
            if(length(team_choices) >= 2) {
              selected_indices <- sample(length(team_choices), 2, replace = FALSE)
              updateSelectInput(session, "home_team_id_pred", choices = team_choices, selected = team_choices[selected_indices[1]])
              updateSelectInput(session, "away_team_id_pred", choices = team_choices, selected = team_choices[selected_indices[2]])
            } else if (length(team_choices) == 1) {
              updateSelectInput(session, "home_team_id_pred", choices = team_choices, selected = team_choices[1])
              updateSelectInput(session, "away_team_id_pred", choices = team_choices, selected = team_choices[1])
            }
          }
          rv_predictor_data$data_loaded <- TRUE
          print("MOD_PREDICTOR: RDS files loaded successfully.")
        }, error = function(e) {
          rv_predictor_data$error_message <- paste("Error loading predictor RDS files. Ensure all .rds files are in the 'data/' directory. Original error:", e$message)
          showNotification(rv_predictor_data$error_message, type = "error", duration = NULL)
          print(paste("MOD_PREDICTOR ERROR:", rv_predictor_data$error_message))
        })
      }
    })
    
    N_GAMES_ROLLING_PRED <- 10
    BASE_STAT_COLS_MEAN_PRED <- c("pts", "pts_allowed", "oreb", "dreb", "reb", "ast", "stl", "blk", "tov", "pf", "plus_minus", "win_flag")
    BASE_SFE_FEATURE_NAMES_NO_PREFIX_PRED <- c(paste0("sfe_avg_", BASE_STAT_COLS_MEAN_PRED), "sfe_fg_pct", "sfe_fg3_pct", "sfe_ft_pct", "game_num_in_season")
    BASE_ROLL_FEATURE_NAMES_NO_PREFIX_PRED <- c(paste0("roll_avg_", N_GAMES_ROLLING_PRED,"_", BASE_STAT_COLS_MEAN_PRED), paste0("roll_fg_pct_",N_GAMES_ROLLING_PRED), paste0("roll_fg3_pct_",N_GAMES_ROLLING_PRED), paste0("roll_ft_pct_",N_GAMES_ROLLING_PRED))
    ALL_TEAM_FEATURE_NAMES_NO_PREFIX_PRED <- unique(c(BASE_SFE_FEATURE_NAMES_NO_PREFIX_PRED, BASE_ROLL_FEATURE_NAMES_NO_PREFIX_PRED))
    FEATURES_FOR_CARDS_PRED <- c("sfe_avg_pts", "sfe_avg_pts_allowed", "sfe_avg_win_flag", "roll_avg_10_pts", "roll_avg_10_win_flag")
    FEATURES_FOR_CARDS_DISPLAY_NAMES_PRED <- c("SFE Avg Points", "SFE Avg Pts Allowed", "SFE Win %", "Roll-10 Avg Points", "Roll-10 Win %")
    names(FEATURES_FOR_CARDS_DISPLAY_NAMES_PRED) <- FEATURES_FOR_CARDS_PRED
    
    calculate_team_features_pred <- function(team_id_selected, current_game_date, all_historical_games, N_roll) {
      output_features <- as_tibble(matrix(NA_real_, nrow = 1, ncol = length(ALL_TEAM_FEATURE_NAMES_NO_PREFIX_PRED)))
      names(output_features) <- ALL_TEAM_FEATURE_NAMES_NO_PREFIX_PRED
      
      team_games_home <- all_historical_games %>%
        filter(team_id_home == team_id_selected, game_date < current_game_date) %>%
        select(game_id, game_date, season_id, team_id = team_id_home, pts = pts_home, pts_allowed = pts_away, fgm = fgm_home, fga = fga_home, fg3m = fg3m_home, fg3a = fg3a_home, ftm = ftm_home, fta = fta_home, oreb = oreb_home, dreb = dreb_home, reb = reb_home, ast = ast_home, stl = stl_home, blk = blk_home, tov = tov_home, pf = pf_home, plus_minus = plus_minus_home, wl = wl_home)
      
      team_games_away <- all_historical_games %>%
        filter(team_id_away == team_id_selected, game_date < current_game_date) %>%
        select(game_id, game_date, season_id, team_id = team_id_away, pts = pts_away, pts_allowed = pts_home, fgm = fgm_away, fga = fga_away, fg3m = fg3m_away, fg3a = fg3a_away, ftm = ftm_away, fta = fta_away, oreb = oreb_away, dreb = dreb_away, reb = reb_away, ast = ast_away, stl = stl_away, blk = blk_away, tov = tov_away, pf = pf_away, plus_minus = plus_minus_away, wl = wl_away)
      
      team_games_history <- bind_rows(team_games_home, team_games_away) %>% mutate(win_flag = ifelse(wl == "W", 1, 0)) %>% select(-wl) %>% arrange(game_date, game_id)
      
      if (nrow(team_games_history) == 0) {
        if ("game_num_in_season" %in% names(output_features)) output_features$game_num_in_season <- 0
        return(output_features)
      }
      
      stat_cols_sum <- c("fgm", "fga", "fg3m", "fg3a", "ftm", "fta")
      temp_rolling_df <- team_games_history
      
      if (nrow(temp_rolling_df) >= 1) {
        for (stat in stat_cols_sum) temp_rolling_df[[paste0("roll_sum_", N_roll, "_", stat)]] <- zoo::rollapplyr(temp_rolling_df[[stat]], width = N_roll, FUN = sum, align = "right", partial = TRUE, fill = NA_real_)
        for (stat in BASE_STAT_COLS_MEAN_PRED) temp_rolling_df[[paste0("roll_avg_", N_roll, "_", stat)]] <- zoo::rollapplyr(temp_rolling_df[[stat]], width = N_roll, FUN = mean, align = "right", partial = TRUE, fill = NA_real_)
        
        latest_rolling <- temp_rolling_df %>% slice_tail(n = 1)
        
        get_roll_sum <- function(df, col_suffix) {
          col_name <- paste0("roll_sum_", N_roll, "_", col_suffix)
          if (col_name %in% names(df) && !is.na(df[[col_name]][1])) return(df[[col_name]][1])
          return(NA_real_)
        }
        
        roll_fga_sum <- get_roll_sum(latest_rolling, "fga")
        roll_fg3a_sum <- get_roll_sum(latest_rolling, "fg3a")
        roll_fta_sum <- get_roll_sum(latest_rolling, "fta")
        
        output_features[[paste0("roll_fg_pct_", N_roll)]] <- ifelse(is.na(roll_fga_sum) || roll_fga_sum == 0, NA_real_, get_roll_sum(latest_rolling, "fgm") / roll_fga_sum)
        output_features[[paste0("roll_fg3_pct_", N_roll)]] <- ifelse(is.na(roll_fg3a_sum) || roll_fg3a_sum == 0, NA_real_, get_roll_sum(latest_rolling, "fg3m") / roll_fg3a_sum)
        output_features[[paste0("roll_ft_pct_", N_roll)]] <- ifelse(is.na(roll_fta_sum) || roll_fta_sum == 0, NA_real_, get_roll_sum(latest_rolling, "ftm") / roll_fta_sum)
        
        for (stat in BASE_STAT_COLS_MEAN_PRED) {
          roll_avg_col_name <- paste0("roll_avg_", N_roll, "_", stat)
          if (roll_avg_col_name %in% names(latest_rolling)) output_features[[roll_avg_col_name]] <- latest_rolling[[roll_avg_col_name]][1]
        }
      }
      
      last_game_played_by_team <- team_games_history %>% slice_tail(n = 1)
      season_id_for_sfe_calc <- NA_character_
      
      if (nrow(last_game_played_by_team) > 0) {
        hypothetical_game_year <- year(current_game_date)
        hypothetical_game_month <- month(current_game_date)
        hypothetical_season_start_year <- if (hypothetical_game_month >= 8) hypothetical_game_year else hypothetical_game_year - 1
        hypothetical_season_id_numeric <- as.numeric(paste0("2", hypothetical_season_start_year))
        
        last_game_season_id_regular <- last_game_played_by_team$season_id
        last_game_season_id_numeric_comp <- if(startsWith(as.character(last_game_season_id_regular), "4")) {
          as.numeric(paste0("2", substr(as.character(last_game_season_id_regular), 2, 5)))
        } else {
          suppressWarnings(as.numeric(last_game_season_id_regular))
        }
        
        if(!is.na(last_game_season_id_numeric_comp) && last_game_season_id_numeric_comp == hypothetical_season_id_numeric) {
          season_id_for_sfe_calc <- last_game_played_by_team$season_id
        } else {
          season_id_for_sfe_calc <- NA_character_
        }
      }
      
      if (!is.na(season_id_for_sfe_calc) && nzchar(season_id_for_sfe_calc)) {
        team_season_history_filtered <- team_games_history %>% filter(season_id == season_id_for_sfe_calc)
        if (nrow(team_season_history_filtered) > 0) {
          sfe_stats_df <- team_season_history_filtered %>% mutate(game_num_in_season_calc = row_number())
          for (stat in stat_cols_sum) sfe_stats_df[[paste0("sfe_sum_", stat)]] <- cumsum(sfe_stats_df[[stat]])
          for (stat in BASE_STAT_COLS_MEAN_PRED) sfe_stats_df[[paste0("sfe_avg_", stat)]] <- cumsum(sfe_stats_df[[stat]]) / sfe_stats_df$game_num_in_season_calc
          latest_sfe <- sfe_stats_df %>% slice_tail(n = 1)
          
          output_features[["sfe_fg_pct"]] <- ifelse(latest_sfe$sfe_sum_fga == 0, NA_real_, latest_sfe$sfe_sum_fgm / latest_sfe$sfe_sum_fga)
          output_features[["sfe_fg3_pct"]] <- ifelse(latest_sfe$sfe_sum_fg3a == 0, NA_real_, latest_sfe$sfe_sum_fg3m / latest_sfe$sfe_sum_fg3a)
          output_features[["sfe_ft_pct"]] <- ifelse(latest_sfe$sfe_sum_fta == 0, NA_real_, latest_sfe$sfe_sum_ftm / latest_sfe$sfe_sum_fta)
          for (stat in BASE_STAT_COLS_MEAN_PRED) output_features[[paste0("sfe_avg_", stat)]] <- latest_sfe[[paste0("sfe_avg_", stat)]][1]
          output_features[["game_num_in_season"]] <- latest_sfe$game_num_in_season_calc[1]
        } else {
          if ("game_num_in_season" %in% names(output_features)) output_features$game_num_in_season <- 0
        }
      } else {
        if ("game_num_in_season" %in% names(output_features)) output_features$game_num_in_season <- 0
      }
      return(output_features)
    }
    
    rv_pred_output <- reactiveValues(
      prob_home_win = NULL, home_team_name = NULL, away_team_name = NULL,
      home_features_display = NULL, away_features_display = NULL, prediction_ready = FALSE
    )
    
    observeEvent(input$predict_button_pred, {
      rv_pred_output$prediction_ready <- FALSE
      req(rv_predictor_data$data_loaded, 
          !is.null(rv_predictor_data$model),
          !is.null(rv_predictor_data$expected_predictors),
          !is.null(rv_predictor_data$historical_games_raw),
          !is.null(rv_predictor_data$teams_for_dropdown)
      )
      req(input$home_team_id_pred, input$away_team_id_pred, input$game_date_shiny_pred)
      
      if (input$home_team_id_pred == input$away_team_id_pred) {
        showModal(modalDialog(title = "Input Error", "Home and Away teams must be different.", easyClose = TRUE))
        return()
      }
      
      home_id <- as.numeric(input$home_team_id_pred)
      away_id <- as.numeric(input$away_team_id_pred)
      predict_date <- as.Date(input$game_date_shiny_pred)
      
      rv_pred_output$home_team_name <- rv_predictor_data$teams_for_dropdown$team_name[rv_predictor_data$teams_for_dropdown$team_id == home_id][1]
      rv_pred_output$away_team_name <- rv_predictor_data$teams_for_dropdown$team_name[rv_predictor_data$teams_for_dropdown$team_id == away_id][1]
      
      shiny::withProgress(message = 'Predicting game outcome...', value = 0, {
        shiny::incProgress(0.1, detail = "Calculating home team stats...")
        home_features_raw <- calculate_team_features_pred(home_id, predict_date, rv_predictor_data$historical_games_raw, N_GAMES_ROLLING_PRED)
        
        shiny::incProgress(0.3, detail = "Calculating away team stats...")
        away_features_raw <- calculate_team_features_pred(away_id, predict_date, rv_predictor_data$historical_games_raw, N_GAMES_ROLLING_PRED)
        
        shiny::incProgress(0.5, detail = "Imputing features...")
        impute_team_features_shiny_pred <- function(df_features) {
          df_out <- df_features
          if ("game_num_in_season" %in% names(df_out)) {
            df_out[["game_num_in_season"]] <- ifelse(is.na(df_out[["game_num_in_season"]][1]) | df_out[["game_num_in_season"]][1] == 0, 1, df_out[["game_num_in_season"]][1])
          } else {
            df_out[["game_num_in_season"]] <- 1
          }
          sfe_avg_cols <- names(df_out)[grepl("^sfe_avg_", names(df_out))]
          for (col in sfe_avg_cols) {
            default_val <- if(grepl("_win_flag$", col)) 0.5 else 0
            df_out[[col]] <- ifelse(is.na(df_out[[col]][1]), default_val, df_out[[col]][1])
          }
          for (col in names(df_out)[grepl("^sfe_f.*_pct", names(df_out))]) {
            df_out[[col]] <- ifelse(is.na(df_out[[col]][1]), 0, df_out[[col]][1])
          }
          roll_avg_cols <- names(df_out)[grepl("^roll_avg_", names(df_out))]
          for (col in roll_avg_cols) {
            default_val <- if(grepl("_win_flag$", col)) 0.5 else 0
            df_out[[col]] <- ifelse(is.na(df_out[[col]][1]), default_val, df_out[[col]][1])
          }
          for (col in names(df_out)[grepl("^roll_f.*_pct_", names(df_out))]) {
            df_out[[col]] <- ifelse(is.na(df_out[[col]][1]), 0, df_out[[col]][1])
          }
          return(as_tibble(lapply(df_out, function(x) if(is.list(x) && length(x)==1) x[[1]] else if(is.list(x)) NA else x)))
        }
        home_features_imputed <- impute_team_features_shiny_pred(home_features_raw)
        away_features_imputed <- impute_team_features_shiny_pred(away_features_raw)
        
        rv_pred_output$home_features_display <- home_features_imputed
        rv_pred_output$away_features_display <- away_features_imputed
        
        shiny::incProgress(0.7, detail = "Creating difference features...")
        diff_features_list <- list()
        for (base_feature_name in ALL_TEAM_FEATURE_NAMES_NO_PREFIX_PRED) {
          model_diff_feature_name <- paste0("diff_", base_feature_name)
          if (model_diff_feature_name %in% rv_predictor_data$expected_predictors) {
            val_home <- if(base_feature_name %in% names(home_features_imputed) && length(home_features_imputed[[base_feature_name]]) > 0 && !is.na(home_features_imputed[[base_feature_name]][1])) home_features_imputed[[base_feature_name]][1] else 0
            val_away <- if(base_feature_name %in% names(away_features_imputed) && length(away_features_imputed[[base_feature_name]]) > 0 && !is.na(away_features_imputed[[base_feature_name]][1])) away_features_imputed[[base_feature_name]][1] else 0
            diff_val <- val_home - val_away
            diff_features_list[[model_diff_feature_name]] <- ifelse(is.na(diff_val), 0, diff_val)
          }
        }
        for(exp_pred in rv_predictor_data$expected_predictors){ if(!exp_pred %in% names(diff_features_list)) diff_features_list[[exp_pred]] <- 0 }
        new_game_data_for_model <- as.data.frame(diff_features_list)[, rv_predictor_data$expected_predictors, drop = FALSE]
        
        shiny::incProgress(0.9, detail = "Making prediction...")
        prob_home_win_val <- predict(rv_predictor_data$model, newdata = new_game_data_for_model, type = "response")
        rv_pred_output$prob_home_win <- prob_home_win_val[1]
        rv_pred_output$prediction_ready <- TRUE
      })
    })
    
    
    # --- Plot Output
    output$probability_bar_plot_pred <- renderPlot({
      req(rv_pred_output$prediction_ready, !is.null(rv_pred_output$prob_home_win),
          !is.null(rv_pred_output$home_team_name), !is.null(rv_pred_output$away_team_name))
      
      home_name <- rv_pred_output$home_team_name
      away_name <- rv_pred_output$away_team_name
      prob_h <- rv_pred_output$prob_home_win
      prob_a <- 1 - prob_h
      
      VAPOR_CYAN_HOME_BAR <- "#00AEEF"  # Home team bar color 
      VAPOR_PINK_AWAY_BAR <- "#F4008A"  # Away team bar color 
      
      VAPOR_CYAN_HOME_TEXT <- "#00AEEF" # Home team text color
      VAPOR_PINK_AWAY_TEXT <- "#F4008A" # Away team text color
      
      VAPOR_WHITE_TEXT_ON_BAR <- "#FFFFFF" 
      
      rect_data <- data.frame(
        team_type = factor(c("Home", "Away"), levels = c("Home", "Away")),
        xmin = c(0, prob_h),             
        xmax = c(prob_h, 1.0),           
        ymin = 0.4,                     
        ymax = 0.6,                    
        fill_color = c(VAPOR_CYAN_HOME_BAR, VAPOR_PINK_AWAY_BAR),
        stringsAsFactors = FALSE
      )
      
      percentage_label_data <- data.frame(
        x_pos = c(prob_h / 2, prob_h + (prob_a / 2)), 
        y_pos = 0.5, 
        label = c(paste0(round(prob_h * 100), "%"), paste0(round(prob_a * 100), "%")),
        stringsAsFactors = FALSE
      )
      
      home_name_display <- if(nchar(home_name) > 18) paste0(substr(home_name, 1, 16), "..") else home_name
      away_name_display <- if(nchar(away_name) > 18) paste0(substr(away_name, 1, 16), "..") else away_name
      
      p <- ggplot() +
        geom_rect(data = rect_data, 
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = team_type),
                  show.legend = FALSE) +
        scale_fill_manual(values = setNames(rect_data$fill_color, rect_data$team_type)) +
        
        geom_text(data = percentage_label_data, 
                  aes(x = x_pos, y = y_pos, label = label),
                  color = VAPOR_WHITE_TEXT_ON_BAR, size = 7, fontface = "bold", family = "Fira Code") +
        
        annotate("text", x = -0.03, y = 0.5, label = home_name_display, hjust = 1, vjust = 0.5,
                 size = 4.5, fontface = "bold", color = VAPOR_CYAN_HOME_TEXT, family = "Fira Code") +
        
        annotate("text", x = 1.03, y = 0.5, label = away_name_display, hjust = 0, vjust = 0.5,
                 size = 4.5, fontface = "bold", color = VAPOR_PINK_AWAY_TEXT, family = "Fira Code") +
        
        scale_x_continuous(limits = c(-0.25, 1.25), expand = c(0,0)) + 
        scale_y_continuous(limits = c(0.3, 0.7), expand = c(0,0)) +    
        coord_cartesian(clip = "off") + 
        theme_void() +
        theme(
          legend.position = "none",
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          text = element_text(family = "Fira Code", color = VAPOR_WHITE_TEXT_ON_BAR) 
        )

      print(p)
      
    }, bg="transparent", height=100) 
    
    

    output$predicted_winner_text_ui_pred <- renderUI({
      req(rv_pred_output$prediction_ready, !is.null(rv_pred_output$prob_home_win), rv_pred_output$home_team_name, rv_pred_output$away_team_name)
      prob_h <- rv_pred_output$prob_home_win
      home_name <- rv_pred_output$home_team_name
      away_name <- rv_pred_output$away_team_name
      
      if (any(sapply(list(home_name, away_name, prob_h), function(x) is.null(x) || length(x) != 1 || is.na(x)))) {
        return(tags$p("Awaiting valid prediction...", style="color: #aaaaaa;"))
      }
      
      winner_name <- if (prob_h > 0.5) home_name else away_name
      winner_prob <- if (prob_h > 0.5) prob_h else 1 - prob_h
      
      VAPOR_CYAN <- "#00AEEF"
      VAPOR_PINK <- "#F4008A"
      VAPOR_TEXT_LIGHT <- "#e0e0e0"
      VAPOR_TEXT_DIM <- "#c0c0c0"
      
      winner_text_color <- if (prob_h > 0.5) VAPOR_CYAN else VAPOR_PINK
      winner_name_styled <- tags$strong(winner_name, style = paste0("color:", winner_text_color, "; font-size: 1.4em;")) # Slightly increased size
      
      tags$div(
        tags$h4(style=paste0("color: ", VAPOR_TEXT_LIGHT, ";"), "Predicted Winner: ", winner_name_styled),
        tags$p(HTML(glue::glue("{winner_name} has a <strong>{round(winner_prob*100,1)}%</strong> chance of winning.")), 
               style=paste0("font-size:1.1em; color: ", VAPOR_TEXT_DIM, ";"))
      )
    })
    
    create_team_stat_cards_ui_pred <- function(team_features_display_reactive_val) {
      VAPOR_CYAN_ICON <- "#00AEEF" 
      VAPOR_TEXT_ON_CARD <- "#e0e0e0"
      
      if (is.null(team_features_display_reactive_val) || nrow(team_features_display_reactive_val) == 0) {
        return(tagList(lapply(1:length(FEATURES_FOR_CARDS_PRED), function(i) {
          bslib::value_box(
            title = FEATURES_FOR_CARDS_DISPLAY_NAMES_PRED[i], value = "N/A",
            showcase = bsicons::bs_icon("hourglass-split", color = VAPOR_TEXT_ON_CARD), 
            theme_color = "secondary", 
            height = "120px", class = "mb-2",
            style = "color: #e0e0e0 !important;"
          )
        })))
      }
      df_display <- team_features_display_reactive_val
      card_list <- lapply(FEATURES_FOR_CARDS_PRED, function(feature_name) {
        display_name <- FEATURES_FOR_CARDS_DISPLAY_NAMES_PRED[feature_name]
        value_str <- "N/A"; raw_value <- NA_real_
        if (feature_name %in% names(df_display) && length(df_display[[feature_name]]) > 0 && !is.na(df_display[[feature_name]][1])) {
          raw_value <- df_display[[feature_name]][1]
          value_str <- if (grepl("pct|win_flag", feature_name)) paste0(round(raw_value * 100, 1), "%") else round(raw_value, 1)
        }
        icon_name <- case_when(
          grepl("pts", feature_name) & !grepl("allowed", feature_name) ~ "graph-up-arrow",
          grepl("pts_allowed", feature_name) ~ "graph-down-arrow",
          grepl("win_flag", feature_name) ~ "trophy-fill",
          TRUE ~ "bar-chart-line-fill"
        )
        
        box_theme_color <- if (feature_name == "sfe_avg_pts_allowed") "danger" else "primary"
        icon_display_color <- VAPOR_CYAN_ICON 
        
        bslib::value_box(
          title = display_name, value = value_str,
          showcase = bsicons::bs_icon(icon_name, color = icon_display_color),
          theme_color = box_theme_color, 
          height = "120px", class = "mb-2",
          style = "color: #e0e0e0 !important;" 
        )
      })
      tagList(card_list)
    }
    
    output$home_team_card_title_pred <- renderText({ req(rv_pred_output$home_team_name); paste(rv_pred_output$home_team_name, "— Avg. Stats Entering Game") })
    output$away_team_card_title_pred <- renderText({ req(rv_pred_output$away_team_name); paste(rv_pred_output$away_team_name, "— Avg. Stats Entering Game") })
    output$home_stat_cards_pred <- renderUI({ create_team_stat_cards_ui_pred(rv_pred_output$home_features_display) })
    output$away_stat_cards_pred <- renderUI({ create_team_stat_cards_ui_pred(rv_pred_output$away_features_display) })
    
  }) # end moduleServer
}
