# --- START FILE: modules/mod_historical_player_stats.R (Complete and Updated with Dual Mode & year_to_season Fix) ---
# Module for Historical Player Statistics Comparison (Per Season and Career Averages)

# Needed libraries (Ensure these are loaded in global.R):
# shiny, dplyr, purrr, hoopR, scales, tidyr, shinycssloaders, glue, rlang, readr

#' Historical Player Stats Module UI Function
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_historical_player_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Player Statistics Comparison"),
    uiOutput(ns("module_description_text"), inline = FALSE),
    
    fluidRow(
      column(12,
             radioButtons(ns("comparison_mode_select"),
                          label = tags$strong("Select Comparison Type:"),
                          choices = c("Per Specific Historical Season" = "per_season",
                                      "All-Time Career Averages" = "career_average"),
                          selected = "per_season",
                          inline = TRUE)
      )
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("comparison_mode_select"), "'] == 'per_season'"),
      fluidRow(
        column(12,
               selectInput(ns("historical_season_select"),
                           label = tags$strong("Select Historical Season:"),
                           choices = c("Loading seasons..." = ""),
                           width = "100%")
        )
      )
    ),
    
    fluidRow(
      column(6, selectizeInput(ns("player_select_1"), label = "Select Player 1:", choices = NULL, options = list(placeholder = 'Type or select...', onInitialize = I('function() { this.setValue(""); }')))),
      column(6, selectizeInput(ns("player_select_2"), label = "Select Player 2:", choices = NULL, options = list(placeholder = 'Type or select...', onInitialize = I('function() { this.setValue(""); }'))))
    ),
    hr(),
    tabsetPanel(
      id = ns("stats_type_tabs"),
      type = "pills",
      tabPanel(
        title = "Regular Season Stats",
        value = ns("subtab_rs"),
        br(),
        uiOutput(ns("stats_description_text_primary"), inline = FALSE),
        withSpinner(uiOutput(ns("comparison_output_primary")), type=7, color="#cccccc")
      ),
      tabPanel(
        title = "Playoff Stats",
        value = ns("subtab_po"),
        br(),
        uiOutput(ns("stats_description_text_playoffs"), inline = FALSE),
        withSpinner(uiOutput(ns("comparison_output_playoffs")), type=7, color="#cccccc")
      )
    )
  )
}

#' Historical Player Stats Module Server Function
#' @param id Internal parameters for {shiny}.
#' @param active_tab A reactive expression containing the value of input$main_nav
#' @param tab_value The specific value string for this module's tab panel
#' @noRd
mod_historical_player_stats_server <- function(id, active_tab, tab_value){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      # Seasonal Data
      stats_data_seasonal_rs = NULL, stats_data_seasonal_rs_season_string = NULL,
      player_list_seasonal_rs = NULL, error_message_seasonal_rs = NULL, data_loaded_seasonal_rs = FALSE,
      stats_data_seasonal_po = NULL, stats_data_seasonal_po_season_string = NULL,
      player_list_seasonal_po = NULL, error_message_seasonal_po = NULL, data_loaded_seasonal_po = FALSE,
      
      # Career Data
      stats_data_career_rs = NULL, stats_data_career_po = NULL,
      player_list_career = NULL, error_message_career = NULL, data_loaded_career = FALSE,
      
      # Dropdown & Mode State
      available_historical_seasons = NULL, historical_seasons_dropdown_initialized = FALSE,
      current_comparison_mode = "per_season"
    )
    
    headshot_cache_hist <- reactiveValues()
    fallback_image_url_local <- "www_files/nba-logo.png"
    
    # --- Helper: Generate choices for the historical season dropdown ---
    generate_historical_season_choices <- function() {
      current_sys_year <- as.numeric(format(Sys.Date(), "%Y"))
      # MODIFIED: Show seasons up to the one ending in the current calendar year
      latest_historical_end_year <- current_sys_year
      
      earliest_season_end_year <- 1996 # Earliest season ending year (1995-96 season)
      
      if (latest_historical_end_year < earliest_season_end_year) {
        return(stats::setNames(list(""), "No historical seasons available"))
      }
      season_end_years <- seq(from = latest_historical_end_year, to = earliest_season_end_year, by = -1)
      if (length(season_end_years) == 0) {
        return(stats::setNames(list(""), "No historical seasons available"))
      }
      season_choices_list <- lapply(season_end_years, function(year) {
        display_label <- paste0(year - 1, "-", substr(as.character(year), 3, 4))
        stats::setNames(as.character(year), display_label) # Name = Display, Value = Year
      })
      final_choices <- do.call(c, season_choices_list)
      if (is.null(final_choices) || length(final_choices) == 0) {
        return(stats::setNames(list(""), "Error generating season choices"))
      }
      return(final_choices)
    }
    
    # --- Helper: Fetch and Process Player Stats for a SPECIFIC historical season ---
    fetch_historical_player_stats_data <- function(target_season_end_year, season_type_to_fetch) {
      if (is.null(target_season_end_year) || is.na(as.numeric(target_season_end_year))) {
        return(list(data = NULL, player_list_for_type = character(0), error = "Invalid target season year provided.", season_string = NA_character_, no_data_found = FALSE))
      }
      if (!season_type_to_fetch %in% c("Regular Season", "Playoffs")) {
        return(list(data = NULL, player_list_for_type = character(0), error = "Invalid season type provided.", season_string = NA_character_, no_data_found = FALSE))
      }
      
      # +++ FIX: Use (end_year - 1) for hoopR::year_to_season as it expects start year +++
      season_start_year_for_api <- as.numeric(target_season_end_year) - 1
      season_string_for_api <- tryCatch(hoopR::year_to_season(season_start_year_for_api), error = function(e) NA_character_)
      # +++ END FIX +++
      
      if (is.na(season_string_for_api)) {
        return(list(data = NULL, player_list_for_type = character(0), error = paste("Could not convert start year", season_start_year_for_api, "(derived from end year", target_season_end_year, ") to season string."), season_string = NA_character_, no_data_found = FALSE))
      }
      print(paste("MOD_HIST_STATS: Fetching historical", season_type_to_fetch, 
                  "data for API season:", season_string_for_api, 
                  "(derived from end year:", target_season_end_year, ", using start year:", season_start_year_for_api, "for API call)"))
      
      local_error_message <- NULL; base_stats_list <- NULL; advanced_stats_list <- NULL
      
      base_stats_list <- tryCatch(
        hoopR::nba_leaguedashplayerstats(season = season_string_for_api, season_type = season_type_to_fetch, measure_type = "Base", per_mode = "PerGame"),
        error = function(e) {local_error_message <<- paste("API Error (Base):", e$message); NULL}
      )
      if (is.null(local_error_message)) {
        advanced_stats_list <- tryCatch(
          hoopR::nba_leaguedashplayerstats(season = season_string_for_api, season_type = season_type_to_fetch, measure_type = "Advanced", per_mode = "PerGame"),
          error = function(e) {
            err_msg_adv <- paste("API Error (Advanced):", e$message)
            local_error_message <<- paste(local_error_message %||% "", err_msg_adv, sep = if (!is.null(local_error_message)) "; " else "")
            NULL
          }
        )
      }
      
      if (!is.null(local_error_message)) return(list(data = NULL, player_list_for_type = character(0), error = local_error_message, season_string = season_string_for_api, no_data_found = FALSE))
      if (is.null(base_stats_list) || is.null(advanced_stats_list)) return(list(data = NULL, player_list_for_type = character(0), error = "API response was NULL for base or advanced stats.", season_string = season_string_for_api, no_data_found = FALSE))
      
      if ("LeagueDashPlayerStats" %in% names(base_stats_list) && "LeagueDashPlayerStats" %in% names(advanced_stats_list)) {
        base_df <- base_stats_list[["LeagueDashPlayerStats"]]
        adv_df <- advanced_stats_list[["LeagueDashPlayerStats"]]
        
        if (is.null(base_df) || nrow(base_df) == 0) return(list(data = data.frame(), player_list_for_type = character(0), error = NULL, season_string = season_string_for_api, no_data_found = TRUE))
        
        expected_base_cols <- c("PLAYER_ID", "PLAYER_NAME", "TEAM_ABBREVIATION", "AGE", "GP", "MIN", "PTS", "REB", "AST", "STL", "BLK", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT", "TOV", "PF")
        expected_adv_cols <- c("PLAYER_ID", "USG_PCT", "TS_PCT")
        
        if (!all(expected_base_cols %in% names(base_df))) {
          missing_cols <- setdiff(expected_base_cols, names(base_df))
          return(list(data = NULL, player_list_for_type = character(0), error = paste("Missing base columns:", paste(missing_cols, collapse=", ")), season_string = season_string_for_api, no_data_found = FALSE))
        }
        
        base_numeric_cols <- c("AGE", "GP", "MIN", "PTS", "REB", "AST", "STL", "BLK", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT", "TOV", "PF")
        adv_numeric_cols <- c("USG_PCT", "TS_PCT")
        
        base_clean <- base_df %>% 
          dplyr::mutate(across(all_of(base_numeric_cols), as.numeric)) %>% 
          dplyr::select(playerId = PLAYER_ID, namePlayer = PLAYER_NAME, team = TEAM_ABBREVIATION, age = AGE, gp = GP, min = MIN, pts = PTS, reb = REB, ast = AST, stl = STL, blk = BLK, fgm = FGM, fga = FGA, pctFG = FG_PCT, fg3m = FG3M, fg3a = FG3A, pctFG3 = FG3_PCT, ftm = FTM, fta = FTA, pctFT = FT_PCT, tov = TOV, pf = PF)
        
        adv_clean <- if (!is.null(adv_df) && nrow(adv_df) > 0 && "PLAYER_ID" %in% names(adv_df) && all(expected_adv_cols %in% names(adv_df))) {
          adv_df %>% 
            dplyr::mutate(across(all_of(adv_numeric_cols), as.numeric)) %>% 
            dplyr::select(playerId = PLAYER_ID, usg_pct = USG_PCT, ts_pct = TS_PCT)
        } else {
          print(glue::glue("Advanced stats for {season_string_for_api} {season_type_to_fetch} empty/missing. Creating NA columns for USG/TS."))
          id_col_data <- if(nrow(base_clean) > 0) base_clean$playerId else character(0)
          tibble::tibble(
            playerId = id_col_data, 
            usg_pct = rep(NA_real_, length(id_col_data)), 
            ts_pct = rep(NA_real_, length(id_col_data))
          ) %>% 
            dplyr::filter(dplyr::n() > 0 || length(playerId) > 0) 
        }
        
        if (nrow(adv_clean) == 0 && !"playerId" %in% names(adv_clean) && nrow(base_clean) > 0) {
          adv_clean <- tibble::tibble(playerId = character(0), usg_pct=numeric(0), ts_pct=numeric(0))
        }
        
        merged_data <- tryCatch(
          dplyr::full_join(base_clean, adv_clean, by = "playerId") %>% dplyr::arrange(namePlayer), 
          error = function(e) {print(paste("MOD_HIST_STATS: Merge error:", e$message)); NULL}
        )
        
        if(!is.null(merged_data) && nrow(merged_data) > 0) {
          return(list(data = merged_data, player_list_for_type = sort(unique(merged_data$namePlayer)), error = NULL, season_string = season_string_for_api, no_data_found = FALSE))
        } else if (is.null(merged_data)) {
          return(list(data = NULL, player_list_for_type = character(0), error = "Data processing/merge failed.", season_string = season_string_for_api, no_data_found = FALSE))
        } else {
          return(list(data = data.frame(), player_list_for_type = character(0), error = NULL, season_string = season_string_for_api, no_data_found = TRUE))
        }
      } else {
        return(list(data = NULL, player_list_for_type = character(0), error = "Unexpected API data format (missing LeagueDashPlayerStats).", season_string = season_string_for_api, no_data_found = FALSE))
      }
    }
    
    # --- Helper: Process PlayerStatistics.csv for Career Averages ---
    process_career_stats_from_csv <- function(csv_filepath = "data/PlayerStatistics.csv") {
      print(paste("MOD_HIST_STATS: Attempting to process career stats from", csv_filepath))
      if (!file.exists(csv_filepath)) {
        return(list(career_rs_stats = NULL, career_po_stats = NULL, all_players_list = character(0), error = glue::glue("Career stats CSV not found at: {csv_filepath}")))
      }
      
      raw_data <- tryCatch({
        readr::read_csv(csv_filepath, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
      }, error = function(e) {
        return(list(career_rs_stats = NULL, career_po_stats = NULL, all_players_list = character(0), error = glue::glue("Error reading CSV: {e$message}")))
      })
      
      stat_cols_to_sum <- c("numMinutes", "points", "assists", "blocks", "steals",
                            "fieldGoalsAttempted", "fieldGoalsMade",
                            "threePointersAttempted", "threePointersMade",
                            "freeThrowsAttempted", "freeThrowsMade",
                            "reboundsTotal", "foulsPersonal", "turnovers")
      
      required_cols_for_processing <- c("personId", "firstName", "lastName", "gameType", stat_cols_to_sum)
      missing_csv_cols <- setdiff(required_cols_for_processing, names(raw_data))
      if (length(missing_csv_cols) > 0) {
        return(list(career_rs_stats = NULL, career_po_stats = NULL, all_players_list = character(0), error = glue::glue("CSV missing essential columns: {paste(missing_csv_cols, collapse=', ')}")))
      }
      
      data_processed <- raw_data %>%
        dplyr::mutate(
          namePlayer = stringr::str_trim(paste(firstName, lastName)),
          dplyr::across(all_of(stat_cols_to_sum), ~suppressWarnings(as.numeric(as.character(.x)))),
          GP = 1 
        ) %>%
        dplyr::filter(!is.na(personId), !is.na(namePlayer), nchar(namePlayer) > 0,
                      gameType %in% c("Regular Season", "Playoffs"))
      
      if (nrow(data_processed) == 0) {
        return(list(career_rs_stats = NULL, career_po_stats = NULL, all_players_list = character(0), error = "No valid player game records found in CSV after initial processing."))
      }
      
      aggregate_stats_for_career_type <- function(df_filtered_by_gametype) {
        if (nrow(df_filtered_by_gametype) == 0) {
          return(tibble::tibble(
            playerId = character(), namePlayer = character(), team = character(), age = integer(),
            gp = integer(), min = numeric(), pts = numeric(), reb = numeric(), ast = numeric(),
            stl = numeric(), blk = numeric(), tov = numeric(), pf = numeric(),
            pctFG = numeric(), pctFG3 = numeric(), pctFT = numeric(),
            ts_pct = numeric(), usg_pct = numeric() 
          ))
        }
        
        df_filtered_by_gametype %>%
          dplyr::group_by(personId, namePlayer) %>%
          dplyr::summarise(
            career_GP = sum(GP, na.rm = TRUE), career_MIN = sum(numMinutes, na.rm = TRUE),
            career_PTS = sum(points, na.rm = TRUE), career_REB = sum(reboundsTotal, na.rm = TRUE),
            career_AST = sum(assists, na.rm = TRUE), career_STL = sum(steals, na.rm = TRUE),
            career_BLK = sum(blocks, na.rm = TRUE), career_TOV = sum(turnovers, na.rm = TRUE),
            career_PF = sum(foulsPersonal, na.rm = TRUE), career_FGM = sum(fieldGoalsMade, na.rm = TRUE),
            career_FGA = sum(fieldGoalsAttempted, na.rm = TRUE), career_FG3M = sum(threePointersMade, na.rm = TRUE),
            career_FG3A = sum(threePointersAttempted, na.rm = TRUE), career_FTM = sum(freeThrowsMade, na.rm = TRUE),
            career_FTA = sum(freeThrowsAttempted, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            min = ifelse(career_GP > 0, career_MIN / career_GP, 0),
            pts = ifelse(career_GP > 0, career_PTS / career_GP, 0),
            reb = ifelse(career_GP > 0, career_REB / career_GP, 0),
            ast = ifelse(career_GP > 0, career_AST / career_GP, 0),
            stl = ifelse(career_GP > 0, career_STL / career_GP, 0),
            blk = ifelse(career_GP > 0, career_BLK / career_GP, 0),
            tov = ifelse(career_GP > 0, career_TOV / career_GP, 0),
            pf = ifelse(career_GP > 0, career_PF / career_GP, 0),
            pctFG = ifelse(career_FGA > 0, career_FGM / career_FGA, 0),
            pctFG3 = ifelse(career_FG3A > 0, career_FG3M / career_FG3A, 0),
            pctFT = ifelse(career_FTA > 0, career_FTM / career_FTA, 0),
            ts_pct = ifelse((career_FGA + 0.44 * career_FTA) > 0, career_PTS / (2 * (career_FGA + 0.44 * career_FTA)), 0),
            usg_pct = NA_real_, team = "MULT", age = NA_integer_, gp = career_GP
          ) %>%
          dplyr::select(
            playerId = personId, namePlayer, team, age, gp, min, pts, reb, ast, stl, blk, tov, pf,
            pctFG, pctFG3, pctFT, ts_pct, usg_pct
          )
      }
      career_rs_stats <- aggregate_stats_for_career_type(data_processed %>% dplyr::filter(gameType == "Regular Season"))
      career_po_stats <- aggregate_stats_for_career_type(data_processed %>% dplyr::filter(gameType == "Playoffs"))
      all_career_players <- sort(unique(c(career_rs_stats$namePlayer, career_po_stats$namePlayer)))
      print(paste("MOD_HIST_STATS: Career stats processing complete. RS players:", nrow(career_rs_stats), "PO players:", nrow(career_po_stats), "Total unique players:", length(all_career_players)))
      return(list(career_rs_stats = career_rs_stats, career_po_stats = career_po_stats, all_players_list = all_career_players, error = NULL))
    }
    
    # --- Initialize Historical Season Dropdown on Tab Activation ---
    observeEvent(active_tab(), {
      req(active_tab() == tab_value, !rv$historical_seasons_dropdown_initialized)
      print("MOD_HIST_STATS: Tab activated. Initializing historical season dropdown...")
      choices <- generate_historical_season_choices()
      if (length(choices) > 0 && !(length(choices) == 1 && names(choices)[1] == "")) {
        updateSelectInput(session, "historical_season_select", choices = choices, selected = choices[[1]])
        rv$available_historical_seasons <- choices
        rv$historical_seasons_dropdown_initialized <- TRUE
        print("MOD_HIST_STATS: Historical season dropdown populated.")
      } else {
        updateSelectInput(session, "historical_season_select", choices = c("Error loading seasons" = ""), selected = "")
        print("MOD_HIST_STATS: Failed to populate historical season dropdown.")
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # --- Reactive: Combined trigger for data fetching ---
    data_fetch_trigger <- reactive({
      current_mode <- input$comparison_mode_select %||% "per_season"
      rv$current_comparison_mode <- current_mode
      if (current_mode == "per_season") {
        req(rv$historical_seasons_dropdown_initialized, input$historical_season_select, input$historical_season_select != "")
        list(mode = current_mode, season_end_year = input$historical_season_select, stats_sub_tab = input$stats_type_tabs %||% ns("subtab_rs"))
      } else {
        list(mode = current_mode, season_end_year = NULL, stats_sub_tab = input$stats_type_tabs %||% ns("subtab_rs"))
      }
    })
    
    # --- Observer: Main data fetching logic ---
    observeEvent(data_fetch_trigger(), {
      triggered_inputs <- data_fetch_trigger()
      current_mode <- triggered_inputs$mode
      selected_hist_season_end_year <- if (current_mode == "per_season") as.numeric(triggered_inputs$season_end_year) else NULL
      active_stats_sub_tab <- triggered_inputs$stats_sub_tab
      print(paste("MOD_HIST_STATS: Data fetch triggered. Mode:", current_mode, "| Hist. Season (End Yr):", selected_hist_season_end_year %||% "N/A", "| Stats Sub-Tab:", active_stats_sub_tab))
      rv$stats_data_seasonal_rs <- NULL; rv$stats_data_seasonal_rs_season_string <- NULL; rv$player_list_seasonal_rs <- NULL; rv$error_message_seasonal_rs <- NULL; rv$data_loaded_seasonal_rs <- FALSE
      rv$stats_data_seasonal_po <- NULL; rv$stats_data_seasonal_po_season_string <- NULL; rv$player_list_seasonal_po <- NULL; rv$error_message_seasonal_po <- NULL; rv$data_loaded_seasonal_po <- FALSE
      rv$error_message_career <- NULL 
      updateSelectizeInput(session, "player_select_1", choices = NULL, selected = "", server = TRUE, options = list(placeholder = 'Loading players...'))
      updateSelectizeInput(session, "player_select_2", choices = NULL, selected = "", server = TRUE, options = list(placeholder = 'Loading players...'))
      current_player_list_for_dropdown <- character(0)
      current_placeholder_for_dropdown <- "Select players..."
      if (current_mode == "per_season") {
        req(selected_hist_season_end_year)
        season_type_api_call <- if (active_stats_sub_tab == ns("subtab_po")) "Playoffs" else "Regular Season"
        result <- fetch_historical_player_stats_data(target_season_end_year = selected_hist_season_end_year, season_type_to_fetch = season_type_api_call)
        if (season_type_api_call == "Regular Season") {
          rv$stats_data_seasonal_rs <- result$data; rv$stats_data_seasonal_rs_season_string <- result$season_string; rv$player_list_seasonal_rs <- result$player_list_for_type
          rv$data_loaded_seasonal_rs <- is.null(result$error) && !is.null(result$data) && nrow(result$data) > 0
          if (!is.null(result$error)) { rv$error_message_seasonal_rs <- result$error; current_placeholder_for_dropdown <- 'Error loading players...' }
          else if (isTRUE(result$no_data_found) || nrow(result$data %||% data.frame()) == 0) { rv$error_message_seasonal_rs <- glue::glue("No regular season data found for {result$season_string %||% selected_hist_season_end_year}.") }
          else { rv$error_message_seasonal_rs <- NULL; current_player_list_for_dropdown <- rv$player_list_seasonal_rs %||% character(0); current_placeholder_for_dropdown <- 'Type or select Player...' }
        } else {
          rv$stats_data_seasonal_po <- result$data; rv$stats_data_seasonal_po_season_string <- result$season_string; rv$player_list_seasonal_po <- result$player_list_for_type
          rv$data_loaded_seasonal_po <- is.null(result$error) && !is.null(result$data) && nrow(result$data) > 0
          if (!is.null(result$error)) { rv$error_message_seasonal_po <- result$error; current_placeholder_for_dropdown <- 'Error loading playoff players...' }
          else if (isTRUE(result$no_data_found) || nrow(result$data %||% data.frame()) == 0) { rv$error_message_seasonal_po <- glue::glue("No playoff data found for {result$season_string %||% selected_hist_season_end_year}.") }
          else { rv$error_message_seasonal_po <- NULL; current_player_list_for_dropdown <- rv$player_list_seasonal_po %||% character(0); current_placeholder_for_dropdown <- 'Type or select Playoff Player...' }
        }
      } else if (current_mode == "career_average") {
        if (!isTRUE(rv$data_loaded_career)) {
          career_results <- process_career_stats_from_csv()
          if (is.null(career_results$error)) {
            rv$stats_data_career_rs <- career_results$career_rs_stats; rv$stats_data_career_po <- career_results$career_po_stats
            rv$player_list_career <- career_results$all_players_list; rv$data_loaded_career <- TRUE; rv$error_message_career <- NULL
          } else { rv$error_message_career <- career_results$error; rv$data_loaded_career <- TRUE }
        }
        current_player_list_for_dropdown <- rv$player_list_career %||% character(0)
        current_placeholder_for_dropdown <- if (length(current_player_list_for_dropdown) > 0) "Select player for career stats..." else (rv$error_message_career %||% "No career players found...")
      }
      updateSelectizeInput(session, "player_select_1", choices = current_player_list_for_dropdown, selected = "", server = TRUE, options = list(placeholder = current_placeholder_for_dropdown))
      updateSelectizeInput(session, "player_select_2", choices = current_player_list_for_dropdown, selected = "", server = TRUE, options = list(placeholder = current_placeholder_for_dropdown))
      if(length(current_player_list_for_dropdown) > 0) print(paste("MOD_HIST_STATS: Player dropdowns updated for mode:", current_mode, ". Players available:", length(current_player_list_for_dropdown)))
      else print(paste("MOD_HIST_STATS: Player dropdowns updated for mode:", current_mode, ". No players available or error during fetch/process."))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    output$module_description_text <- renderUI({
      mode <- rv$current_comparison_mode
      if (mode == "per_season") { p("Select a historical season and two players. Then, choose Regular Season or Playoff stats to view their comparison for that season.") } 
      else { p("Select two players to compare their All-Time Career Average statistics (Regular Season or Playoffs).") }
    })
    output$stats_description_text_primary <- renderUI({
      mode <- rv$current_comparison_mode; req(input$stats_type_tabs == ns("subtab_rs"))
      if (mode == "per_season") {
        req(input$historical_season_select, input$historical_season_select != "", rv$available_historical_seasons)
        selected_val <- input$historical_season_select; season_display_label <- names(rv$available_historical_seasons)[rv$available_historical_seasons == selected_val]
        season_str <- if (length(season_display_label) > 0) season_display_label[1] else paste0(as.numeric(selected_val)-1, "-", substr(selected_val,3,4))
        api_season_str <- rv$stats_data_seasonal_rs_season_string %||% season_str
        p(glue::glue("Comparing {api_season_str} Regular Season per-game statistics."), style="text-align:center; font-style:italic; color:#aaa; margin-bottom:15px;")
      } else { p("Comparing All-Time Career Regular Season per-game statistics.", style="text-align:center; font-style:italic; color:#aaa; margin-bottom:15px;") }
    })
    output$stats_description_text_playoffs <- renderUI({
      mode <- rv$current_comparison_mode; req(input$stats_type_tabs == ns("subtab_po"))
      if (mode == "per_season") {
        req(input$historical_season_select, input$historical_season_select != "", rv$available_historical_seasons)
        selected_val <- input$historical_season_select; season_display_label <- names(rv$available_historical_seasons)[rv$available_historical_seasons == selected_val]
        season_str <- if (length(season_display_label) > 0) season_display_label[1] else paste0(as.numeric(selected_val)-1, "-", substr(selected_val,3,4))
        api_season_str <- rv$stats_data_seasonal_po_season_string %||% season_str
        p(glue::glue("Comparing {api_season_str} Playoff per-game statistics."), style="text-align:center; font-style:italic; color:#aaa; margin-bottom:15px;")
      } else { p("Comparing All-Time Career Playoff per-game statistics.", style="text-align:center; font-style:italic; color:#aaa; margin-bottom:15px;") }
    })
    
    get_player_data <- function(player_name_selected, is_playoffs) {
      mode <- rv$current_comparison_mode; req(player_name_selected, player_name_selected != "")
      data_source <- if (mode == "per_season") { if (is_playoffs) rv$stats_data_seasonal_po else rv$stats_data_seasonal_rs } 
      else { if (is_playoffs) rv$stats_data_career_po else rv$stats_data_career_rs }
      loaded_flag <- if (mode == "per_season") { if (is_playoffs) isTRUE(rv$data_loaded_seasonal_po) else isTRUE(rv$data_loaded_seasonal_rs) } 
      else { isTRUE(rv$data_loaded_career) }
      req(loaded_flag, !is.null(data_source)); data_source %>% dplyr::filter(namePlayer == player_name_selected)
    }
    player1_primary_data_r <- reactive({ get_player_data(input$player_select_1, is_playoffs = FALSE) })
    player2_primary_data_r <- reactive({ get_player_data(input$player_select_2, is_playoffs = FALSE) })
    player1_playoff_data_r <- reactive({ get_player_data(input$player_select_1, is_playoffs = TRUE) })
    player2_playoff_data_r <- reactive({ get_player_data(input$player_select_2, is_playoffs = TRUE) })
    
    get_selected_player_id_historical <- function(selected_player_name) {
      req(selected_player_name, selected_player_name != ""); mode <- rv$current_comparison_mode
      is_playoff_sub_tab_active <- (input$stats_type_tabs %||% ns("subtab_rs")) == ns("subtab_po")
      data_source_for_id <- if (mode == "per_season") { if (is_playoff_sub_tab_active) rv$stats_data_seasonal_po else rv$stats_data_seasonal_rs } 
      else { if (is_playoff_sub_tab_active) rv$stats_data_career_po else rv$stats_data_career_rs }
      if (is.null(data_source_for_id) || nrow(data_source_for_id) == 0) return(NA_character_)
      player_info <- data_source_for_id %>% dplyr::filter(namePlayer == selected_player_name) %>% dplyr::slice(1)
      if (nrow(player_info) > 0 && "playerId" %in% names(player_info)) player_info$playerId else NA_character_
    }
    get_player_headshot_url_local <- function(player_id) {
      req(player_id, !is.na(player_id)); cache_key <- as.character(player_id)
      if (!is.null(headshot_cache_hist[[cache_key]])) return(headshot_cache_hist[[cache_key]])
      url <- tryCatch(hoopR::nba_playerheadshot(player_id = as.numeric(player_id)), error = function(e) {print(paste("MOD_HIST_STATS: Headshot fetch error ID", player_id, ":", e$message)); NULL})
      final_url <- if (!is.null(url) && grepl("^http", url)) url else fallback_image_url_local
      headshot_cache_hist[[cache_key]] <- final_url; return(final_url)
    }
    player1_headshot_hist <- reactive({ id <- get_selected_player_id_historical(input$player_select_1); if(is.na(id)) return(fallback_image_url_local); get_player_headshot_url_local(id) })
    player2_headshot_hist <- reactive({ id <- get_selected_player_id_historical(input$player_select_2); if(is.na(id)) return(fallback_image_url_local); get_player_headshot_url_local(id) })
    
    create_stat_item <- function(label, value, format_func = NULL) {
      value_clean <- if(is.na(value)) "N/A" else value
      display_value <- if (!is.null(format_func) && value_clean != "N/A" && (is.numeric(value_clean) || inherits(value_clean, "numeric"))) format_func(value_clean) else if (is.numeric(value_clean) || inherits(value_clean, "numeric")) round(value_clean, 1) else value_clean
      tags$div(class="stat-item", tags$span(class="stat-label", strong(paste0(label, ":"))), tags$span(class="stat-value", display_value))
    }
    render_player_card_historical <- function(player_data_r_fn, headshot_r_fn, player_name_input_val, card_type_label, context_string_for_msg_fn) {
      p_dat <- player_data_r_fn(); selected_player_name <- player_name_input_val; current_mode <- rv$current_comparison_mode
      context_string_for_msg <- context_string_for_msg_fn()
      mode_context_message <- if (current_mode == "per_season") { glue::glue("for the {context_string_for_msg %||% 'selected historical'} {tolower(card_type_label)}") } 
      else { glue::glue("for their All-Time Career {tolower(card_type_label)}") }
      if (is.null(selected_player_name) || selected_player_name == "") return(tags$div(class = "player-stats-grid", style = "text-align: center; padding: 20px; color: #aaa; min-height: 300px; display:flex; flex-direction:column; justify-content:center;", p("Select a player.")))
      if (is.null(p_dat) || nrow(p_dat) == 0) return(tags$div(class = "player-stats-grid", style = "text-align: center; padding: 20px; color: #aaa; min-height: 300px; display:flex; flex-direction:column; justify-content:center;", h4(selected_player_name, style="color: #fff; margin-bottom:10px;"), p(glue::glue("{selected_player_name} has no stats {mode_context_message}."))))
      current_headshot <- headshot_r_fn()
      stats_to_display_map <- list("Team"="team", "GP"="gp", "MIN"="min", "PTS"="pts", "REB"="reb", "AST"="ast", "STL"="stl", "BLK"="blk", "TOV"="tov", "FG%"="pctFG", "3P%"="pctFG3", "FT%"="pctFT", "TS%"="ts_pct", "USG%"="usg_pct")
      if (current_mode == "per_season") { stats_to_display_map <- c(list("Age"="age"), stats_to_display_map) }
      missing_display_cols <- setdiff(unlist(stats_to_display_map), names(p_dat))
      if (length(missing_display_cols) > 0) { 
        warning(paste("MOD_HIST_STATS: Card for", selected_player_name, card_type_label, "mode", current_mode, "missing columns:", paste(missing_display_cols, collapse=", ")))
        return(tags$div(class="player-stats-grid", style="color:red; text-align:center; padding:10px;", "Error: Incomplete player data for display.")) 
      }
      stat_items_list <- purrr::map2(names(stats_to_display_map), stats_to_display_map, function(label, col_name) {
        value <- p_dat[[col_name]][1]; format_func <- if (label %in% c("FG%", "3P%", "FT%", "TS%", "USG%")) scales::percent_format(accuracy = 1) else NULL
        create_stat_item(label, value, format_func = format_func)
      })
      tagList(tags$img(src = current_headshot, alt = p_dat$namePlayer %||% selected_player_name, class = "player-img"),
              h4(p_dat$namePlayer %||% selected_player_name, style="text-align: center; color: #ffffff; margin-top: 5px;"),
              tags$div(class="player-stats-grid", stat_items_list))
    }
    primary_context_string_r <- reactive({ mode <- rv$current_comparison_mode; if (mode == "per_season") { rv$stats_data_seasonal_rs_season_string %||% names(rv$available_historical_seasons)[rv$available_historical_seasons == input$historical_season_select[1]] %||% "selected season" } else { "Career" } })
    playoff_context_string_r <- reactive({ mode <- rv$current_comparison_mode; if (mode == "per_season") { rv$stats_data_seasonal_po_season_string %||% names(rv$available_historical_seasons)[rv$available_historical_seasons == input$historical_season_select[1]] %||% "selected season" } else { "Career Playoffs" } })
    
    output$comparison_output_primary <- renderUI({
      mode <- rv$current_comparison_mode; error_msg_to_show <- if (mode == "per_season") rv$error_message_seasonal_rs else rv$error_message_career
      data_is_loaded_flag <- if (mode == "per_season") isTRUE(rv$data_loaded_seasonal_rs) else isTRUE(rv$data_loaded_career)
      is_data_empty_or_null <- if (mode == "per_season") { is.null(rv$stats_data_seasonal_rs) || nrow(rv$stats_data_seasonal_rs) == 0 || length(rv$player_list_seasonal_rs %||% character(0)) == 0 } 
      else { is.null(rv$stats_data_career_rs) || nrow(rv$stats_data_career_rs) == 0 || length(rv$player_list_career %||% character(0)) == 0 }
      context_str <- primary_context_string_r(); empty_data_message <- if (mode == "per_season") glue::glue("No regular season player data found for {context_str}.") else "Career regular season data not yet available or no players found."
      if (!is.null(error_msg_to_show)) return(tags$div(class = "validation-error-message", error_msg_to_show))
      if (data_is_loaded_flag && is_data_empty_or_null && is.null(error_msg_to_show)) { return(tags$p(empty_data_message, style="text-align: center; color: #aaaaaa; margin-top: 20px;")) }
      if (is.null(input$player_select_1) || input$player_select_1 == "" || is.null(input$player_select_2) || input$player_select_2 == "") { if(data_is_loaded_flag) return(tags$p("Please select two players to compare.", style="text-align: center; color: #aaaaaa; margin-top: 20px;")); return() }
      if (input$player_select_1 == input$player_select_2) return(tags$div(class = "validation-error-message", "Please select two different players."))
      fluidRow( column(6, render_player_card_historical(player1_primary_data_r, player1_headshot_hist, input$player_select_1, "Regular Season", primary_context_string_r)),
                column(6, render_player_card_historical(player2_primary_data_r, player2_headshot_hist, input$player_select_2, "Regular Season", primary_context_string_r)))
    })
    output$comparison_output_playoffs <- renderUI({
      mode <- rv$current_comparison_mode; error_msg_to_show <- if (mode == "per_season") rv$error_message_seasonal_po else rv$error_message_career
      data_is_loaded_flag <- if (mode == "per_season") isTRUE(rv$data_loaded_seasonal_po) else isTRUE(rv$data_loaded_career)
      is_data_empty_or_null <- if (mode == "per_season") { is.null(rv$stats_data_seasonal_po) || nrow(rv$stats_data_seasonal_po) == 0 || length(rv$player_list_seasonal_po %||% character(0)) == 0 } 
      else { is.null(rv$stats_data_career_po) || nrow(rv$stats_data_career_po) == 0 || length(rv$player_list_career %||% character(0)) == 0 }
      context_str <- playoff_context_string_r(); empty_data_message <- if (mode == "per_season") glue::glue("No playoff data or no players participated in the {context_str} Playoffs.") else "Career playoff data not yet available or no players found."
      if (!is.null(error_msg_to_show)) return(tags$div(class = "validation-error-message", error_msg_to_show))
      if (data_is_loaded_flag && is_data_empty_or_null && is.null(error_msg_to_show)) { return(tags$p(empty_data_message, style="text-align: center; color: #aaaaaa; margin-top: 20px;")) }
      if (is.null(input$player_select_1) || input$player_select_1 == "" || is.null(input$player_select_2) || input$player_select_2 == "") { if(data_is_loaded_flag) return(tags$p("Please select two players to compare for playoffs.", style="text-align: center; color: #aaaaaa; margin-top: 20px;")); return() }
      if (input$player_select_1 == input$player_select_2) return(tags$div(class = "validation-error-message", "Please select two different players."))
      fluidRow( column(6, render_player_card_historical(player1_playoff_data_r, player1_headshot_hist, input$player_select_1, "Playoffs", playoff_context_string_r)),
                column(6, render_player_card_historical(player2_playoff_data_r, player2_headshot_hist, input$player_select_2, "Playoffs", playoff_context_string_r)))
    })
    
  }) # End moduleServer
}
# --- END FILE: modules/mod_historical_player_stats.R (Complete and Updated with Dual Mode & year_to_season Fix) ---