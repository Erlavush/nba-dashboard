# --- START FILE: modules/mod_player_stats.R ---
# modules/mod_player_stats.R
# VERSION: Fallback logic (Target Year - 2 -> Target Year - 3) fully implemented, robust column handling.

# Needed libraries (Ensure loaded in global.R):
# shiny, dplyr, purrr, hoopR, scales, tidyr, shinycssloaders, glue, rlang

#' Player Stats Module UI Function
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_player_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Player Statistics Comparison"),
    uiOutput(ns("module_description_text"), inline = FALSE), 
    fluidRow(
      column(6, selectizeInput(ns("player_select_1"), label = "Select Player 1:", choices = NULL, options = list(placeholder = 'Type or select...', onInitialize = I('function() { this.setValue(""); }')))),
      column(6, selectizeInput(ns("player_select_2"), label = "Select Player 2:", choices = NULL, options = list(placeholder = 'Type or select...', onInitialize = I('function() { this.setValue(""); }'))))
    ),
    hr(),
    tabsetPanel(
      id = ns("stats_type_tabs"),
      type = "pills",
      tabPanel(
        title = "Season Statistics",
        value = ns("subtab_season"),
        br(),
        uiOutput(ns("season_stats_description_text"), inline = FALSE), 
        withSpinner(uiOutput(ns("season_comparison_output")), type=7, color="#cccccc")
      ),
      tabPanel(
        title = "Playoff Statistics",
        value = ns("subtab_playoff"),
        br(),
        uiOutput(ns("playoff_stats_description_text"), inline = FALSE),
        withSpinner(uiOutput(ns("playoff_comparison_output")), type=7, color="#cccccc")
      )
    )
  )
}

#' Player Stats Module Server Function
#' @param id Internal parameters for {shiny}.
#' @param active_tab A reactive expression containing the value of input$main_nav
#' @param tab_value The specific value string for this module's tab panel
#' @noRd
mod_player_stats_server <- function(id, active_tab, tab_value){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      stats_data = NULL,
      stats_data_season_string = NULL, 
      player_list = NULL,
      error_message = NULL,
      data_loaded = FALSE,
      playoff_stats_data = NULL,
      playoff_stats_data_season_string = NULL,
      playoff_player_list = NULL,
      playoff_error_message = NULL,
      playoff_data_loaded = FALSE
    )
    
    # --- Helper: Determine the TARGET "primary" display season end year ---
    # This will be Sys.Date() calendar year - 2 for the season ENDING in that year.
    # e.g., if current calendar year is 2025, this targets season ending 2023 (2022-23 season).
    get_primary_target_season_end_year <- function() {
      current_sys_year <- as.numeric(format(Sys.Date(), "%Y"))
      return(current_sys_year - 2) 
    }
    
    # --- Helper: Fetch and Process Player Stats for a given season and type ---
    fetch_and_process_player_stats <- function(target_season_end_year, season_type_to_fetch) {
      season_string_for_api <- hoopR::year_to_season(target_season_end_year)
      print(paste("Fetching", season_type_to_fetch, "data for API season:", season_string_for_api))
      local_error_message <- NULL
      base_stats_list <- tryCatch({ hoopR::nba_leaguedashplayerstats(season = season_string_for_api, season_type = season_type_to_fetch, measure_type = "Base", per_mode = "PerGame") }, error = function(e) { local_error_message <<- paste("API Error (Base for", season_string_for_api, season_type_to_fetch, "):", e$message); NULL })
      advanced_stats_list <- tryCatch({ hoopR::nba_leaguedashplayerstats(season = season_string_for_api, season_type = season_type_to_fetch, measure_type = "Advanced", per_mode = "PerGame") }, error = function(e) { err_msg_adv <- paste("API Error (Advanced for", season_string_for_api, season_type_to_fetch, "):", e$message); if (is.null(local_error_message)) local_error_message <<- err_msg_adv else local_error_message <<- paste(local_error_message, ";", err_msg_adv); NULL })
      if (!is.null(local_error_message)) { return(list(data = NULL, player_list_for_type = character(0), error = local_error_message, season_string = season_string_for_api)) }
      if (is.null(base_stats_list) || is.null(advanced_stats_list)) { return(list(data = NULL, player_list_for_type = character(0), error = paste("API response was NULL for", season_string_for_api, season_type_to_fetch), season_string = season_string_for_api)) }
      
      if ("LeagueDashPlayerStats" %in% names(base_stats_list) && "LeagueDashPlayerStats" %in% names(advanced_stats_list)) {
        base_df <- base_stats_list[["LeagueDashPlayerStats"]]; adv_df <- advanced_stats_list[["LeagueDashPlayerStats"]]
        if (is.null(base_df) || nrow(base_df) == 0) { return(list(data = data.frame(), player_list_for_type = character(0), error = NULL, season_string = season_string_for_api, no_data_found = TRUE)) }
        
        # Define ALL columns expected by create_stat_item and processing
        expected_base_cols <- c("PLAYER_ID", "PLAYER_NAME", "TEAM_ABBREVIATION", "AGE", "GP", "MIN", "PTS", "REB", "AST", "STL", "BLK", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT", "TOV", "PF")
        expected_adv_cols_for_select <- c("PLAYER_ID", "USG_PCT", "TS_PCT", "AST_PCT", "REB_PCT", "OFF_RATING", "DEF_RATING", "NET_RATING", "PIE")
        
        if (!all(expected_base_cols %in% names(base_df))) { 
          missing_cols <- setdiff(expected_base_cols, names(base_df))
          return(list(data = NULL, player_list_for_type = character(0), error = paste("Key base columns missing in", season_string_for_api, season_type_to_fetch, "base_df. Missing:", paste(missing_cols, collapse=", ")), season_string = season_string_for_api))
        }
        
        base_numeric_cols <- c("AGE", "GP", "MIN", "PTS", "REB", "AST", "STL", "BLK", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT", "TOV", "PF")
        adv_numeric_cols <- c("USG_PCT", "TS_PCT", "AST_PCT", "REB_PCT", "OFF_RATING", "DEF_RATING", "NET_RATING", "PIE")
        
        base_clean <- base_df %>% 
          dplyr::mutate(across(all_of(base_numeric_cols), as.numeric)) %>% 
          dplyr::select(
            playerId = PLAYER_ID, namePlayer = PLAYER_NAME, team = TEAM_ABBREVIATION, 
            age = AGE, gp = GP, min = MIN, pts = PTS, reb = REB, ast = AST, stl = STL, blk = BLK, 
            fgm = FGM, fga = FGA, pctFG = FG_PCT, fg3m = FG3M, fg3a = FG3A, pctFG3 = FG3_PCT, 
            ftm = FTM, fta = FTA, pctFT = FT_PCT, tov = TOV, pf = PF
          )
        
        adv_clean <- if (!is.null(adv_df) && nrow(adv_df) > 0 && "PLAYER_ID" %in% names(adv_df) && all(adv_numeric_cols %in% names(adv_df))) {
          adv_df %>% 
            dplyr::mutate(across(all_of(adv_numeric_cols), as.numeric)) %>% 
            dplyr::select(
              playerId = PLAYER_ID, 
              usg_pct = USG_PCT, ts_pct = TS_PCT, ast_pct = AST_PCT, reb_pct = REB_PCT, 
              off_rtg = OFF_RATING, def_rtg = DEF_RATING, net_rtg = NET_RATING, pie = PIE
            )
        } else {
          print(paste("Advanced stats (adv_df) for", season_string_for_api, season_type_to_fetch, "is empty or missing key columns. Creating adv_clean with NAs."))
          id_col_example <- if(nrow(base_clean) > 0 && "playerId" %in% names(base_clean)) base_clean$playerId[0] else character(0)
          tibble::tibble(
            playerId = id_col_example, 
            usg_pct=NA_real_, ts_pct=NA_real_, ast_pct=NA_real_, reb_pct=NA_real_, 
            off_rtg=NA_real_, def_rtg=NA_real_, net_rtg=NA_real_, pie=NA_real_
          )
        }
        
        merged_data <- tryCatch({ dplyr::full_join(base_clean, adv_clean, by = "playerId") %>% dplyr::arrange(namePlayer) }, 
                                error = function(e_join) { print(paste("Error merging stats for", season_string_for_api, season_type_to_fetch, ":", e_join$message)); NULL })
        
        if(!is.null(merged_data) && nrow(merged_data) > 0 ) { 
          return(list(data = merged_data, player_list_for_type = sort(unique(merged_data$namePlayer)), error = NULL, season_string = season_string_for_api))
        } else {
          return(list(data = data.frame(), player_list_for_type = character(0), error = paste("No valid data after processing for", season_string_for_api, season_type_to_fetch), season_string = season_string_for_api, no_data_found = TRUE))
        }
      } else { 
        return(list(data = NULL, player_list_for_type = character(0), error = paste("Unexpected API data format for", season_string_for_api, season_type_to_fetch), season_string = season_string_for_api))
      }
    }
    
    observeEvent(active_tab(), {
      req(active_tab() == tab_value, !rv$data_loaded)
      print("Player Stats Module: Main tab activated. Attempting to fetch Regular Season data with fallback...")
      
      primary_target_year <- get_primary_target_season_end_year() # e.g., 2023 if current year is 2025
      fallback_target_year <- primary_target_year - 1            # e.g., 2022 if current year is 2025
      
      result_primary <- fetch_and_process_player_stats(primary_target_year, "Regular Season")
      
      if (is.null(result_primary$error) && !is.null(result_primary$data) && nrow(result_primary$data) > 0) {
        rv$stats_data <- result_primary$data
        rv$stats_data_season_string <- result_primary$season_string
        rv$player_list <- result_primary$player_list_for_type
        rv$error_message <- NULL
        print(paste("Successfully loaded Regular Season data for primary target season:", result_primary$season_string))
      } else {
        primary_attempt_info <- result_primary$error %||% paste("No data found for", result_primary$season_string %||% paste0("season ending ", primary_target_year))
        print(paste("Primary target Regular Season data unavailable:", primary_attempt_info, ". Trying fallback season ending in", fallback_target_year))
        result_fallback <- fetch_and_process_player_stats(fallback_target_year, "Regular Season")
        
        if (is.null(result_fallback$error) && !is.null(result_fallback$data) && nrow(result_fallback$data) > 0) {
          rv$stats_data <- result_fallback$data
          rv$stats_data_season_string <- result_fallback$season_string
          rv$player_list <- result_fallback$player_list_for_type
          rv$error_message <- NULL 
          print(paste("Successfully loaded Regular Season data for fallback season:", result_fallback$season_string))
        } else {
          rv$stats_data <- NULL
          rv$stats_data_season_string <- result_primary$season_string %||% hoopR::year_to_season(primary_target_year)
          rv$player_list <- character(0)
          fallback_attempt_info <- result_fallback$error %||% paste("No data found for", result_fallback$season_string %||% paste0("season ending ", fallback_target_year))
          rv$error_message <- glue::glue("Could not load regular season data. Attempt for {rv$stats_data_season_string}: {primary_attempt_info}. Fallback to {result_fallback$season_string %||% paste0('season ending ', fallback_target_year)}: {fallback_attempt_info}.")
          print(rv$error_message)
        }
      }
      
      updateSelectizeInput(session, "player_select_1", choices = rv$player_list %||% character(0), selected = "", server = TRUE, options = list(placeholder = if(length(rv$player_list %||% character(0)) > 0) 'Type or select...' else 'Data unavailable...'))
      updateSelectizeInput(session, "player_select_2", choices = rv$player_list %||% character(0), selected = "", server = TRUE, options = list(placeholder = if(length(rv$player_list %||% character(0)) > 0) 'Type or select...' else 'Data unavailable...'))
      
      rv$data_loaded <- TRUE
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$stats_type_tabs, {
      current_sub_tab <- input$stats_type_tabs
      
      if (current_sub_tab == ns("subtab_playoff") && !rv$playoff_data_loaded) {
        print("Player Stats Module: 'Playoff Statistics' sub-tab selected. Attempting to fetch Playoff data with fallback...")
        
        primary_target_year_playoff <- get_primary_target_season_end_year()
        fallback_target_year_playoff <- primary_target_year_playoff - 1
        
        result_primary_playoff <- fetch_and_process_player_stats(primary_target_year_playoff, "Playoffs")
        
        if (is.null(result_primary_playoff$error) && !is.null(result_primary_playoff$data) && nrow(result_primary_playoff$data) > 0) {
          rv$playoff_stats_data <- result_primary_playoff$data
          rv$playoff_stats_data_season_string <- result_primary_playoff$season_string
          rv$playoff_player_list <- result_primary_playoff$player_list_for_type
          rv$playoff_error_message <- NULL
          print(paste("Successfully loaded Playoff data for primary target season:", result_primary_playoff$season_string))
        } else {
          primary_attempt_info_po <- result_primary_playoff$error %||% paste("No data found for", result_primary_playoff$season_string %||% paste0("playoffs ending ", primary_target_year_playoff))
          print(paste("Primary target Playoff data unavailable:", primary_attempt_info_po, ". Trying fallback season playoffs ending in", fallback_target_year_playoff))
          result_fallback_playoff <- fetch_and_process_player_stats(fallback_target_year_playoff, "Playoffs")
          
          if (is.null(result_fallback_playoff$error) && !is.null(result_fallback_playoff$data) && nrow(result_fallback_playoff$data) > 0) {
            rv$playoff_stats_data <- result_fallback_playoff$data
            rv$playoff_stats_data_season_string <- result_fallback_playoff$season_string
            rv$playoff_player_list <- result_fallback_playoff$player_list_for_type
            rv$playoff_error_message <- NULL
            print(paste("Successfully loaded Playoff data for fallback season:", result_fallback_playoff$season_string))
          } else {
            rv$playoff_stats_data <- NULL
            rv$playoff_stats_data_season_string <- result_primary_playoff$season_string %||% hoopR::year_to_season(primary_target_year_playoff)
            rv$playoff_player_list <- character(0)
            fallback_attempt_info_po <- result_fallback_playoff$error %||% paste("No data found for", result_fallback_playoff$season_string %||% paste0("playoffs ending ", fallback_target_year_playoff))
            rv$playoff_error_message <- glue::glue("Could not load playoff data. Attempt for {rv$playoff_stats_data_season_string}: {primary_attempt_info_po}. Fallback to {result_fallback_playoff$season_string %||% paste0('playoffs ending ', fallback_target_year_playoff)}: {fallback_attempt_info_po}.")
            print(rv$playoff_error_message)
          }
        }
        rv$playoff_data_loaded <- TRUE
      }
      
      current_selected_player1 <- isolate(input$player_select_1)
      current_selected_player2 <- isolate(input$player_select_2)
      
      if (current_sub_tab == ns("subtab_playoff")) {
        active_player_list_dd <- rv$playoff_player_list %||% character(0)
        placeholder_text_dd <- if(length(active_player_list_dd) > 0) 'Select Playoff Player...' else 'No playoff players found...'
        if(rv$playoff_data_loaded){ 
          updateSelectizeInput(session, "player_select_1", choices = active_player_list_dd, selected = if (!is.null(current_selected_player1) && current_selected_player1 %in% active_player_list_dd) current_selected_player1 else "", server = TRUE, options = list(placeholder = placeholder_text_dd))
          updateSelectizeInput(session, "player_select_2", choices = active_player_list_dd, selected = if (!is.null(current_selected_player2) && current_selected_player2 %in% active_player_list_dd) current_selected_player2 else "", server = TRUE, options = list(placeholder = placeholder_text_dd))
        }
      } else if (current_sub_tab == ns("subtab_season")) {
        active_player_list_dd <- rv$player_list %||% character(0)
        placeholder_text_dd <- if(length(active_player_list_dd) > 0) 'Type or select...' else 'Data unavailable...'
        if(rv$data_loaded){
          updateSelectizeInput(session, "player_select_1", choices = active_player_list_dd, selected = if (!is.null(current_selected_player1) && current_selected_player1 %in% active_player_list_dd) current_selected_player1 else "", server = TRUE, options = list(placeholder = placeholder_text_dd))
          updateSelectizeInput(session, "player_select_2", choices = active_player_list_dd, selected = if (!is.null(current_selected_player2) && current_selected_player2 %in% active_player_list_dd) current_selected_player2 else "", server = TRUE, options = list(placeholder = placeholder_text_dd))
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    output$module_description_text <- renderUI({
      p("Select two players below. Then, choose a statistics type (Season or Playoffs) to view their comparison.")
    })
    output$season_stats_description_text <- renderUI({
      season_str <- rv$stats_data_season_string %||% "the displayed"
      p(glue::glue("Comparing {season_str} Regular Season per-game statistics (Source: NBA Stats API via hoopR)."), style="text-align:center; font-style:italic; color:#aaa; margin-bottom:15px;")
    })
    output$playoff_stats_description_text <- renderUI({
      season_str <- rv$playoff_stats_data_season_string %||% "the displayed"
      p(glue::glue("Comparing {season_str} Playoff per-game statistics (Source: NBA Stats API via hoopR)."), style="text-align:center; font-style:italic; color:#aaa; margin-bottom:15px;")
    })
    
    get_player_headshot_url <- function(player_id) { req(player_id, !is.na(player_id)); url <- tryCatch({ hoopR::nba_playerheadshot(player_id = as.numeric(player_id)) }, error = function(e) { print(paste("Error fetching headshot for ID", player_id, ":", e$message)); NULL }); if (!is.null(url) && grepl("^http", url)) { return(url) } else { return("www_files/nba-logo.png") } }
    create_stat_item <- function(label, value, format_func = NULL) { value_clean <- if(is.na(value)) "N/A" else value; display_value <- if (!is.null(format_func) && value_clean != "N/A" && (is.numeric(value_clean) || inherits(value_clean, "numeric"))) format_func(value_clean) else if (is.numeric(value_clean) || inherits(value_clean, "numeric")) round(value_clean, 1) else value_clean; tags$div(class="stat-item", tags$span(class="stat-label", strong(paste0(label, ":"))), tags$span(class="stat-value", display_value)) }
    
    player1_season_data <- reactive({ req(rv$stats_data, input$player_select_1, input$player_select_1 != ""); rv$stats_data %>% dplyr::filter(namePlayer == input$player_select_1) })
    player2_season_data <- reactive({ req(rv$stats_data, input$player_select_2, input$player_select_2 != ""); rv$stats_data %>% dplyr::filter(namePlayer == input$player_select_2) })
    player1_playoff_data <- reactive({ req(rv$playoff_stats_data, input$player_select_1, input$player_select_1 != ""); rv$playoff_stats_data %>% dplyr::filter(namePlayer == input$player_select_1) })
    player2_playoff_data <- reactive({ req(rv$playoff_stats_data, input$player_select_2, input$player_select_2 != ""); rv$playoff_stats_data %>% dplyr::filter(namePlayer == input$player_select_2) })
    
    get_selected_player_id <- function(selected_player_name) {
      req(selected_player_name, selected_player_name != "")
      player_id_val <- NA 
      active_data_for_id <- if (!is.null(input$stats_type_tabs) && input$stats_type_tabs == ns("subtab_playoff") && !is.null(rv$playoff_stats_data)) { rv$playoff_stats_data } else { rv$stats_data }
      active_list_for_id <- if (!is.null(input$stats_type_tabs) && input$stats_type_tabs == ns("subtab_playoff") && !is.null(rv$playoff_player_list)) { rv$playoff_player_list } else { rv$player_list }
      if (!is.null(active_data_for_id) && !is.null(active_list_for_id) && selected_player_name %in% active_list_for_id) {
        player_info <- active_data_for_id %>% dplyr::filter(namePlayer == selected_player_name) %>% dplyr::slice(1)
        if (nrow(player_info) > 0 && "playerId" %in% names(player_info)) { player_id_val <- player_info$playerId }
      }
      if(is.na(player_id_val)){
        other_data_for_id <- if (!is.null(input$stats_type_tabs) && input$stats_type_tabs == ns("subtab_playoff")) rv$stats_data else rv$playoff_stats_data
        other_list_for_id <- if (!is.null(input$stats_type_tabs) && input$stats_type_tabs == ns("subtab_playoff")) rv$player_list else rv$playoff_player_list
        if(!is.null(other_data_for_id) && !is.null(other_list_for_id) && selected_player_name %in% other_list_for_id){
          player_info <- other_data_for_id %>% dplyr::filter(namePlayer == selected_player_name) %>% dplyr::slice(1)
          if (nrow(player_info) > 0 && "playerId" %in% names(player_info)) { player_id_val <- player_info$playerId }
        }
      }
      return(player_id_val)
    }
    player1_headshot <- reactive({ id <- get_selected_player_id(input$player_select_1); if(is.na(id)) return("www_files/nba-logo.png"); get_player_headshot_url(id) })
    player2_headshot <- reactive({ id <- get_selected_player_id(input$player_select_2); if(is.na(id)) return("www_files/nba-logo.png"); get_player_headshot_url(id) })
    
    render_player_card <- function(player_data_reactive, headshot_reactive, player_name_input_val, type = "Season", displayed_season_string) {
      p_dat <- player_data_reactive(); selected_player_name <- player_name_input_val
      season_text_for_message <- displayed_season_string %||% "the relevant"
      if (is.null(selected_player_name) || selected_player_name == "") { return(tags$div(class = "player-stats-grid", style = "text-align: center; padding: 20px; color: #aaa; min-height: 300px; display:flex; flex-direction:column; justify-content:center;", p("Select a player."))) }
      if (nrow(p_dat) == 0) { return(tags$div(class = "player-stats-grid", style = "text-align: center; padding: 20px; color: #aaa; min-height: 300px; display:flex; flex-direction:column; justify-content:center;", h4(selected_player_name, style="color: #fff; margin-bottom:10px;"), p(glue::glue("{selected_player_name} did not have {tolower(type)} stats for the {season_text_for_message} season.")))) }
      current_headshot <- headshot_reactive()
      stats_to_display_map <- list("Team" = "team", "Age" = "age", "GP" = "gp", "MIN" = "min", "PTS" = "pts", "REB" = "reb", "AST" = "ast", "STL" = "stl", "BLK" = "blk", "TOV" = "tov", "FG%" = "pctFG", "3P%" = "pctFG3", "FT%" = "pctFT", "TS%" = "ts_pct", "USG%" = "usg_pct")
      missing_display_cols <- setdiff(unlist(stats_to_display_map), names(p_dat))
      if (length(missing_display_cols) > 0) { print(paste("Warning: Player card for", selected_player_name,type, "is missing columns:", paste(missing_display_cols, collapse=", "))); return(tags$div(class="player-stats-grid", style="color:red; text-align:center; padding:10px;", "Error: Incomplete player data for display.")) }
      stat_items_list <- purrr::map2(names(stats_to_display_map), stats_to_display_map, function(label, col_name) { value <- p_dat[[col_name]][1]; format_func <- if (label %in% c("FG%", "3P%", "FT%", "TS%", "USG%")) scales::percent else NULL; create_stat_item(label, value, format_func = format_func) })
      tagList(tags$img(src = current_headshot, alt = p_dat$namePlayer %||% selected_player_name, class = "player-img"), h4(p_dat$namePlayer %||% selected_player_name, style="text-align: center; color: #ffffff; margin-top: 5px;"), tags$div(class="player-stats-grid", stat_items_list))
    }
    
    output$player1_season_info_ui <- renderUI({ render_player_card(player1_season_data, player1_headshot, input$player_select_1, "Regular Season", rv$stats_data_season_string) })
    output$player2_season_info_ui <- renderUI({ render_player_card(player2_season_data, player2_headshot, input$player_select_2, "Regular Season", rv$stats_data_season_string) })
    output$player1_playoff_info_ui <- renderUI({ render_player_card(player1_playoff_data, player1_headshot, input$player_select_1, "Playoff", rv$playoff_stats_data_season_string) })
    output$player2_playoff_info_ui <- renderUI({ render_player_card(player2_playoff_data, player2_headshot, input$player_select_2, "Playoff", rv$playoff_stats_data_season_string) })
    
    output$season_comparison_output <- renderUI({
      if (!rv$data_loaded && is.null(rv$error_message)) { return(tags$p(glue::glue("Loading regular season player data..."), style="text-align: center; color: #aaaaaa; margin-top: 20px;")) }
      if (!is.null(rv$error_message)) { return(tags$div(class = "validation-error-message", rv$error_message)) }
      if (rv$data_loaded && (is.null(rv$stats_data) || length(rv$player_list %||% character(0)) == 0)) { return(tags$div(class = "validation-error-message", rv$error_message %||% glue::glue("Failed to load or process regular season player data. Player list is empty."))) }
      if (is.null(input$player_select_1) || input$player_select_1 == "" || is.null(input$player_select_2) || input$player_select_2 == "") { return(tags$p("Please select two players using the dropdowns above.", style="text-align: center;")) }
      if (input$player_select_1 == input$player_select_2) { return(tags$div(class = "validation-error-message", "Please select two different players.")) }
      fluidRow(column(6, uiOutput(ns("player1_season_info_ui"))), column(6, uiOutput(ns("player2_season_info_ui"))))
    })
    
    output$playoff_comparison_output <- renderUI({
      if (!rv$playoff_data_loaded && is.null(rv$playoff_error_message)) { return(tags$p(glue::glue("Loading playoff player data..."), style="text-align: center; color: #aaaaaa; margin-top: 20px;")) }
      if (!is.null(rv$playoff_error_message)) { return(tags$div(class = "validation-error-message", rv$playoff_error_message)) }
      if (rv$playoff_data_loaded && (is.null(rv$playoff_player_list) || length(rv$playoff_player_list) == 0) && is.null(rv$playoff_error_message) ){ season_str <- rv$playoff_stats_data_season_string %||% "the displayed"; return(tags$p(glue::glue("No players participated in the {season_str} Playoffs or data is unavailable for this season."), style="text-align: center; color: #aaaaaa; margin-top: 20px;")) }
      if (is.null(input$player_select_1) || input$player_select_1 == "" || is.null(input$player_select_2) || input$player_select_2 == "") { if (input$stats_type_tabs == ns("subtab_playoff") && (is.null(rv$playoff_player_list) || length(rv$playoff_player_list) == 0)) { return() }; season_str <- rv$playoff_stats_data_season_string %||% "the displayed"; return(tags$p(glue::glue("Please select two players from the list for {season_str} playoff comparison."), style="text-align: center;")) }
      if (input$player_select_1 == input$player_select_2) { return(tags$div(class = "validation-error-message", "Please select two different players.")) }
      fluidRow(column(6, uiOutput(ns("player1_playoff_info_ui"))), column(6, uiOutput(ns("player2_playoff_info_ui"))))
    })
    
  }) # End moduleServer
}
# --- END FILE: modules/mod_player_stats.R ---