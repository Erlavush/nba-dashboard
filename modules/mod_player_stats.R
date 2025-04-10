# modules/mod_player_stats.R
# CLEAN VERSION: Handles player comparison with headshots and 3-col stat grid only.

#' Player Stats Module UI Function
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_player_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Player Season Statistics Comparison"),
    p("Select two players to compare their stats for the 2023-24 Regular Season (Source: NBA Stats API via hoopR)."),
    fluidRow(
      column(6, selectizeInput(ns("player_select_1"), label = "Select Player 1:", choices = NULL, options = list(placeholder = 'Type or select...', onInitialize = I('function() { this.setValue(""); }')))),
      column(6, selectizeInput(ns("player_select_2"), label = "Select Player 2:", choices = NULL, options = list(placeholder = 'Type or select...', onInitialize = I('function() { this.setValue(""); }'))))
    ),
    hr(),
    # Main output area for player cards OR messages
    uiOutput(ns("comparison_display_area"))
  )
}

#' Player Stats Module Server Function
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_player_stats_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # --- Reactive Values ---
    rv <- reactiveValues(stats_data = NULL, player_list = NULL, error_message = NULL)
    
    # --- Fetch Data (Merge Base & Advanced) ---
    # This block remains the same as the previous correct version
    observe({
      current_season <- 2024
      season_string <- hoopR::year_to_season(current_season)
      print(paste("Player Stats Module: Attempting to fetch Base & Advanced stats using hoopR (Season:", season_string, ")"))
      base_stats_list <- tryCatch({ hoopR::nba_leaguedashplayerstats(season = season_string, season_type = "Regular Season", measure_type = "Base", per_mode = "PerGame") }, error = function(e) { print(paste("Error fetching Base stats:", e$message)); NULL })
      advanced_stats_list <- tryCatch({ hoopR::nba_leaguedashplayerstats(season = season_string, season_type = "Regular Season", measure_type = "Advanced", per_mode = "PerGame") }, error = function(e) { print(paste("Error fetching Advanced stats:", e$message)); NULL })
      
      if (!is.null(base_stats_list) && "LeagueDashPlayerStats" %in% names(base_stats_list) && !is.null(advanced_stats_list) && "LeagueDashPlayerStats" %in% names(advanced_stats_list)) {
        base_df <- base_stats_list[["LeagueDashPlayerStats"]]
        adv_df <- advanced_stats_list[["LeagueDashPlayerStats"]]
        print("Player Stats Module: Extracted Base and Advanced data frames.")
        base_numeric_cols <- c("AGE", "GP", "MIN", "PTS", "REB", "AST", "STL", "BLK", "FGM", "FGA", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT", "TOV", "PF")
        adv_numeric_cols <- c("E_OFF_RATING", "OFF_RATING", "E_DEF_RATING", "DEF_RATING", "E_NET_RATING", "NET_RATING", "AST_PCT", "AST_TO", "AST_RATIO", "OREB_PCT", "DREB_PCT", "REB_PCT", "TM_TOV_PCT", "EFG_PCT", "TS_PCT", "USG_PCT", "E_USG_PCT", "E_PACE", "PACE", "PACE_PER40", "POSS", "PIE")
        
        base_clean <- base_df %>% dplyr::mutate(across(any_of(base_numeric_cols), as.numeric)) %>% dplyr::select(playerId = PLAYER_ID, namePlayer = PLAYER_NAME, team = TEAM_ABBREVIATION, age = AGE, gp = GP, min = MIN, pts = PTS, reb = REB, ast = AST, stl = STL, blk = BLK, fgm = FGM, fga = FGA, pctFG = FG_PCT, fg3m = FG3M, fg3a = FG3A, pctFG3 = FG3_PCT, ftm = FTM, fta = FTA, pctFT = FT_PCT, tov = TOV, pf = PF)
        adv_clean <- adv_df %>% dplyr::mutate(across(any_of(adv_numeric_cols), as.numeric)) %>% dplyr::select(playerId = PLAYER_ID, usg_pct = USG_PCT, ts_pct = TS_PCT, ast_pct = AST_PCT, reb_pct = REB_PCT, off_rtg = OFF_RATING, def_rtg = DEF_RATING, net_rtg = NET_RATING, pie = PIE)
        
        merged_stats <- tryCatch({ dplyr::full_join(base_clean, adv_clean, by = "playerId") %>% dplyr::arrange(namePlayer) }, error = function(e_join) { print(paste("Error merging stats:", e_join$message)); NULL })
        
        if(!is.null(merged_stats)) {
          rv$stats_data <- merged_stats
          rv$player_list <- sort(unique(rv$stats_data$namePlayer))
          print("Player Stats Module: Base and Advanced stats merged. Player list created.")
          rv$error_message <- NULL
        } else { rv$error_message <- "Error: Failed to merge base and advanced player stats."; rv$stats_data <- NULL; rv$player_list <- NULL }
      } else { print("Player Stats Module: Failed to fetch Base or Advanced stats."); rv$error_message <- "Error: Could not retrieve complete player statistics (Base or Advanced)."; rv$stats_data <- NULL; rv$player_list <- NULL }
    })
    
    # --- Update Dropdowns ---
    observe({ req(rv$player_list); updateSelectizeInput(session, "player_select_1", choices = rv$player_list, selected = "", server = TRUE); updateSelectizeInput(session, "player_select_2", choices = rv$player_list, selected = "", server = TRUE) })
    
    # --- Headshot Helper ---
    get_player_headshot_url <- function(player_id) { req(player_id); print(paste("Fetching headshot for player ID:", player_id)); url <- tryCatch({ hoopR::nba_playerheadshot(player_id = player_id) }, error = function(e) { print(paste("Error fetching headshot:", e$message)); NULL }); if (!is.null(url) && grepl("^http", url)) { return(url) } else { return("https://cdn.nba.com/headshots/nba/latest/260x190/fallback.png") } }
    
    # --- Player Data Reactives ---
    player1_data <- reactive({ req(rv$stats_data, input$player_select_1, input$player_select_1 != ""); rv$stats_data %>% filter(namePlayer == input$player_select_1) })
    player2_data <- reactive({ req(rv$stats_data, input$player_select_2, input$player_select_2 != ""); rv$stats_data %>% filter(namePlayer == input$player_select_2) })
    player1_headshot <- reactive({ req(player1_data()); get_player_headshot_url(player1_data()$playerId) })
    player2_headshot <- reactive({ req(player2_data()); get_player_headshot_url(player2_data()$playerId) })
    # combined_data <- reactive({ ... }) # No longer needed if no plots use it
    
    # --- Stat Item Helper ---
    create_stat_item <- function(label, value, format_func = NULL) { value_clean <- if(is.na(value)) "N/A" else value; display_value <- if (!is.null(format_func) && value_clean != "N/A") format_func(value_clean) else if (is.numeric(value_clean)) round(value_clean, 1) else value_clean; tags$div(class="stat-item", tags$span(class="stat-label", strong(paste0(label, ":"))), tags$span(class="stat-value", display_value)) }
    
    # --- Player Info Box UI Renderers ---
    output$player1_info_ui <- renderUI({ req(player1_data(), player1_headshot()); p_dat <- player1_data(); tagList( tags$img(src = player1_headshot(), alt = p_dat$namePlayer, class = "player-img"), h4(p_dat$namePlayer, style="text-align: center; color: #ffffff; margin-top: 5px;"), tags$div(class="player-stats-grid", create_stat_item("Team", p_dat$team, format_func = identity), create_stat_item("Age", p_dat$age, format_func = identity), create_stat_item("GP", p_dat$gp, format_func = identity), create_stat_item("MIN", p_dat$min), create_stat_item("PTS", p_dat$pts), create_stat_item("REB", p_dat$reb), create_stat_item("AST", p_dat$ast), create_stat_item("STL", p_dat$stl), create_stat_item("BLK", p_dat$blk), create_stat_item("TOV", p_dat$tov), create_stat_item("FG%", p_dat$pctFG, format_func = scales::percent), create_stat_item("3P%", p_dat$pctFG3, format_func = scales::percent), create_stat_item("FT%", p_dat$pctFT, format_func = scales::percent), create_stat_item("TS%", p_dat$ts_pct, format_func = scales::percent), create_stat_item("USG%", p_dat$usg_pct, format_func = scales::percent) )) })
    output$player2_info_ui <- renderUI({ req(player2_data(), player2_headshot()); p_dat <- player2_data(); tagList( tags$img(src = player2_headshot(), alt = p_dat$namePlayer, class = "player-img"), h4(p_dat$namePlayer, style="text-align: center; color: #ffffff; margin-top: 5px;"), tags$div(class="player-stats-grid", create_stat_item("Team", p_dat$team, format_func = identity), create_stat_item("Age", p_dat$age, format_func = identity), create_stat_item("GP", p_dat$gp, format_func = identity), create_stat_item("MIN", p_dat$min), create_stat_item("PTS", p_dat$pts), create_stat_item("REB", p_dat$reb), create_stat_item("AST", p_dat$ast), create_stat_item("STL", p_dat$stl), create_stat_item("BLK", p_dat$blk), create_stat_item("TOV", p_dat$tov), create_stat_item("FG%", p_dat$pctFG, format_func = scales::percent), create_stat_item("3P%", p_dat$pctFG3, format_func = scales::percent), create_stat_item("FT%", p_dat$pctFT, format_func = scales::percent), create_stat_item("TS%", p_dat$ts_pct, format_func = scales::percent), create_stat_item("USG%", p_dat$usg_pct, format_func = scales::percent) )) })
    
    # --- Main Display Area UI Renderer (No Plot Anymore) ---
    output$comparison_display_area <- renderUI({
      if (!is.null(rv$error_message)) { tags$div(class = "validation-error-message", rv$error_message) }
      else if (is.null(input$player_select_1) || input$player_select_1 == "" || is.null(input$player_select_2) || input$player_select_2 == "") { if (is.null(rv$stats_data)) { tags$p("Loading player data...") } else { tags$p("Please select two players using the dropdowns above.", style="text-align: center;") } } # Centered prompt
      else if (input$player_select_1 == input$player_select_2) { tags$div(class = "validation-error-message", "Please select two different players.") }
      else {
        # Only show the player cards
        fluidRow(
          column(6, uiOutput(ns("player1_info_ui"))),
          column(6, uiOutput(ns("player2_info_ui")))
        )
      }
    })
    
    # --- REMOVED ALL PLOTTING CODE ---
    # No radar_data_scaled reactive needed
    # No renderPlotly or renderEcharts4r needed
    
  }) # End moduleServer
} # End Server Function

