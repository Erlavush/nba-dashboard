# modules/mod_welcome.R
# VERSION: Scoreboard, Standings & Multiple Leaders (PTS, AST, REB, BLK, STL)
# Uses SEPARATED LOGIC and HELPER FUNCTIONS for fetching and rendering leaders.

# Needed libraries (Ensure loaded in global.R):
# shiny, dplyr, purrr, hoopR, glue, tidyr, scales, rlang

#' Welcome Module UI Function
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Main Content Row with 3 columns (e.g., 4 + 4 + 4 = 12)
    fluidRow(
      # --- Left Column: Scoreboard & Date Controls ---
      column(width = 4, # Adjust width as needed
             div(class = "date-nav-container",
                 actionButton(ns("prev_day"), label = NULL, icon = icon("angle-left"), class = "date-nav-btn"),
                 tags$div(class="date-display-wrapper", uiOutput(ns("current_date_display"), inline = TRUE)),
                 actionButton(ns("next_day"), label = NULL, icon = icon("angle-right"), class = "date-nav-btn"),
                 tags$div(style = "display: none;", dateInput(ns("select_date"), label = NULL, value = Sys.Date())) # Hidden date input
             ),
             h4("Games", style = "color: #ffffff; border-bottom: 1px solid #444; padding-bottom: 5px; margin-top: 15px; margin-bottom: 15px;"),
             uiOutput(ns("scoreboard_output")) # Output for scoreboard cards
      ), # End Left Column
      
      # --- Middle Column: Standings ---
      column(width = 4, # Adjust width as needed
             h4("Standings", style = "color: #ffffff; border-bottom: 1px solid #444; padding-bottom: 5px; margin-bottom: 15px;"),
             uiOutput(ns("standings_output")) # Output for standings tables
      ), # End Middle Column
      
      # --- Right Column: League Leaders ---
      column(width = 4, # Adjust width as needed
             h4("League Leaders", style = "color: #ffffff; border-bottom: 1px solid #444; padding-bottom: 5px; margin-bottom: 15px;"), # General Title
             # Outputs for each leader category, displayed vertically
             uiOutput(ns("leaders_pts_output")),
             tags$br(),
             uiOutput(ns("leaders_ast_output")),
             tags$br(),
             uiOutput(ns("leaders_reb_output")),
             tags$br(),
             uiOutput(ns("leaders_blk_output")),
             tags$br(),
             uiOutput(ns("leaders_stl_output"))
      ) # End Right Column
      
    ) # End Main Content fluidRow
  ) # End tagList
}

#' Welcome Module Server Function
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # --- Separate Reactive Values for Each Section ---
    rv_sb <- reactiveValues( data = NULL, error = NULL, last_fetch_time = NULL ) # Scoreboard
    rv_st <- reactiveValues( data = NULL, error = NULL, last_fetch_time = NULL ) # Standings
    # Reactive values for each leader category
    rv_ld_pts <- reactiveValues( data = NULL, error = NULL, last_fetch_time = NULL) # Points
    rv_ld_ast <- reactiveValues( data = NULL, error = NULL, last_fetch_time = NULL) # Assists
    rv_ld_reb <- reactiveValues( data = NULL, error = NULL, last_fetch_time = NULL) # Rebounds
    rv_ld_blk <- reactiveValues( data = NULL, error = NULL, last_fetch_time = NULL) # Blocks
    rv_ld_stl <- reactiveValues( data = NULL, error = NULL, last_fetch_time = NULL) # Steals
    
    # --- Shared Reactive Value for Selected Date ---
    selected_date <- reactiveVal(Sys.Date() - 1) # Default to yesterday
    
    # --- Date Control Observers (Shared) ---
    observeEvent(input$prev_day, { selected_date(selected_date() - 1); updateDateInput(session, "select_date", value = selected_date()) })
    observeEvent(input$next_day, { selected_date(selected_date() + 1); updateDateInput(session, "select_date", value = selected_date()) })
    observeEvent(input$select_date, { req(input$select_date); if (!identical(input$select_date, selected_date())) { selected_date(input$select_date) } }, ignoreInit = TRUE)
    output$current_date_display <- renderUI({ req(selected_date()); tags$h5(format(selected_date(), "%a, %b %d, %Y"), style="margin:0;") })
    
    # --- Headshot Helper & Cache (Shared but self-contained) ---
    headshot_cache <- reactiveValues()
    get_player_headshot_url <- function(player_id) {
      req(player_id)
      cache_key <- as.character(player_id)
      if (!is.null(headshot_cache[[cache_key]])) {
        return(headshot_cache[[cache_key]])
      }
      print(paste("Fetching headshot for player ID:", player_id)) # Keep this for debugging fetch calls
      url <- tryCatch({
        hoopR::nba_playerheadshot(player_id = as.numeric(player_id))
      }, error = function(e) {
        print(paste("Error fetching headshot for", player_id, ":", e$message)); NULL
      })
      final_url <- if (!is.null(url) && grepl("^http", url)) { url }
      else { "https://cdn.nba.com/headshots/nba/latest/260x190/fallback.png" } # Fallback image
      headshot_cache[[cache_key]] <- final_url
      return(final_url)
    }
    
    # --- SEPARATE Fetch Function for Scoreboard ---
    fetch_scoreboard_data <- function(fetch_date) {
      # (Code from previous correct version)
      current_date_string <- format(fetch_date, "%Y-%m-%d")
      current_date_espn <- format(fetch_date, "%Y%m%d")
      print(paste("Fetching Scoreboard data for date:", current_date_string))
      rv_sb$last_fetch_time <- Sys.time(); rv_sb$data <- NULL; rv_sb$error <- NULL
      data_sb <- NULL; sb_v3_err_msg <- NULL; sb_espn_err_msg <- NULL
      v3_result_sb <- tryCatch({ hoopR::nba_scoreboardv3(game_date = current_date_string) }, error = function(e) { list(error = TRUE, message = paste("v3 internal error:", e$message)) })
      is_v3_error_structure <- is.list(v3_result_sb) && !is.null(v3_result_sb$error) && isTRUE(v3_result_sb$error)
      if (!is_v3_error_structure) {
        if ("scoreboard" %in% names(v3_result_sb) && !is.null(v3_result_sb$scoreboard) && is.data.frame(v3_result_sb$scoreboard) && nrow(v3_result_sb$scoreboard) > 0) {
          sb_data_v3 <- v3_result_sb$scoreboard
          has_records_v3 <- all(c("home_wins", "home_losses", "away_wins", "away_losses") %in% names(sb_data_v3))
          data_sb <- tryCatch({
            sb_data_v3 %>%
              dplyr::select( gameId = game_id, gameStatus = game_status_text, gameTime = game_et, homeTeamName = home_team_name, homeTeamAbbr = home_team_tricode, homeScore = home_score, awayTeamName = away_team_name, awayTeamAbbr = away_team_tricode, awayScore = away_score, any_of(c(home_wins = "home_wins", home_losses = "home_losses", away_wins = "away_wins", away_losses = "away_losses")) ) %>%
              dplyr::mutate( homeLogo = glue::glue("https://cdn.nba.com/logos/nba/{homeTeamAbbr}/global/L/logo.svg"), awayLogo = glue::glue("https://cdn.nba.com/logos/nba/{awayTeamAbbr}/global/L/logo.svg"), homeRecord = if(has_records_v3 && exists("home_wins", where = .)) paste0("(", home_wins, "-", home_losses, ")") else NA_character_, awayRecord = if(has_records_v3 && exists("away_wins", where = .)) paste0("(", away_wins, "-", away_losses, ")") else NA_character_, across(c(homeScore, awayScore), ~ as.integer(tidyr::replace_na(.x, NA))) ) %>%
              dplyr::select(gameId, gameStatus, gameTime, homeTeamName, homeTeamAbbr, homeScore, homeLogo, homeRecord, awayTeamName, awayTeamAbbr, awayScore, awayLogo, awayRecord)
          }, error = function(e_proc) { print(paste("Error processing v3 scoreboard:", e_proc$message)); sb_v3_err_msg <<- paste("Proc fail:", e_proc$message); NULL })
        } else { sb_v3_err_msg <- "V3 success, but no game data." }
      } else { sb_v3_err_msg <- v3_result_sb$message %||% "V3 fetch failed (error structure)." }
      if (is.null(data_sb)) {
        print("Attempting ESPN scoreboard fallback...")
        espn_result_sb <- tryCatch({ hoopR::espn_nba_scoreboard(season = current_date_espn) }, error = function(e) { list(error = TRUE, message = paste("espn internal error:", e$message)) })
        is_espn_error_structure <- is.list(espn_result_sb) && !is.null(espn_result_sb$error) && isTRUE(espn_result_sb$error)
        if (!is_espn_error_structure) {
          if(is.data.frame(espn_result_sb) && nrow(espn_result_sb) > 0) {
            espn_data <- espn_result_sb
            has_records_espn <- all(c("home_record", "away_record") %in% names(espn_data))
            data_sb <- tryCatch({
              espn_data %>%
                dplyr::select( gameId = game_id, gameStatus = status_name, gameTime = start_date, homeTeamName = home_team_name, homeTeamAbbr = home_team_abb, homeScore = home_score, homeLogo = home_team_logo, awayTeamName = away_team_name, awayTeamAbbr = away_team_abb, awayScore = away_score, awayLogo = away_team_logo, any_of(c(homeRecord_raw = "home_record", awayRecord_raw = "away_record")) ) %>%
                dplyr::mutate( homeRecord = if(has_records_espn && exists("homeRecord_raw", where = .)) paste0("(", homeRecord_raw, ")") else NA_character_, awayRecord = if(has_records_espn && exists("awayRecord_raw", where = .)) paste0("(", awayRecord_raw, ")") else NA_character_, across(c(homeScore, awayScore), ~ as.integer(tidyr::replace_na(.x, NA))) ) %>%
                dplyr::select(gameId, gameStatus, gameTime, homeTeamName, homeTeamAbbr, homeScore, homeLogo, homeRecord, awayTeamName, awayTeamAbbr, awayScore, awayLogo, awayRecord)
            }, error = function(e_proc) { print(paste("Error processing ESPN scoreboard:", e_proc$message)); sb_espn_err_msg <<- paste("Proc fail:", e_proc$message); NULL })
          } else { sb_espn_err_msg <- "ESPN success, but no game data." }
        } else { sb_espn_err_msg <- espn_result_sb$message %||% "ESPN fetch failed (error structure)." }
      }
      if (!is.null(data_sb)) { rv_sb$data <- data_sb; print(paste("Scoreboard fetch SUCCESS:", nrow(data_sb), "games.")) }
      else { rv_sb$error <- glue::glue("SB Unavail. (V3: {sb_v3_err_msg %||% 'Fail'}) (ESPN: {sb_espn_err_msg %||% 'Fail'})"); print(paste("Scoreboard fetch FAILED:", rv_sb$error)) }
      print("--- Finished fetch_scoreboard_data ---")
    } # End fetch_scoreboard_data
    
    # --- SEPARATE Fetch Function for Standings ---
    fetch_standings_data <- function(fetch_date) {
      # (Code from previous correct version)
      current_season_year_st <- as.numeric(format(fetch_date, "%Y"))
      season_start_year_st <- if(as.numeric(format(fetch_date, "%m")) < 10) current_season_year_st - 1 else current_season_year_st
      end_year_short_st <- (season_start_year_st + 1) %% 100
      season_slug_st <- sprintf("%d-%02d", season_start_year_st, end_year_short_st)
      print(paste("Fetching Standings data for season:", season_slug_st))
      rv_st$last_fetch_time <- Sys.time(); rv_st$data <- NULL; rv_st$error <- NULL
      standings_raw <- tryCatch({ hoopR::nba_leaguestandingsv3(season = season_slug_st, season_type = "Regular Season") }, error = function(e) { list(error = TRUE, message = paste("Standings internal error:", e$message)) })
      if (!is.list(standings_raw) || !isTRUE(standings_raw$error)) {
        if ("Standings" %in% names(standings_raw) && is.data.frame(standings_raw$Standings) && nrow(standings_raw$Standings) > 0) {
          standings_df_st <- standings_raw$Standings
          expected_cols_st <- c("TeamID", "TeamName", "TeamCity", "TeamSlug", "Conference", "PlayoffRank", "WINS", "LOSSES", "WinPCT", "ConferenceGamesBack", "ConferenceRecord", "HOME", "ROAD", "L10", "CurrentStreak")
          if(all(expected_cols_st %in% names(standings_df_st))) {
            processed_standings <- tryCatch({
              standings_df_st %>%
                dplyr::select( teamId = TeamID, teamName = TeamName, teamCity = TeamCity, teamAbbr = TeamSlug, conf = Conference, confRank = PlayoffRank, W = WINS, L = LOSSES, Pct = WinPCT, GB = ConferenceGamesBack, confRecord = ConferenceRecord, homeRecord = HOME, awayRecord = ROAD, L10 = L10, Strk = CurrentStreak ) %>%
                dplyr::mutate( across(c(W, L, Pct, GB), ~ suppressWarnings(as.numeric(.x))), across(c(confRank), ~ suppressWarnings(as.integer(.x))), GB = if_else(confRank == 1 & (is.na(GB) | GB == ""), 0.0, GB) ) %>%
                dplyr::arrange(conf, confRank)
            }, error = function(e_proc){ print(paste("Error processing standings:", e_proc$message)); NULL })
            if (!is.null(processed_standings)) { rv_st$data <- processed_standings; print("Standings fetch SUCCESS.") }
            else { rv_st$error <- "Standings processing failed."; print(rv_st$error) }
          } else { missing_cols_st <- setdiff(expected_cols_st, names(standings_df_st)); rv_st$error <- glue::glue("Standings missing cols: {paste(missing_cols_st, collapse=', ')}"); print(rv_st$error) }
        } else { rv_st$error <- "Standings data empty/wrong format."; print(rv_st$error) }
      } else { rv_st$error <- standings_raw$message %||% "Standings fetch failed."; print(paste("Standings fetch FAILED:", rv_st$error)) }
      print("--- Finished fetch_standings_data ---")
    } # End fetch_standings_data
    
    # --- Helper Function to Fetch Specific Leader Category ---
    # (Defined in previous step - ensure it's here)
    fetch_leader_category <- function(stat_category, rv_target, fetch_date, season_slug_override = NULL) {
      season_slug_ld <- if (!is.null(season_slug_override)) {
        season_slug_override
      } else {
        current_season_year_ld <- as.numeric(format(fetch_date, "%Y"))
        season_start_year_ld <- if(as.numeric(format(fetch_date, "%m")) < 10) current_season_year_ld - 1 else current_season_year_ld
        end_year_short_ld <- (season_start_year_ld + 1) %% 100
        sprintf("%d-%02d", season_start_year_ld, end_year_short_ld)
      }
      print(paste("Fetching", stat_category, "Leaders data for season:", season_slug_ld))
      rv_target$last_fetch_time <- Sys.time()
      rv_target$data <- NULL; rv_target$error <- NULL
      leaders_result <- NULL
      error_message_ld <- NULL
      leaders_result <- tryCatch({
        hoopR::nba_leagueleaders(
          season = season_slug_ld,
          stat_category = stat_category,
          season_type = "Regular Season",
          per_mode = "PerGame"
        )
      }, error = function(e) {
        error_message_ld <<- paste("API Call Error (", stat_category, " Leaders):", e$message)
        NULL
      })
      if (!is.null(error_message_ld)) {
        rv_target$error <- error_message_ld
        print(paste(stat_category, "Leaders fetch FAILED:", rv_target$error))
      } else if (is.null(leaders_result)) {
        rv_target$error <- paste("API call for", stat_category, "Leaders returned NULL.")
        print(paste(stat_category, "Leaders fetch FAILED:", rv_target$error))
      } else if (!is.list(leaders_result) || !"LeagueLeaders" %in% names(leaders_result)) {
        rv_target$error <- paste(stat_category, "Leaders structure unexpected.")
        print(paste(stat_category, "Leaders fetch FAILED:", rv_target$error))
      } else if (!is.data.frame(leaders_result$LeagueLeaders)) {
        rv_target$error <- paste(stat_category, "Leaders 'LeagueLeaders' not a data frame.")
        print(paste(stat_category, "Leaders fetch FAILED:", rv_target$error))
      } else if (nrow(leaders_result$LeagueLeaders) == 0) {
        print(paste(stat_category, "Leaders fetch SUCCESS (0 rows returned)."))
        rv_target$data <- data.frame()
      } else {
        print(paste(stat_category, "Leaders fetch SUCCESS:", nrow(leaders_result$LeagueLeaders), "rows found."))
        processed_leaders <- tryCatch({
          leaders_result$LeagueLeaders %>%
            dplyr::slice_head(n = 5) %>%
            dplyr::select(
              playerId = PLAYER_ID,
              Rank = RANK,
              Player = PLAYER,
              Team = TEAM,
              Value = !!rlang::sym(stat_category) # Use dynamic symbol
            ) %>%
            dplyr::mutate(
              Value = suppressWarnings(as.numeric(Value))
            )
        }, error = function(e_proc) {
          print(paste("Error processing", stat_category, "leaders data:", e_proc$message)); NULL
        })
        if(!is.null(processed_leaders)){
          rv_target$data <- processed_leaders
          print(paste("Stored processed", stat_category, "leaders."))
        } else {
          rv_target$error <- paste("Failed to process", stat_category, "leaders data.")
          print(paste(stat_category, "Leaders FAILED:", rv_target$error))
        }
      }
      print(paste("--- Finished fetch_leader_category for", stat_category, "---"))
    } # End fetch_leader_category function
    
    # --- Auto-Refresh Timer (Shared) ---
    auto_refresh <- reactiveTimer(900000) # 15 minutes
    
    # --- SINGLE Observer to Trigger ALL Fetches ---
    observe({
      current_date_to_fetch <- selected_date()
      timer_val <- auto_refresh()
      
      print(paste("AUTO/DATE TRIGGER (All): Fetching ALL for date:", format(current_date_to_fetch, "%Y-%m-%d")))
      
      # Call scoreboard and standings fetches
      fetch_scoreboard_data(current_date_to_fetch)
      Sys.sleep(0.2) # Small pause between different API types
      fetch_standings_data(current_date_to_fetch)
      Sys.sleep(0.2)
      
      # Calculate season slug once for all leader fetches
      current_season_year_ld <- as.numeric(format(current_date_to_fetch, "%Y"))
      season_start_year_ld <- if(as.numeric(format(current_date_to_fetch, "%m")) < 10) current_season_year_ld - 1 else current_season_year_ld
      end_year_short_ld <- (season_start_year_ld + 1) %% 100
      common_season_slug_ld <- sprintf("%d-%02d", season_start_year_ld, end_year_short_ld)
      
      # Call the helper for each leader category
      fetch_leader_category("PTS", rv_ld_pts, current_date_to_fetch, common_season_slug_ld)
      Sys.sleep(0.6) # Keep longer pause between leader API calls
      fetch_leader_category("AST", rv_ld_ast, current_date_to_fetch, common_season_slug_ld)
      Sys.sleep(0.6)
      fetch_leader_category("REB", rv_ld_reb, current_date_to_fetch, common_season_slug_ld)
      Sys.sleep(0.6)
      fetch_leader_category("BLK", rv_ld_blk, current_date_to_fetch, common_season_slug_ld)
      Sys.sleep(0.6)
      fetch_leader_category("STL", rv_ld_stl, current_date_to_fetch, common_season_slug_ld)
      
      print("--- Finished ALL fetches triggered by observer ---")
    })
    
    
    # --- Render Scoreboard UI (Uses rv_sb) ---
    # (Code unchanged from previous version)
    output$scoreboard_output <- renderUI({
      if (!is.null(rv_sb$error)) { return(tags$div(class = "validation-error-message", rv_sb$error)) }
      if (is.null(rv_sb$data) && is.null(rv_sb$error)) { return(tags$p("Loading scoreboard...", style="color: #aaaaaa; text-align: center;")) }
      if (is.data.frame(rv_sb$data) && nrow(rv_sb$data) == 0) { return(tags$p(glue::glue("No games found for {format(selected_date(), '%a, %b %d')}."), style="color: #aaaaaa; text-align: center;")) }
      if (is.data.frame(rv_sb$data)) {
        game_cards <- tryCatch({
          purrr::pmap( .l = rv_sb$data, .f = function(gameStatus, gameTime, homeTeamName, awayTeamName, homeTeamAbbr, awayTeamAbbr, homeLogo, awayLogo, homeScore, awayScore, homeRecord, awayRecord, ...) {
            game_status_text <- gameStatus %||% "Scheduled"; game_time_text <- gameTime %||% ""
            home_name <- homeTeamName %||% "Home"; away_name <- awayTeamName %||% "Away"
            home_abbr <- homeTeamAbbr %||% "HMT"; away_abbr <- awayTeamAbbr %||% "AWT"
            home_logo_url <- homeLogo %||% "https://via.placeholder.com/50"; away_logo_url <- awayLogo %||% "https://via.placeholder.com/50"
            home_s <- homeScore; away_s <- awayScore
            home_rec <- homeRecord %||% ""; away_rec <- awayRecord %||% ""
            game_status_display <- dplyr::case_when( grepl("Final|FT", game_status_text, ignore.case = TRUE) ~ "Final", grepl("pm|am|ET|:", game_status_text, ignore.case = TRUE) ~ game_status_text, nchar(game_time_text) > 4 ~ tryCatch(format(as.POSIXct(game_time_text, tz="UTC"), format="%I:%M %p ET"), error=function(e) game_time_text), TRUE ~ game_status_text )
            home_score_display <- if (is.na(home_s) || home_s == "") "--" else as.character(home_s)
            away_score_display <- if (is.na(away_s) || away_s == "") "--" else as.character(away_s)
            tags$div(class = "sb-game-card",
                     tags$div(class = "sb-top-bar", tags$span(class = "sb-game-date", paste("NBA ·", format(selected_date(), "%a, %b %d"))), tags$span(class = "sb-game-status", game_status_display)),
                     tags$div(class = "sb-main-content",
                              tags$div(class = "sb-team sb-away-team", tags$img(src = away_logo_url, class="sb-team-logo", alt = away_abbr), tags$div(class = "sb-team-details", tags$span(class = "sb-team-name", away_name), tags$span(class = "sb-team-record", away_rec))),
                              tags$div(class = "sb-score-area", tags$span(class = "sb-score", away_score_display), tags$span(class = "sb-score-separator", HTML("–")), tags$span(class = "sb-score", home_score_display)),
                              tags$div(class = "sb-team sb-home-team", tags$div(class = "sb-team-details", tags$span(class = "sb-team-name", home_name), tags$span(class = "sb-team-record", home_rec)), tags$img(src = home_logo_url, class="sb-team-logo", alt = home_abbr))
                     ) # end sb-main-content
            ) # end sb-game-card
          }) # End pmap
        }, error = function(e){ print(paste("Error rendering scoreboard cards:", e$message)); tags$div(class = "validation-error-message", "Error displaying scoreboard details.") })
        tagList(game_cards)
      } else { tags$div(class = "validation-error-message", "Scoreboard data invalid format.") }
    }) # End renderUI scoreboard_output
    
    # --- Render Standings UI (Uses rv_st) ---
    # (Code unchanged from previous version)
    output$standings_output <- renderUI({
      if (!is.null(rv_st$error)) { return(tags$div(class = "validation-error-message", rv_st$error)) }
      if (is.null(rv_st$data) && is.null(rv_st$error)) { return(tags$p("Loading standings...", style="color: #aaaaaa; text-align: center;")) }
      if (!is.data.frame(rv_st$data) || nrow(rv_st$data) == 0) { return(tags$p("Standings data unavailable or empty.", style="color: #aaaaaa; text-align: center;")) }
      standings <- rv_st$data
      east_standings <- if(any(standings$conf == "East")) standings %>% filter(conf == "East") %>% select(any_of(c(Rank="confRank", Team="teamName", W="W", L="L", Pct="Pct", GB="GB", Strk="Strk"))) else data.frame()
      west_standings <- if(any(standings$conf == "West")) standings %>% filter(conf == "West") %>% select(any_of(c(Rank="confRank", Team="teamName", W="W", L="L", Pct="Pct", GB="GB", Strk="Strk"))) else data.frame()
      create_standings_table <- function(conf_data, title) {
        required_cols_st <- c("Rank", "Team", "W", "L", "Pct", "GB", "Strk")
        if(is.null(conf_data) || !is.data.frame(conf_data) || nrow(conf_data) == 0 || !all(required_cols_st %in% names(conf_data))) {
          return(tags$div(class="standings-conference", h5(title, class="standings-header"), tags$p(paste("Standings for", title, "unavailable."), style="color:#aaaaaa;")))
        }
        table_rows <- tryCatch({
          purrr::pmap( .l = conf_data, .f = function(Rank, Team, W, L, Pct, GB, Strk, ...) {
            tags$tr( tags$td(Rank %||% "-"), tags$td(Team %||% "N/A"), tags$td(W %||% "-"), tags$td(L %||% "-"), tags$td(if(!is.na(Pct) && is.numeric(Pct)) sprintf("%.3f", Pct) else "-"), tags$td(if(!is.na(GB) && is.numeric(GB)) sprintf("%.1f", GB) else "-"), tags$td(Strk %||% "-") )
          })
        }, error = function(e){ print(paste("Error rendering", title, "standings rows:", e$message)); list(tags$tr(tags$td(colspan="7", "Error displaying standings."))) })
        tags$div(class="standings-conference", h5(title, class="standings-header"), tags$table(class = "standings-table", tags$thead( tags$tr( tags$th("RK"), tags$th("Team"), tags$th("W"), tags$th("L"), tags$th("PCT"), tags$th("GB"), tags$th("STRK") )), tags$tbody( table_rows )))
      } # End helper
      tagList( create_standings_table(east_standings, "Eastern Conference"), tags$br(), create_standings_table(west_standings, "Western Conference") )
    }) # End renderUI standings_output
    
    # --- Helper function to render a leader list UI ---
    # (Defined in previous step - ensure it's here)
    render_leader_list <- function(rv_data_source, category_title) {
      renderUI({
        if (!is.null(rv_data_source$error)) {
          return(tags$div(class = "validation-error-message", paste("Error loading", category_title, ":", rv_data_source$error)))
        }
        if (is.null(rv_data_source$data) && is.null(rv_data_source$error)) {
          return(tags$p(paste("Loading", category_title, "..."), style="color: #aaaaaa; text-align: center; font-size: 0.9em;"))
        }
        if (is.data.frame(rv_data_source$data) && nrow(rv_data_source$data) == 0) {
          return(tags$p(paste("No", category_title, "data available."), style="color:#aaaaaa; font-size: 0.9em; margin-top: 5px; margin-bottom: 10px;")) # Added margin
        }
        if (is.data.frame(rv_data_source$data) && nrow(rv_data_source$data) > 0) {
          list_items <- tryCatch({
            purrr::pmap( .l = rv_data_source$data, .f = function(playerId, Rank, Player, Team, Value, ...) {
              headshot_url <- get_player_headshot_url(playerId)
              tags$li(class = "leader-item",
                      tags$span(class="leader-rank", Rank %||% "-"),
                      tags$img(src = headshot_url, class="leader-headshot", alt=Player %||% "Player"),
                      tags$div(class="leader-details",
                               tags$span(class="leader-name", Player %||% "N/A"),
                               tags$span(class="leader-team", Team %||% "N/A")
                      ),
                      tags$span(class="leader-stat", if(!is.na(Value) && is.numeric(Value)) sprintf("%.1f", Value) else (Value %||% "-"))
              )
            })
          }, error = function(e) {
            print(paste("Error creating leader list items for", category_title, ":", e$message))
            list(tags$li(class = "leader-item validation-error-message", style="justify-content: center;", paste("Error displaying", category_title)))
          })
          return(tagList(
            h5(category_title, class="leaders-header"), # Use h5 for sub-headers within the column
            tags$ul(class = "leader-list", list_items)
          ))
        } else {
          return(tags$div(class = "validation-error-message", paste("Invalid state for", category_title, "data.")))
        }
      }) # End renderUI
    } # End render_leader_list function
    
    # --- Render Leader Lists using Helper ---
    output$leaders_pts_output <- render_leader_list(rv_ld_pts, "Points per game")
    output$leaders_ast_output <- render_leader_list(rv_ld_ast, "Assists per game")
    output$leaders_reb_output <- render_leader_list(rv_ld_reb, "Rebounds per game")
    output$leaders_blk_output <- render_leader_list(rv_ld_blk, "Blocks per game")
    output$leaders_stl_output <- render_leader_list(rv_ld_stl, "Steals per game")
    
  }) # End moduleServer
} # End Server Function