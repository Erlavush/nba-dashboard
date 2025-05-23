# modules/mod_overview.R

print("--- mod_overview.R: File Sourced (New Layout: SB/Leaders Stacked, Playoff Full Width Below) ---")

# --- Overview Tab UI Function ---
overviewTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Row 1: Latest News (Full Width) ---
    fluidRow(
      column(width = 12,
             card(
               class = "mb-3", 
               card_header(h4("Latest NBA News", class = "my-0")), 
               card_body(
                 style = "padding: 0;", 
                 shinycssloaders::withSpinner(uiOutput(ns("news_output")), type = 6, color = "#00AEEF")
               )
             )
      )
    ),
    
    tags$hr(style="border-top: 1px solid #444; margin-top: 1rem; margin-bottom: 1rem;"),
    
    # --- Row 2: Scoreboard/Leaders and Standings ---
    fluidRow(
      # Column 1: Scoreboard and Stats Leaders (Stacked)
      column(width = 4, 
             # Scoreboard Card
             card(
               class = "mb-3", # Added margin-bottom
               card_body(
                 style = "padding: 10px;",
                 div(class = "date-nav-container-scoreboard",
                     actionButton(ns("prev_day_sb"), label = icon("chevron-left"), class = "btn-sm btn-outline-light"),
                     uiOutput(ns("current_date_display_scoreboard")), 
                     actionButton(ns("next_day_sb"), label = icon("chevron-right"), class = "btn-sm btn-outline-light")
                 ),
                 div(style = "margin-top: 10px; margin-bottom: 10px;",
                     dateInput(ns("scoreboard_date_picker"), 
                               label = NULL, 
                               value = Sys.Date(), 
                               min = "1946-11-01", 
                               max = Sys.Date() + lubridate::years(1), 
                               format = "M d, yyyy", 
                               width = "100%",
                               autoclose = TRUE)
                 ),
                 div(class = "scoreboard-scroll-container", 
                     shinycssloaders::withSpinner(
                       uiOutput(ns("scoreboard_games_output")), 
                       type = 6, color = "#00AEEF"
                     )
                 )
               )
             ),
             # League Leaders Card
             card(
               card_header("League Leaders"),
               card_body(
                 # Using a single column layout within leaders for better stacking in a narrow column
                 uiOutput(ns("leaders_pts_output")),
                 uiOutput(ns("leaders_reb_output")),
                 uiOutput(ns("leaders_ast_output")),
                 uiOutput(ns("leaders_stl_output")),
                 uiOutput(ns("leaders_blk_output"))
               )
             )
      ),
      # Column 2: Standings
      column(width = 8, 
             card(
               card_header("NBA Standings"),
               card_body(
                 fluidRow(
                   column(width = 6,
                          h5("Eastern Conference", class="conference-title", style="text-align:center; color: #E0E0E0; margin-bottom: 10px;"),
                          shinycssloaders::withSpinner(
                            uiOutput(ns("east_standings_output_ui")), 
                            type = 6, color = "#00AEEF")
                   ),
                   column(width = 6,
                          h5("Western Conference", class="conference-title", style="text-align:center; color: #E0E0E0; margin-bottom: 10px;"),
                          shinycssloaders::withSpinner(
                            uiOutput(ns("west_standings_output_ui")), 
                            type = 6, color = "#00AEEF")
                   )
                 )
               )
             )
      )
    ),
    
    tags$hr(style="border-top: 1px solid #444; margin-top: 1rem; margin-bottom: 1rem;"),
    
    # --- Row 3: Playoff Bracket (Full Width) ---
    fluidRow(
      column(width = 12, 
             card(
               card_header("Playoff Bracket"),
               card_body(
                 style = "padding: 5px; overflow-x: auto;", 
                 shinycssloaders::withSpinner( 
                   uiOutput(ns("static_playoff_bracket_output_ui")), 
                   type = 6, color = "#00AEEF"
                 )
               )
             )
      )
    ) 
  ) 
}


# --- Overview Tab Server Function ---
overviewTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns 
    print(paste0(Sys.time(), " [OV_MOD_SERVER] Instance started for '", id, "'."))
    
    # === INITIALIZE ALL REACTIVE VALUES ===
    rv_news <- reactiveValues(data = NULL, error = NULL, status = "initial_load", last_fetch_time = NULL)
    rv_standings <- reactiveValues(east_data = NULL, west_data = NULL, error = NULL, status = "initial_load", last_fetch_time = NULL)
    rv_scoreboard <- reactiveValues(
      data = NULL, 
      error = NULL, 
      status = "initial_load", 
      current_date = Sys.Date(),
      current_display_date_from_page = format(Sys.Date(), "%B %d, %Y")
    )
    rv_leaders <- reactiveValues(data = NULL, error = NULL, status = "initial_load")
    rv_playoffs <- reactiveValues(data = NULL, error = NULL, status = "initial_load") 
    
    # === News Fetching Logic ===
    observe({
      print(paste0(Sys.time(), " [OV_MOD_NEWS_FETCH_ONCE] Attempting initial news fetch."))
      current_time_for_fetch_news <- Sys.time() 
      if(exists("fetch_nba_dot_com_news", mode="function")){
        news_res <- tryCatch({ fetch_nba_dot_com_news(num_headlines = 10) }, 
                             error = function(e) { print(paste0(Sys.time(), " [OV_MOD_NEWS_ERROR] ", e$message)); NULL })
        if(!is.null(news_res) && is.data.frame(news_res) && nrow(news_res) > 0){
          rv_news$data <- news_res; rv_news$error <- NULL; rv_news$status <- "success"
          # print(paste0(Sys.time(), " [OV_MOD_NEWS_SUCCESS] News fetch. Rows: ", nrow(news_res)))
        } else {
          rv_news$data <- NULL; rv_news$error <- news_res$error %||% "Failed to fetch news or no items."; rv_news$status <- "error"
          print(paste0(Sys.time(), " [OV_MOD_NEWS_FAIL] ", rv_news$error))
        }
      } else { 
        rv_news$data <- NULL; rv_news$error <- "News func not available."; rv_news$status <- "error"
        print(paste0(Sys.time(), " [OV_MOD_NEWS_CRITICAL] ", rv_news$error))
      }
      rv_news$last_fetch_time <- current_time_for_fetch_news 
    }) 
    output$news_output <- renderUI({
      req(rv_news$last_fetch_time, cancelOutput = TRUE) 
      if (rv_news$status == "initial_load") return(tags$div(class="news-scroll-container loading-msg", "Loading news..."))
      if (!is.null(rv_news$error)) return(tags$div(class="news-scroll-container", div(class="error-msg validation-error-message", rv_news$error)))
      if (is.null(rv_news$data) || nrow(rv_news$data) == 0) return(tags$div(class="news-scroll-container", div(class="error-msg validation-error-message", "No news.")))
      news_cards_html <- purrr::pmap(rv_news$data, function(headline, link, image_url, ...) {
        img_onerror_js <- sprintf("this.onerror=null; this.src='%s'; this.style.objectFit='contain';", FALLBACK_IMAGE_URL)
        tags$a(href = link %||% "#", target = "_blank", class = "news-card", rel = "noopener noreferrer",
               tags$img(src = image_url %||% FALLBACK_IMAGE_URL, class = "news-image", alt = headline %||% "NBA News", loading = "lazy", onerror = img_onerror_js),
               tags$div(class = "news-headline", headline %||% "Untitled Article"))})
      tags$div(class = "news-scroll-container", news_cards_html)
    })
    
    # === Standings Fetching and Rendering Logic ===
    observe({
      print(paste0(Sys.time(), " [OV_MOD_STANDINGS_FETCH_ONCE] Attempting initial standings fetch."))
      current_time_for_fetch_standings <- Sys.time()
      if(exists("scrape_standings_data", mode="function")){
        standings_result <- tryCatch({ scrape_standings_data() }, 
                                     error = function(e) { print(paste0(Sys.time(), " [OV_MOD_STANDINGS_ERROR] ", e$message)); list(data=list(), error=e$message)})
        if(!is.null(standings_result$error) && standings_result$error != ""){
          rv_standings$east_data <- NULL; rv_standings$west_data <- NULL; rv_standings$error <- standings_result$error; rv_standings$status <- "error"
        } else if (!is.null(standings_result$data)) {
          rv_standings$east_data <- standings_result$data$east_standings; rv_standings$west_data <- standings_result$data$west_standings; rv_standings$error <- NULL
          east_valid <- !is.null(rv_standings$east_data) && nrow(rv_standings$east_data)>0; west_valid <- !is.null(rv_standings$west_data) && nrow(rv_standings$west_data)>0
          if (east_valid && west_valid) rv_standings$status <- "success"
          else if (east_valid || west_valid) { rv_standings$status <- "partial_success"; rv_standings$error <- "One conference standings missing." }
          else { rv_standings$status <- "error"; rv_standings$error <- "Standings data empty." }
        } else { rv_standings$status <- "error"; rv_standings$error <- "Standings scraper returned NULL." }
      } else { rv_standings$error <- "Standings func not available."; rv_standings$status <- "error" }
      rv_standings$last_fetch_time <- current_time_for_fetch_standings
    })
    output$east_standings_output_ui <- renderUI({
      req(rv_standings$last_fetch_time, cancelOutput = TRUE)
      if (rv_standings$status == "initial_load") return(tags$p("Loading East standings...", class="text-muted"))
      if (rv_standings$status == "error" && !grepl("Western", rv_standings$error %||% "", ignore.case=T)) return(tags$p(class="validation-error-message", rv_standings$error))
      if (is.null(rv_standings$east_data) || nrow(rv_standings$east_data) == 0) return(tags$p("No East standings.", class="text-muted"))
      table_header <- tags$thead(tags$tr(tags$th("Team",style="text-align:left;"), tags$th("W"), tags$th("L"), tags$th("W/L%"), tags$th("GB")))
      table_body <- tags$tbody(purrr::pmap(rv_standings$east_data, function(Logo_Path,Team,W,L,`W/L%`,GB,...) tags$tr(tags$td(style="text-align:left;vertical-align:middle;", tags$img(src=Logo_Path,class="team-logo-inline",onerror="this.src='team_logos/NBA.png';"),tags$span(Team,style="margin-left:8px;")), tags$td(W), tags$td(L), tags$td(sprintf("%.3f",`W/L%`)), tags$td(sprintf("%.1f",GB)))))
      tags$div(class="table-responsive", tags$table(class="table table-sm standings-table", table_header, table_body))
    })
    output$west_standings_output_ui <- renderUI({
      req(rv_standings$last_fetch_time, cancelOutput = TRUE)
      if (rv_standings$status == "initial_load") return(tags$p("Loading West standings...", class="text-muted"))
      if (rv_standings$status == "error" && !grepl("Eastern", rv_standings$error %||% "", ignore.case=T)) return(tags$p(class="validation-error-message", rv_standings$error))
      if (is.null(rv_standings$west_data) || nrow(rv_standings$west_data) == 0) return(tags$p("No West standings.", class="text-muted"))
      table_header <- tags$thead(tags$tr(tags$th("Team",style="text-align:left;"), tags$th("W"), tags$th("L"), tags$th("W/L%"), tags$th("GB")))
      table_body <- tags$tbody(purrr::pmap(rv_standings$west_data, function(Logo_Path,Team,W,L,`W/L%`,GB,...) tags$tr(tags$td(style="text-align:left;vertical-align:middle;", tags$img(src=Logo_Path,class="team-logo-inline",onerror="this.src='team_logos/NBA.png';"),tags$span(Team,style="margin-left:8px;")), tags$td(W), tags$td(L), tags$td(sprintf("%.3f",`W/L%`)), tags$td(sprintf("%.1f",GB)))))
      tags$div(class="table-responsive", tags$table(class="table table-sm standings-table", table_header, table_body))
    })
    
    # === Scoreboard Logic ===
    observeEvent(input$prev_day_sb, { updateDateInput(session, "scoreboard_date_picker", value = rv_scoreboard$current_date - lubridate::days(1)) })
    observeEvent(input$next_day_sb, { updateDateInput(session, "scoreboard_date_picker", value = rv_scoreboard$current_date + lubridate::days(1)) })
    observeEvent(input$scoreboard_date_picker, {
      req(input$scoreboard_date_picker); if (input$scoreboard_date_picker != rv_scoreboard$current_date) { 
        rv_scoreboard$current_date <- input$scoreboard_date_picker; rv_scoreboard$status <- "loading_new_date" }
    }, ignoreInit = TRUE) 
    output$current_date_display_scoreboard <- renderUI({ tags$span(rv_scoreboard$current_display_date_from_page) })
    observe({ 
      req(rv_scoreboard$current_date, rv_scoreboard$status %in% c("initial_load", "loading_new_date"))
      if(input$scoreboard_date_picker != rv_scoreboard$current_date && rv_scoreboard$status != "initial_load"){ updateDateInput(session, "scoreboard_date_picker", value = rv_scoreboard$current_date) }
      # print(paste0(Sys.time(), " [OV_MOD_SCOREBOARD_FETCH] Fetching for: ", rv_scoreboard$current_date))
      if(exists("scrape_scoreboard_data", mode="function")){
        res <- tryCatch({ scrape_scoreboard_data(rv_scoreboard$current_date) }, error=function(e){ list(data=NULL, error=e$message, current_date_display=format(rv_scoreboard$current_date,"%B %d, %Y"))})
        rv_scoreboard$current_display_date_from_page <- res$current_date_display %||% format(rv_scoreboard$current_date,"%B %d, %Y")
        if(!is.null(res$error)){ rv_scoreboard$data <- NULL; rv_scoreboard$error <- res$error; rv_scoreboard$status <- "error"
        } else { rv_scoreboard$data <- res$data; rv_scoreboard$error <- NULL; rv_scoreboard$status <- "success"}
      } else { rv_scoreboard$data <- NULL; rv_scoreboard$error <- "Scoreboard func missing."; rv_scoreboard$status <- "error" }
    })
    output$scoreboard_games_output <- renderUI({
      req(rv_scoreboard$status != "initial_load" && rv_scoreboard$status != "loading_new_date", cancelOutput = TRUE)
      if (!is.null(rv_scoreboard$error) && (stringr::str_detect(tolower(rv_scoreboard$error %||% ""), "no games scheduled") || stringr::str_detect(tolower(rv_scoreboard$error %||% ""), "scoreboard is empty")) ) return(tags$div(class="text-center p-3 text-muted", rv_scoreboard$error))
      if (rv_scoreboard$status == "error" && !is.null(rv_scoreboard$error)) return(tags$div(class="text-center p-3 validation-error-message", rv_scoreboard$error))
      if (is.null(rv_scoreboard$data) || nrow(rv_scoreboard$data) == 0) return(tags$div(class="text-center p-3 text-muted", "No scoreboard data for this date."))
      game_cards_html <- purrr::pmap(rv_scoreboard$data, function(team_a_name, team_a_logo, team_a_score, team_a_is_winner, team_a_record, team_b_name, team_b_logo, team_b_score, team_b_is_winner, team_b_record, game_status, ...) {
        tags$div(class="sb-game-card", div(class="sb-top-bar",tags$span(class="sb-game-status",game_status %||% "TBD")),
                 div(class="sb-main-content",
                     div(class=paste("sb-team sb-away-team",if(isTRUE(team_a_is_winner))"sb-team-winner"), tags$img(src=team_a_logo,class="sb-team-logo",alt=team_a_name,onerror="this.src='team_logos/NBA.png';"),div(class="sb-team-details",tags$span(class="sb-team-name",team_a_name),tags$span(class="sb-team-record",team_a_record%||%""))),
                     div(class="sb-score-area",tags$span(class="sb-score-value",if(!is.na(team_a_score))team_a_score else"-"),tags$span(class="sb-score-separator",if(!is.na(team_a_score)||!is.na(team_b_score))"vs" else""),tags$span(class="sb-score-value",if(!is.na(team_b_score))team_b_score else"-")),
                     div(class=paste("sb-team sb-home-team",if(isTRUE(team_b_is_winner))"sb-team-winner"), div(class="sb-team-details",tags$span(class="sb-team-name",team_b_name),tags$span(class="sb-team-record",team_b_record%||%"")),tags$img(src=team_b_logo,class="sb-team-logo",alt=team_b_name,onerror="this.src='team_logos/NBA.png';"))))})
      do.call(tagList, game_cards_html)
    })
    
    # === League Leaders Logic (Static) ===
    observe({ 
      print(paste0(Sys.time(), " [OV_MOD_LEADERS_LOAD_ONCE] Attempting to load static league leaders."))
      req(rv_leaders$status == "initial_load") 
      if(exists("scrape_league_leaders_data", mode="function")){
        leaders_result <- tryCatch({ scrape_league_leaders_data() }, 
                                   error = function(e) { print(paste0(Sys.time(), " [OV_MOD_LEADERS_ERROR] ", e$message)); list(data=NULL, error=e$message)})
        if(!is.null(leaders_result$error)){ rv_leaders$data <- NULL; rv_leaders$error <- leaders_result$error; rv_leaders$status <- "error"
        } else if (!is.null(leaders_result$data)) { rv_leaders$data <- leaders_result$data; rv_leaders$error <- NULL; rv_leaders$status <- "success"
        } else { rv_leaders$data <- NULL; rv_leaders$error <- "Unknown issue loading static leaders."; rv_leaders$status <- "error" }
      } else { rv_leaders$data <- NULL; rv_leaders$error <- "Static leaders func not available."; rv_leaders$status <- "error" }
    })
    render_leader_list_ui <- function(stat_df, stat_title) {
      if (is.null(stat_df) || nrow(stat_df) == 0) return(tags$div(class="leader-list-container", tags$h5(class="leaders-header", stat_title), tags$p("No data.", class="text-muted p-2")))
      req_cols <- c("Rank","Player","Team","Stat"); if(!all(req_cols %in% names(stat_df))) return(tags$p("Leader data structure error.",class="validation-error-message"))
      items <- purrr::pmap(head(stat_df,5), function(Rank,Player,Team,Stat,...) tags$li(class="leader-item leader-item-no-image", tags$span(class="leader-rank",Rank%||%"-"), div(class="leader-details",tags$span(class="leader-name",Player%||%"N/A"),tags$span(class="leader-team",Team%||%"N/A")), tags$span(class="leader-stat",if(!is.na(Stat))sprintf("%.1f",Stat)else"-")))
      tags$div(class="leader-list-container mb-3", tags$h5(class="leaders-header",stat_title), tags$ul(class="leader-list",items))
    }
    output$leaders_pts_output <- renderUI({ req(rv_leaders$status!="initial_load"); if(rv_leaders$status=="error"&&is.null(rv_leaders$data$AST)) return(tags$p(class="val-err",rv_leaders$error)); render_leader_list_ui(rv_leaders$data$PTS,"Points")})
    output$leaders_ast_output <- renderUI({ req(rv_leaders$status!="initial_load"); if(rv_leaders$status=="error"&&is.null(rv_leaders$data$PTS)) return(NULL); render_leader_list_ui(rv_leaders$data$AST,"Assists")})
    output$leaders_reb_output <- renderUI({ req(rv_leaders$status!="initial_load"); if(rv_leaders$status=="error"&&is.null(rv_leaders$data$PTS)) return(NULL); render_leader_list_ui(rv_leaders$data$REB,"Rebounds")})
    output$leaders_blk_output <- renderUI({ req(rv_leaders$status!="initial_load"); if(rv_leaders$status=="error"&&is.null(rv_leaders$data$PTS)) return(NULL); render_leader_list_ui(rv_leaders$data$BLK,"Blocks")})
    output$leaders_stl_output <- renderUI({ req(rv_leaders$status!="initial_load"); if(rv_leaders$status=="error"&&is.null(rv_leaders$data$PTS)) return(NULL); render_leader_list_ui(rv_leaders$data$STL,"Steals")})
    
    # === Static Playoff Bracket Logic ===
    observe({ 
      print(paste0(Sys.time(), " [OV_MOD_PLAYOFFS_STATIC_LOAD] Attempting static playoff bracket load."))
      req(rv_playoffs$status == "initial_load") 
      
      playoff_result <- tryCatch({ 
        scrape_playoff_bracket_data() 
      }, error = function(e) { 
        print(paste0(Sys.time(), " [OV_MOD_PLAYOFFS_STATIC_ERROR] Error calling scrape_playoff_bracket_data: ", e$message))
        list(data = NULL, error = paste("Internal error fetching static playoff data:", e$message))
      })
      
      if (is.null(playoff_result)) {
        rv_playoffs$data <- NULL; rv_playoffs$error <- "Static playoff loader failed to return a result."; rv_playoffs$status <- "error"
      } else if (!is.null(playoff_result$error) && playoff_result$error != "") {
        rv_playoffs$data <- NULL; rv_playoffs$error <- playoff_result$error; rv_playoffs$status <- "error"
      } else if (!is.null(playoff_result$data) && is.data.frame(playoff_result$data) && nrow(playoff_result$data) > 0) {
        rv_playoffs$data <- playoff_result$data; rv_playoffs$error <- NULL; rv_playoffs$status <- "success"
      } else { 
        rv_playoffs$data <- NULL; rv_playoffs$error <- playoff_result$error %||% "No static playoff data returned."; 
        rv_playoffs$status <- if(!is.null(playoff_result$error) && playoff_result$error != "") "error" else "empty"
      }
    })
    
    # Helper function to render a single matchup
    render_matchup_ui <- function(series_data) {
      team1_winner_class <- ""
      team2_winner_class <- ""
      
      if (isTRUE(series_data$team1_is_winner_visual)) {
        team1_winner_class <- "playoff-team-winner"
      } else if (isTRUE(series_data$team2_is_winner_visual)) { 
        team2_winner_class <- "playoff-team-winner"
      }
      
      tags$div(class="playoff-matchup",
               div(class=paste("playoff-team", team1_winner_class),
                   tags$img(src=series_data$team1_logo, class="playoff-team-logo", alt=series_data$team1_name, onerror="this.src='team_logos/NBA.png';"),
                   tags$span(class="playoff-team-seed", series_data$team1_seed %||% ""),
                   tags$span(class="playoff-team-name", series_data$team1_name %||% "TBD")
               ),
               div(class=paste("playoff-team", team2_winner_class),
                   tags$img(src=series_data$team2_logo, class="playoff-team-logo", alt=series_data$team2_name, onerror="this.src='team_logos/NBA.png';"),
                   tags$span(class="playoff-team-seed", series_data$team2_seed %||% ""),
                   tags$span(class="playoff-team-name", series_data$team2_name %||% "TBD")
               ),
               div(class="playoff-series-info", 
                   series_data$series_score_text %||% "Details TBD",
                   if (!is.na(series_data$game_time_info) && 
                       series_data$game_time_info != "Series Ended" && 
                       !stringr::str_detect(series_data$series_score_text %||% "", "WINS|ENDED") && 
                       !stringr::str_detect(series_data$game_time_info %||% "", "WINS|ENDED") ) { 
                     tags$div(class="playoff-game-time", series_data$game_time_info)
                   }
               )
      )
    }
    
    # Helper function to render a column of matchups for a specific round and conference
    render_round_column_ui <- function(conference_filter, round_numeric_filter, data, column_title = NULL) {
      round_data <- data %>% 
        dplyr::filter(conference == conference_filter, round_numeric == round_numeric_filter) %>%
        dplyr::arrange(matchup_id_in_round) 
      
      if (nrow(round_data) == 0) return(NULL)
      
      matchup_uis <- purrr::map(1:nrow(round_data), function(i) { # Ensure this line doesn't have 'ks'
        render_matchup_ui(round_data[i, ])
      })
      
      column_content <- list()
      
      if (round_numeric_filter == 2) { 
        column_content[[length(column_content) + 1]] <- div(class="playoff-spacer", style="height: 100px;")
      }
      if (round_numeric_filter == 3) { 
        column_content[[length(column_content) + 1]] <- div(class="playoff-spacer", style="height: 220px;")
      }
      if (round_numeric_filter == 4 && conference_filter == "Finals") { 
        column_content[[length(column_content) + 1]] <- div(class="playoff-spacer", style="height: 100px;")
      }
      
      column_content <- c(column_content, matchup_uis)
      
      tags$div(class = paste("playoff-grid-column-content", tolower(gsub(" ", "-", conference_filter)), paste0("r",round_numeric_filter)),
               if(!is.null(column_title)) tags$h6(class="playoff-column-header", column_title),
               do.call(tagList, column_content)
      )
    }
    
    output$static_playoff_bracket_output_ui <- renderUI({
      req(rv_playoffs$status)

      if(rv_playoffs$status == "initial_load") return(tags$p("Loading playoff bracket...", class="text-muted text-center p-3"))
      if(rv_playoffs$status == "error") return(tags$p(class="validation-error-message text-center p-3", rv_playoffs$error %||% "Error loading playoff data."))
      if(rv_playoffs$status == "empty" || is.null(rv_playoffs$data) || nrow(rv_playoffs$data) == 0) return(tags$p(rv_playoffs$error %||% "Playoff data unavailable.", class="text-muted text-center p-3"))
      
      playoff_data <- rv_playoffs$data
      
      tagList(
        tags$div(class = "playoff-bracket-outer-wrapper", 
                 tags$div(class = "playoff-bracket-grid-container",
                          tags$div(class = "playoff-grid-column west-r1-col",
                                   render_round_column_ui("West", 1, playoff_data, "West First Round")
                          ),
                          tags$div(class = "playoff-grid-column west-r2-col",
                                   render_round_column_ui("West", 2, playoff_data, "West Semifinals")
                          ),
                          tags$div(class = "playoff-grid-column west-r3-col",
                                   render_round_column_ui("West", 3, playoff_data, "West Finals")
                          ),
                          tags$div(class = "playoff-grid-column nba-finals-col",
                                   tags$div(class="finals-logo-wrapper", tags$img(src="nba-logo.png", class="finals-main-logo"), tags$h6("NBA Finals", class="playoff-column-header finals-header")),
                                   render_round_column_ui("Finals", 4, playoff_data) 
                          ),
                          tags$div(class = "playoff-grid-column east-r3-col",
                                   render_round_column_ui("East", 3, playoff_data, "East Finals")
                          ),
                          tags$div(class = "playoff-grid-column east-r2-col",
                                   render_round_column_ui("East", 2, playoff_data, "East Semifinals")
                          ),
                          tags$div(class = "playoff-grid-column east-r1-col",
                                   render_round_column_ui("East", 1, playoff_data, "East First Round")
                          )
                 )
        )
      )
    })
    
    output$current_date_display <- renderUI({NULL}) 
    output$east_standings_table_actual <- NULL 
    output$west_standings_table_actual <- NULL
    output$scraped_bracket_output <- NULL 
    
    print(paste0(Sys.time(), " [OV_MOD_SERVER] Server logic for '", id, "' fully defined for new static bracket layout."))
  }) 
}