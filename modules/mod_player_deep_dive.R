
print("--- mod_player_deep_dive.R: File Sourced (Dynamic Text Color for Stat Cards) ---")

playerDeepDiveTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style="margin-bottom: 20px; max-width: 400px;",
        selectInput(ns("selectedPlayer"), 
                    label = h4("Select Player:", style="color: var(--accent-primary);"), 
                    choices = NULL, 
                    selected = NULL 
        )
    ),
    uiOutput(ns("playerHeaderUI")), 
    card( class = "mt-4",
          card_header(bsicons::bs_icon("book-fill"), "Player Biography"),
          card_body( class = "bio-text", uiOutput(ns("playerFullBio")) )
    ),
    div(class="stats-section mt-4",
        h3(class = "stats-section-title", bsicons::bs_icon("bar-chart-line-fill"), textOutput(ns("statsTitle"))),
        div(class = "player-stats-grid-container", 
            uiOutput(ns("playerStatsLayout"))
        )
    )
  )
}


playerDeepDiveTabServer <- function(id, nba_stats_df_main, player_url_map_main, team_aesthetics_main) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print(paste0(Sys.time(), " [PLAYER_DIVE_MOD_SERVER] Instance started for '", id, "'."))
    
    observe({
      req(nba_stats_df_main)
      player_choices <- sort(unique(nba_stats_df_main$Player))
      selected_choice <- if ("LeBron James" %in% player_choices) "LeBron James" else if (length(player_choices) > 0) player_choices[1] else NULL
      updateSelectInput(session, "selectedPlayer", choices = player_choices, selected = selected_choice)
    })
    
    selected_player_url <- reactive({
      req(input$selectedPlayer); player_name_selected <- input$selectedPlayer; req(player_url_map_main)
      url_info <- player_url_map_main %>% filter(Player == player_name_selected) %>% slice(1)
      if (nrow(url_info) > 0 && !is.na(url_info$NBABioURL) && url_info$NBABioURL != "") return(url_info$NBABioURL)
      else { message(paste("No NBA.com URL for:", player_name_selected)); return(NULL) }
    })
    
    scraped_bio_cache <- reactiveValues()
    scraped_player_bio_data <- reactive({
      url <- selected_player_url(); if (is.null(url) || url == "") return(NULL)
      if (!is.null(scraped_bio_cache[[url]])) { message(paste("CACHE HIT Player Bio:", url)); return(scraped_bio_cache[[url]]) }
      message(paste("CACHE MISS Player Bio, scraping:", url))
      if(exists("scrape_nba_player_bio", mode="function")) { data <- scrape_nba_player_bio(url); scraped_bio_cache[[url]] <- data; return(data) }
      else { warning("scrape_nba_player_bio func not found."); return(NULL) }
    })
    
    selected_player_game_data <- reactive({ req(input$selectedPlayer, nba_stats_df_main); nba_stats_df_main %>% filter(Player == input$selectedPlayer) })
    aggregated_player_stats <- reactive({
      player_data <- selected_player_game_data(); if (nrow(player_data) == 0) return(NULL)
      player_data %>% summarise( GP = n(), PTS = mean(PTS, na.rm = TRUE), REB = mean(TRB, na.rm = TRUE), AST = mean(AST, na.rm = TRUE), STL = mean(STL, na.rm=TRUE), BLK = mean(BLK, na.rm=TRUE), FG_made = sum(FG, na.rm=TRUE), FGA_total = sum(FGA, na.rm=TRUE), `3P_made` = sum(`3P`, na.rm=TRUE), `3PA_total` = sum(`3PA`, na.rm=TRUE), FT_made = sum(FT, na.rm=TRUE), FTA_total = sum(FTA, na.rm=TRUE), .groups = 'drop') %>%
        mutate(`FG%` = ifelse(FGA_total > 0, FG_made / FGA_total, 0), `3P%` = ifelse(`3PA_total` > 0, `3P_made` / `3PA_total`, 0), `FT%` = ifelse(FTA_total > 0, FT_made / FTA_total, 0))
    })
    
    current_player_team_theme <- reactive({
      req(input$selectedPlayer)
      player_csv <- selected_player_game_data()
      current_team_abbr <- if(nrow(player_csv) > 0) {
        player_csv %>% arrange(desc(GameDate)) %>% slice(1) %>% pull(Tm)
      } else { "N/A" }
      
      req(team_aesthetics_main)
      team_theme_df <- team_aesthetics_main %>% 
        filter(Tm == current_team_abbr) %>% 
        slice(1)
      
      card_bg_color <- if(nrow(team_theme_df) > 0 && !is.na(team_theme_df$PrimaryColor)) {
        team_theme_df$PrimaryColor
      } else {
        "#241E33" 
      }
      
      stat_card_text_color <- if(exists("get_contrasting_text_color", mode="function")){
        get_contrasting_text_color(card_bg_color)
      } else {
        "#FFFFFF" 
      }
      
      logo_url_val <- if(nrow(team_theme_df) > 0 && !is.na(team_theme_df$LogoURL)) {
        team_theme_df$LogoURL
      } else {
        "www/nba-logo.png" 
      }
      
      list(
        background = card_bg_color, 
        stat_card_text = stat_card_text_color, 
        logo = logo_url_val,
        abbr = current_team_abbr,
    
        header_text_color = if(nrow(team_theme_df) > 0 && "TextColor" %in% names(team_theme_df) && !is.na(team_theme_df$TextColor)) team_theme_df$TextColor else "#FFFFFF"
      )
    })
    
    output$playerHeaderUI <- renderUI({ 
      req(input$selectedPlayer); bio <- scraped_player_bio_data(); stats_csv <- aggregated_player_stats(); team_theme <- current_player_team_theme()
      header_bg_color_style <- team_theme$background; header_text_color_style <- team_theme$header_text_color; logo_url <- team_theme$logo; current_team_abbr <- team_theme$abbr
      player_name_val <- input$selectedPlayer
      headshot_url_val <- if(!is.null(bio) && !is.null(bio$headshot_url) && bio$headshot_url != "") bio$headshot_url else "www/nba-logo.png"
      jersey_val <- if(!is.null(bio) && !is.null(bio$Jersey) && !is.na(bio$Jersey) && bio$Jersey != "") paste0("#", bio$Jersey) else "#??"
      position_val <- if(!is.null(bio) && !is.null(bio$Position) && !is.na(bio$Position)) bio$Position else "N/A Pos"
      team_pos_jersey_val <- paste(current_team_abbr, "|", jersey_val, "|", position_val)
      ppg_val <- if(!is.null(stats_csv) && !is.na(stats_csv$PTS)) scales::number(stats_csv$PTS, accuracy=0.1) else "N/A"
      rpg_val <- if(!is.null(stats_csv) && !is.na(stats_csv$REB)) scales::number(stats_csv$REB, accuracy=0.1) else "N/A"
      apg_val <- if(!is.null(stats_csv) && !is.na(stats_csv$AST)) scales::number(stats_csv$AST, accuracy=0.1) else "N/A"
      height_val <- if(!is.null(bio) && !is.null(bio$Height)) bio$Height else "N/A"; weight_val <- if(!is.null(bio) && !is.null(bio$Weight)) bio$Weight else "N/A"
      country_val <- if(!is.null(bio) && !is.null(bio$Country)) bio$Country else "N/A"; last_attended_val <- if(!is.null(bio) && !is.null(bio$`Last Attended`)) bio$`Last Attended` else "N/A"
      age_val <- if(!is.null(bio) && !is.null(bio$Age)) bio$Age else "N/A"; birthdate_val <- if(!is.null(bio) && !is.null(bio$Birthdate)) bio$Birthdate else "N/A"
      draft_val <- if(!is.null(bio) && !is.null(bio$Draft)) bio$Draft else "N/A"; experience_val <- if(!is.null(bio) && !is.null(bio$Experience)) bio$Experience else "N/A"
      div(class="player-header-nba", div(class="player-header-nba-background", style=paste0("background-color:",header_bg_color_style,";")),
          div(class="player-header-nba-content-wrapper", div(class="player-header-nba-top-visual", if(logo_url!="" && !is.na(logo_url)) div(class="team-logo-container",tags$img(src=logo_url,alt=paste(current_team_abbr,"Logo"))), div(class="player-name-details",style=paste0("color:",header_text_color_style,";"), p(class="team-pos-jersey",team_pos_jersey_val), h1(class="player-name-main",player_name_val))),
              div(class="player-headshot-container",tags$img(src=headshot_url_val,alt=paste(player_name_val,"Headshot"),onerror="this.onerror=null; this.src='www/nba-logo.png'; this.style.objectFit='contain';")),
              tagList(div(class="player-header-nba-bottom-bar",style=paste0("color:",header_text_color_style,";"), div(class="item stat-item",div(class="item-label","PPG"),div(class="item-value stat-value",ppg_val)), div(class="item stat-item",div(class="item-label","RPG"),div(class="item-value stat-value",rpg_val)), div(class="item stat-item",div(class="item-label","APG"),div(class="item-value stat-value",apg_val)), div(class="item bio-item",div(class="item-label","HEIGHT"),div(class="item-value",height_val)), div(class="item bio-item",div(class="item-label","WEIGHT"),div(class="item-value",weight_val)), div(class="item bio-item",div(class="item-label","COUNTRY"),div(class="item-value",country_val)), div(class="item bio-item",div(class="item-label","LAST ATTENDED"),div(class="item-value",last_attended_val))),
                      div(class="player-header-nba-bottom-bar-row2",style=paste0("color:",header_text_color_style,";"), div(class="item bio-item",div(class="item-label","AGE"),div(class="item-value",age_val)), div(class="item bio-item",div(class="item-label","BIRTHDATE"),div(class="item-value",birthdate_val)), div(class="item bio-item",div(class="item-label","DRAFT"),div(class="item-value",draft_val)), div(class="item bio-item",div(class="item-label","EXPERIENCE"),div(class="item-value",experience_val))))))
    })
    
    output$playerFullBio <- renderUI({
      bio <- scraped_player_bio_data()
      if (!is.null(bio) && !is.null(bio$FullBio) && bio$FullBio != "" && !grepl("not found|not available|failed to scrape", bio$FullBio, ignore.case = TRUE)) {
        sections <- str_split(bio$FullBio, "\n\n---\n\n")[[1]]; bio_html <- lapply(sections, function(s_text) { lapply(str_split(s_text, "\n\n")[[1]], tags$p) }) %>% unlist(recursive=FALSE) %>% tagList(); div(class="bio-text-scraped", bio_html)
      } else { p(if(!is.null(bio) && !is.null(bio$FullBio) && bio$FullBio != "") bio$FullBio else "Bio text not available.", style = "margin:0; color:#B0B0B0;") }
    })
    
    output$statsTitle <- renderText({ req(input$selectedPlayer); paste("Detailed Season Averages for", input$selectedPlayer) })
    
    output$playerStatsLayout <- renderUI({
      stats_data <- aggregated_player_stats()
      theme_colors <- current_player_team_theme() 
      
      if(is.null(stats_data) || is.na(stats_data$GP)) {
        return(p("Detailed statistics from CSV not available.", style="color: #B0B0B0; text-align: center; padding: 20px;"))
      }
      
      create_stat_card_v3 <- function(stat_item_details, card_bg_color, card_text_color) {
        display_value <- if (!is.na(stat_item_details$value) && !is.null(stat_item_details$value)) {
          stat_item_details$format_func(stat_item_details$value)
        } else { "N/A" }
        
        card_style_attr <- sprintf("background-color: %s; color: %s;", card_bg_color, card_text_color)
        

        tags$div(class = "stat-card-v3", 
                 style = card_style_attr, 
                 tags$div(class = "stat-card-v3-abbr", stat_item_details$abbr),
                 tags$hr(class = "stat-card-v3-separator"), 
                 tags$div(class = "stat-card-v3-value", display_value), 
                 tags$div(class = "stat-card-v3-name", stat_item_details$name) 
        )
      }
      
      stats_list_for_layout <- list(
        PTS = list(abbr = "PTS", name = "Points Per Game", value = stats_data$PTS, format_func = function(x) scales::number(x, accuracy = 0.1)),
        REB = list(abbr = "REB", name = "Rebounds Per Game", value = stats_data$REB, format_func = function(x) scales::number(x, accuracy = 0.1)),
        AST = list(abbr = "AST", name = "Assists Per Game", value = stats_data$AST, format_func = function(x) scales::number(x, accuracy = 0.1)),
        STL = list(abbr = "STL", name = "Steals Per Game", value = stats_data$STL, format_func = function(x) scales::number(x, accuracy = 0.1)),
        BLK = list(abbr = "BLK", name = "Blocks Per Game", value = stats_data$BLK, format_func = function(x) scales::number(x, accuracy = 0.1)),
        `FG%` = list(abbr = "FG%", name = "Field Goal %", value = stats_data$`FG%`, format_func = function(x) scales::percent(x, accuracy = 0.1)),
        `3P%` = list(abbr = "3P%", name = "3-Point %", value = stats_data$`3P%`, format_func = function(x) scales::percent(x, accuracy = 0.1)),
        `FT%` = list(abbr = "FT%", name = "Free Throw %", value = stats_data$`FT%`, format_func = function(x) scales::percent(x, accuracy = 0.1))
      )
      
      tagList(
        fluidRow(class = "stat-card-row",
                 column(width = 4, create_stat_card_v3(stats_list_for_layout$PTS, theme_colors$background, theme_colors$stat_card_text)),
                 column(width = 4, create_stat_card_v3(stats_list_for_layout$REB, theme_colors$background, theme_colors$stat_card_text)),
                 column(width = 4, create_stat_card_v3(stats_list_for_layout$AST, theme_colors$background, theme_colors$stat_card_text))
        ),
        fluidRow(class = "stat-card-row",
                 column(width = 6, create_stat_card_v3(stats_list_for_layout$STL, theme_colors$background, theme_colors$stat_card_text)),
                 column(width = 6, create_stat_card_v3(stats_list_for_layout$BLK, theme_colors$background, theme_colors$stat_card_text))
        ),
        fluidRow(class = "stat-card-row",
                 column(width = 4, create_stat_card_v3(stats_list_for_layout$`FG%`, theme_colors$background, theme_colors$stat_card_text)),
                 column(width = 4, create_stat_card_v3(stats_list_for_layout$`3P%`, theme_colors$background, theme_colors$stat_card_text)),
                 column(width = 4, create_stat_card_v3(stats_list_for_layout$`FT%`, theme_colors$background, theme_colors$stat_card_text))
        )
      )
    }) # End of playerStatsLayout
    
  }) # End of moduleServer
}