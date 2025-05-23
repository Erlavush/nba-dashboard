# modules/mod_player_comparison.R

print("--- mod_player_comparison.R: File Sourced ---")

# UI Function for Player Comparison Tab
playerComparisonTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Player and Team Selection Area
    fluidRow(
      column(width = 6, class = "player-selection-block",
             h4("Player 1 Selection", style="margin-top: 5px; margin-bottom: 10px; text-align: center;"),
             fluidRow(
               column(width = 10, offset = 1,
                      selectInput(ns("team1_filter"), "Filter by Team 1 (Optional):", choices = c("All Teams"), 
                                  selected = "All Teams", width = "100%", selectize = TRUE)
               )
             ),
             fluidRow(
               column(width = 10, offset = 1,
                      selectInput(ns("player1"), "Select Player 1:", choices = NULL,
                                  width = "100%", selectize = TRUE)
               )
             )
      ),
      column(width = 6, class = "player-selection-block",
             h4("Player 2 Selection", style="margin-top: 5px; margin-bottom: 10px; text-align: center;"),
             fluidRow(
               column(width = 10, offset = 1,
                      selectInput(ns("team2_filter"), "Filter by Team 2 (Optional):", choices = c("All Teams"), 
                                  selected = "All Teams", width = "100%", selectize = TRUE)
               )
             ),
             fluidRow(
               column(width = 10, offset = 1,
                      selectInput(ns("player2"), "Select Player 2:", choices = NULL,
                                  width = "100%", selectize = TRUE)
               )
             )
      )
    ),
    hr(style="margin-top: 5px; margin-bottom: 15px; border-top: 1px solid #444;"),
    
    # Charts and Tables
    fluidRow(
      column(width = 6,
             bslib::card(
               bslib::card_header("Average Stats Radar Chart"),
               bslib::card_body(
                 plotOutput(ns("radarPlot"), height = "450px")
               )
             )
      ),
      column(width = 6,
             bslib::card(
               bslib::card_header("Selected Average Stats Bar Chart"),
               bslib::card_body(
                 plotOutput(ns("barChart"), height = "450px")
               )
             )
      )
    ),
    fluidRow(
      column(width = 6,
             bslib::card(
               bslib::card_header(textOutput(ns("player1NameTable"))),
               bslib::card_body(
                 DT::DTOutput(ns("player1Table"))
               )
             )
      ),
      column(width = 6,
             bslib::card(
               bslib::card_header(textOutput(ns("player2NameTable"))),
               bslib::card_body(
                 DT::DTOutput(ns("player2Table"))
               )
             )
      )
    ),
    fluidRow(
      column(width = 12,
             bslib::card(
               bslib::card_header("Average Shooting Percentages Comparison"),
               bslib::card_body(
                 plotOutput(ns("shootingPlot"), height = "400px")
               )
             )
      )
    )
  )
}

# Server Function for Player Comparison Tab
playerComparisonTabServer <- function(id, nba_stats_df_main) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print(paste0(Sys.time(), " [PLAYER_COMPARISON_MOD_SERVER] Instance started for '", id, "'."))
    
    # --- Data Preparation 
    aggregated_data <- reactive({
      req(nba_stats_df_main)
      

      current_raw_nba_data <- nba_stats_df_main
      

      numeric_cols_for_summing <- c("MP", "FG", "FGA", "3P", "3PA", "FT", "FTA",
                                    "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")
      existing_numeric_cols <- intersect(numeric_cols_for_summing, colnames(current_raw_nba_data))
      
      critical_for_aggregation <- c("Player", "FG", "FGA", "3P", "3PA", "FT", "FTA", "Tm", existing_numeric_cols)
      missing_crit_agg <- setdiff(unique(critical_for_aggregation), colnames(current_raw_nba_data))
      if(length(missing_crit_agg) > 0){
        showNotification(paste("Player Comparison: Critical columns for aggregation missing:", paste(missing_crit_agg, collapse=", ")), type="error", duration=NULL)
        return(NULL) 
      }
      
      processed_data <- current_raw_nba_data %>%
        filter(!is.na(Player) & Player != "") %>%
        group_by(Player) %>%
        summarise(
          G = n(),
          across(all_of(existing_numeric_cols), ~mean(.x, na.rm = TRUE)),
          `FG.` = ifelse(sum(FGA, na.rm = TRUE) == 0, 0, sum(FG, na.rm = TRUE) / sum(FGA, na.rm = TRUE)),
          `3P.` = ifelse(sum(`3PA`, na.rm = TRUE) == 0, 0, sum(`3P`, na.rm = TRUE) / sum(`3PA`, na.rm = TRUE)), 
          `FT.` = ifelse(sum(FTA, na.rm = TRUE) == 0, 0, sum(FT, na.rm = TRUE) / sum(FTA, na.rm = TRUE)),
          Tm = first(Tm[!is.na(Tm) & Tm != ""]) 
        ) %>%
        ungroup() %>%
        mutate(
          `FG.` = ifelse(is.nan(`FG.`), 0, `FG.`),
          `3P.` = ifelse(is.nan(`3P.`), 0, `3P.`),
          `FT.` = ifelse(is.nan(`FT.`), 0, `FT.`)
        )
      
      final_cols_aggregated <- c("Player", "Tm", "G", "MP", "PTS", "TRB", "AST", "STL", "BLK",
                                 "FG.", "3P.", "FT.", "TOV", "PF")
      final_cols_to_select <- intersect(final_cols_aggregated, colnames(processed_data))
      processed_data <- processed_data %>% select(all_of(final_cols_to_select))
      
      stats_to_clean_na <- c("PTS", "TRB", "AST", "STL", "BLK", "FG.", "3P.", "FT.")
      cols_to_clean_na_existing <- intersect(stats_to_clean_na, colnames(processed_data))
      if(length(cols_to_clean_na_existing) > 0) {
        processed_data <- processed_data %>%
          mutate(across(all_of(cols_to_clean_na_existing), ~ifelse(is.na(.), 0, .)))
      }
      
      if (nrow(processed_data) < 2) {
        showNotification("Player Comparison: Not enough unique players after aggregation (need at least 2).", type="warning", duration=10)
        return(NULL)
      }
      return(processed_data)
    })
    
    # Reactive values for player choices based on team filters
    observe({
      data_agg <- aggregated_data()
      req(data_agg)
      
      all_teams_in_data <- sort(unique(data_agg$Tm))
      team_choices_select <- c("All Teams", all_teams_in_data)
      
      updateSelectInput(session, "team1_filter", choices = team_choices_select, selected = isolate(input$team1_filter) %||% "All Teams")
      updateSelectInput(session, "team2_filter", choices = team_choices_select, selected = isolate(input$team2_filter) %||% "All Teams")
    })
    
    player_choices_all_dynamic <- reactive({
      req(aggregated_data())
      sort(unique(aggregated_data()$Player))
    })
    
    player1_dynamic_choices <- reactive({
      data_agg <- aggregated_data(); req(data_agg)
      player_list_all <- player_choices_all_dynamic()
      team_selected <- input$team1_filter
      
      if (is.null(team_selected) || team_selected == "All Teams") {
        player_list_all
      } else {
        data_agg %>%
          filter(Tm == team_selected) %>%
          pull(Player) %>%
          sort()
      }
    }) %>% debounce(250)
    
    player2_dynamic_choices <- reactive({
      data_agg <- aggregated_data(); req(data_agg)
      player_list_all <- player_choices_all_dynamic()
      team_selected <- input$team2_filter
      p1_current <- input$player1
      
      base_choices <- if (is.null(team_selected) || team_selected == "All Teams") {
        player_list_all
      } else {
        data_agg %>%
          filter(Tm == team_selected) %>%
          pull(Player) %>%
          sort()
      }
      
      # Ensure player 2 is different from player 1
      if (!is.null(p1_current) && nzchar(p1_current)) {
        setdiff(base_choices, p1_current)
      } else {
        base_choices
      }
    }) %>% debounce(250)
    
    observe({
      new_choices_p1 <- player1_dynamic_choices()
      current_selection_p1 <- isolate(input$player1)
      
      selected_val_p1 <- if (!is.null(current_selection_p1) && nzchar(current_selection_p1) && current_selection_p1 %in% new_choices_p1) {
        current_selection_p1
      } else {
        if(length(new_choices_p1) > 0) new_choices_p1[1] else NULL
      }
      updateSelectInput(session, "player1", choices = new_choices_p1, selected = selected_val_p1)
    })
    
    observe({
      new_choices_p2 <- player2_dynamic_choices()
      current_selection_p2 <- isolate(input$player2)
      
      selected_val_p2 <- if (!is.null(current_selection_p2) && nzchar(current_selection_p2) && current_selection_p2 %in% new_choices_p2) {
        current_selection_p2
      } else {
        p1_current <- isolate(input$player1)
        available_for_p2 <- if (!is.null(p1_current) && nzchar(p1_current)) setdiff(new_choices_p2, p1_current) else new_choices_p2
        if(length(available_for_p2) > 0) available_for_p2[1] else if(length(new_choices_p2) > 0) new_choices_p2[1] else NULL
      }
      updateSelectInput(session, "player2", choices = new_choices_p2, selected = selected_val_p2)
    })
    
    player1_avg_data <- reactive({
      data_agg <- aggregated_data(); req(data_agg)
      req(input$player1, nzchar(input$player1))
      data_agg %>% filter(Player == input$player1) %>% head(1)
    })
    
    player2_avg_data <- reactive({
      data_agg <- aggregated_data(); req(data_agg)
      req(input$player2, nzchar(input$player2))
      req(input$player1, nzchar(input$player1)) 
      req(input$player2 != input$player1) 
      data_agg %>% filter(Player == input$player2) %>% head(1)
    })
    
    radar_stat_names <- c("PTS", "TRB", "AST", "STL", "BLK")
    bar_chart_stats <- c("PTS", "TRB", "AST", "FG.", "3P.") 
    
    output$radarPlot <- renderPlot({
      p1_data <- player1_avg_data()
      p2_data <- player2_avg_data()
      data_agg <- aggregated_data()
      req(nrow(p1_data) > 0, nrow(p2_data) > 0, data_agg)
      
      p1_stats <- p1_data %>% select(all_of(radar_stat_names))
      p2_stats <- p2_data %>% select(all_of(radar_stat_names))
      
      valid_radar_stats_for_max <- intersect(radar_stat_names, colnames(data_agg))
      if(length(valid_radar_stats_for_max) != length(radar_stat_names)){
        warning("Not all radar_stat_names found in aggregated_data for max_vals.")
        return(NULL)
      }
      
      max_vals <- sapply(data_agg[valid_radar_stats_for_max], function(x) quantile(x, 0.99, na.rm = TRUE))
      min_vals <- rep(0, length(valid_radar_stats_for_max))
      
      for(i in seq_along(max_vals)){
        if(is.na(max_vals[i])) max_vals[i] <- 1
        if(is.na(min_vals[i])) min_vals[i] <- 0
        if(max_vals[i] <= min_vals[i]) max_vals[i] <- min_vals[i] + max(1, 0.1 * abs(min_vals[i]), na.rm=TRUE)
      }
      
      df_p1_stats <- as.data.frame(p1_stats[, valid_radar_stats_for_max, drop = FALSE])
      df_p2_stats <- as.data.frame(p2_stats[, valid_radar_stats_for_max, drop = FALSE])
      
      data_for_radar <- rbind(max_vals, min_vals, df_p1_stats, df_p2_stats)
      colnames(data_for_radar) <- valid_radar_stats_for_max
      
      if(nrow(data_for_radar) < 4 || any(is.na(data_for_radar[3:4,])) || ncol(data_for_radar) == 0) {
        plot(NULL, xlim=c(0,1), ylim=c(0,1), ann=FALSE, xaxt='n', yaxt='n', bg = "transparent")
        text(0.5, 0.5, "Data missing or insufficient for radar chart.", cex=1.2, col="grey")
        return()
      }
      
      colors_border <- c("#00AEEF", "#F4008A") 
      colors_in <- scales::alpha(colors_border, 0.4)
      
      par(mar = c(1, 1, 1, 1), bg = "transparent") 
      fmsb::radarchart(data_for_radar,
                       axistype = 1, pcol = colors_border, pfcol = colors_in, plwd = 2, plty = 1,
                       cglcol = "#4A406A", cglty = 1, axislabcol = "#B0B0B0", caxislabels = seq(0, max(max_vals, na.rm=TRUE)*1.1, length.out = 5) %>% round(1),
                       cglwd = 0.8, vlcex = 0.9, palcex = 1, calcex = 1)
      
      legend_labels <- c(input$player1, input$player2)
      legend_labels <- sapply(legend_labels, function(name) {
        if (nchar(name) > 15) paste0(substr(name, 1, 13), "...") else name
      })
      
      legend("topright", legend = legend_labels, bty = "n", pch = 20, col = colors_in,
             text.col = "#E0E0E0", cex = 0.9, pt.cex = 2.5)
    }, bg="transparent")
    
    output$barChart <- renderPlot({
      p1_data_bar <- player1_avg_data()
      p2_data_bar <- player2_avg_data()
      req(nrow(p1_data_bar) > 0, nrow(p2_data_bar) > 0)
      
      # Ensure bar_chart_stats exist in the data
      valid_bar_stats_p1 <- intersect(bar_chart_stats, colnames(p1_data_bar))
      valid_bar_stats_p2 <- intersect(bar_chart_stats, colnames(p2_data_bar))
      
      if(length(valid_bar_stats_p1) == 0 || length(valid_bar_stats_p2) == 0) return(NULL)
      
      p1 <- p1_data_bar %>% select(Player, all_of(valid_bar_stats_p1)) %>%
        tidyr::pivot_longer(cols = all_of(valid_bar_stats_p1), names_to = "Stat", values_to = "Value")
      p2 <- p2_data_bar %>% select(Player, all_of(valid_bar_stats_p2)) %>%
        tidyr::pivot_longer(cols = all_of(valid_bar_stats_p2), names_to = "Stat", values_to = "Value")
      
      combined_data <- rbind(p1, p2)
      combined_data$Stat <- factor(combined_data$Stat, levels = bar_chart_stats) 
      
      ggplot(combined_data, aes(x = Stat, y = Value, fill = Player)) +
        geom_col(position = position_dodge(width = 0.9), color="#100E17", linewidth=0.3) +
        labs(title = paste("Comparison:", input$player1, "vs", input$player2),
             x = "Statistic", y = "Value") +
        theme_minimal(base_size = 14) +
        scale_fill_manual(values = c("#00AEEF", "#F4008A")) + 
        theme(
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_line(color = "#3A3052"),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, colour = "#E0E0E0"),
          axis.text.y = element_text(colour = "#E0E0E0"),
          axis.title = element_text(colour = "#E0E0E0"),
          plot.title = element_text(hjust = 0.5, face = "bold", colour = "#FFFFFF"),
          legend.position = "top",
          legend.background = element_rect(fill="transparent"),
          legend.text = element_text(colour = "#E0E0E0"),
          legend.title = element_text(colour = "#E0E0E0")
        ) +
        geom_text(aes(label=round(Value,2)), position=position_dodge(width=0.9), vjust=-0.3, size=3.5, color="#FFFFFF")
    }, bg="transparent")
    
    output$player1NameTable <- renderText({ paste("Average Stats:", input$player1 %||% "Player 1") })
    output$player1Table <- DT::renderDT({
      p1_data_table <- player1_avg_data()
      req(nrow(p1_data_table) > 0)
      
      final_cols_display <- c("Player", "Tm", "G", "MP", "PTS", "TRB", "AST", "STL", "BLK", "FG.", "3P.", "FT.", "TOV", "PF")
      display_cols_for_table <- intersect(final_cols_display, colnames(p1_data_table))
      
      display_data <- p1_data_table %>%
        select(all_of(display_cols_for_table)) %>%
        mutate(across(where(is.numeric), ~round(.x, 2)))
      DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 1, dom = 't'), rownames = FALSE, class = 'cell-border stripe')
    })
    
    output$player2NameTable <- renderText({ paste("Average Stats:", input$player2 %||% "Player 2") })
    output$player2Table <- DT::renderDT({
      p2_data_table <- player2_avg_data()
      req(nrow(p2_data_table) > 0)
      
      final_cols_display <- c("Player", "Tm", "G", "MP", "PTS", "TRB", "AST", "STL", "BLK", "FG.", "3P.", "FT.", "TOV", "PF")
      display_cols_for_table <- intersect(final_cols_display, colnames(p2_data_table))
      
      display_data <- p2_data_table %>%
        select(all_of(display_cols_for_table)) %>%
        mutate(across(where(is.numeric), ~round(.x, 2)))
      DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 1, dom = 't'), rownames = FALSE, class = 'cell-border stripe')
    })
    
    output$shootingPlot <- renderPlot({
      p1_data_shoot <- player1_avg_data()
      p2_data_shoot <- player2_avg_data()
      req(nrow(p1_data_shoot) > 0, nrow(p2_data_shoot) > 0)
      
      shooting_stats_names <- c("FG.", "3P.", "FT.")
      
      valid_shooting_stats_p1 <- intersect(shooting_stats_names, colnames(p1_data_shoot))
      valid_shooting_stats_p2 <- intersect(shooting_stats_names, colnames(p2_data_shoot))
      
      if(length(valid_shooting_stats_p1) == 0 || length(valid_shooting_stats_p2) == 0) return(NULL)
      
      p1_shooting <- p1_data_shoot %>%
        select(Player, all_of(valid_shooting_stats_p1)) %>%
        tidyr::pivot_longer(cols = all_of(valid_shooting_stats_p1), names_to = "PercentageType", values_to = "Value")
      
      p2_shooting <- p2_data_shoot %>%
        select(Player, all_of(valid_shooting_stats_p2)) %>%
        tidyr::pivot_longer(cols = all_of(valid_shooting_stats_p2), names_to = "PercentageType", values_to = "Value")
      
      combined_shooting_data <- rbind(p1_shooting, p2_shooting)
      combined_shooting_data$PercentageType <- factor(combined_shooting_data$PercentageType, levels = shooting_stats_names)
      
      ggplot(combined_shooting_data, aes(x = PercentageType, y = Value, fill = Player)) +
        geom_col(position = position_dodge(width = 0.7), width=0.6, color="#100E17", linewidth=0.3) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
        labs(title = "Average Shooting Percentages",
             x = "Shooting Type", y = "Percentage") +
        theme_minimal(base_size = 14) + 
        scale_fill_manual(values = c("#00AEEF", "#F4008A")) +
        theme(
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.major = element_line(color = "#3A3052"),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5, colour = "#E0E0E0"), 
          axis.text.y = element_text(colour = "#E0E0E0"),
          axis.title = element_text(colour = "#E0E0E0"),
          plot.title = element_text(hjust = 0.5, face="bold", colour = "#FFFFFF"),
          legend.position = "top",
          legend.background = element_rect(fill="transparent"),
          legend.text = element_text(colour = "#E0E0E0"),
          legend.title = element_text(colour = "#E0E0E0")
        ) +
        geom_text(aes(label=scales::percent(Value, accuracy=0.1)), position=position_dodge(width=0.7), vjust=-0.3, size=3.5, color="#FFFFFF")
    }, bg="transparent")
  })
}