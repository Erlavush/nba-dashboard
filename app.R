        ui <- fluidPage(
          theme = bslib::bs_theme(
            version = 5,
            bootswatch = "darkly",
            bg = "#000000", fg = "#f0f0f0", primary = "#1d428a",
            base_font = bslib::font_google("Fira Code", local = FALSE),
            heading_font = bslib::font_google("Fira Code", local = FALSE)
          ) %>%
            bslib::bs_add_rules(" // h3, h4 { margin-bottom: 1rem; } "),
          
          tags$head(
            tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
            tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin = TRUE),
            tags$link(href = "https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;700&display=swap", rel = "stylesheet"),
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
          ),
          
          navbarPage(
            title = tagList(
              tags$img(src = "nba-logo.png", height = "30px", style = "margin-top: -5px; margin-right: 10px; vertical-align: middle;"),
              "NBA Dashboard"
            ),
            id = "main_nav",
            collapsible = TRUE,
            
            # --- Welcome Tab ---
            tabPanel(
              title = "Welcome",
              value = "welcome_tab",
              icon = icon("house-chimney"),
              mod_welcome_ui("welcome_page") # Calls UI from mod_welcome.R
            ),
            
            # --- Player Stats Tab ---
            tabPanel(
              title = "Player Comparison",
              value = "player_comp_tab",
              icon = icon("people-arrows"),
              mod_player_stats_ui("player_stats_page") # Calls UI from mod_player_stats.R
            )
            
          ) # end navbarPage
        ) # end fluidPage
        
        # --- Server Definition ---
        server <- function(input, output, session) {
          # Call server function for welcome module
          mod_welcome_server("welcome_page") # Calls Server from mod_welcome.R
          
          # Call server function for player stats module
          mod_player_stats_server("player_stats_page") # Calls Server from mod_player_stats.R
        } # end server
        
        # --- Run Application ---
        shinyApp(ui = ui, server = server)
