# nba-dashboard/app.R

source("global.R") 

ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "vapor", 
    base_font = bslib::font_google("Roboto Condensed"), 
    heading_font = bslib::font_google("Montserrat")   
  ),
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = TRUE),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") 
  ),
  
  navbarPage(
    title = tagList(
      tags$img(src = "nba-logo.png", height = "30px", 
               style = "margin-top: -5px; margin-right: 10px; vertical-align: middle;"),
      "NBA Dashboard" 
    ),
    id = "main_nav", 
    collapsible = TRUE,
    
    tabPanel(
      title = "Overview", 
      value = "overview_tab", 
      icon = icon("newspaper"), 
      overviewTabUI("overview_module") 
    ),
    tabPanel(
      title = "Player Deep Dive", 
      value = "player_deep_dive_tab", 
      icon = icon("user-astronaut"), 
      playerDeepDiveTabUI("player_deep_dive_module") 
    ),
    tabPanel(
      title = "Player Comparison",
      value = "player_comparison_tab",
      icon = icon("balance-scale"), 
      playerComparisonTabUI("player_comparison_module") 
    ),
    tabPanel(
      title = "Goat Discourse",
      value = "goat_discourse_tab_value", 
      icon = icon("comments"), 
      mod_goat_discourse_ui("goat_discourse_module")
    ),
    tabPanel(
      title = "Historical NBA Trends",
      value = "historical_trends_tab",
      icon = icon("chart-line"), 
      mod_historical_trends_ui("historical_trends_module") 
    ),
    tabPanel(
      title = "NBA Predictor",
      value = "nba_predictor_tab",
      icon = icon("magic"), 
      mod_predictor_ui("predictor_module") 
    ),
    tabPanel(
      title = "About",
      value = "about_tab_value", 
      icon = icon("info-circle"),
      mod_about_ui("about_module") 
    )
  ) 
)

server <- function(input, output, session) {
  overviewTabServer("overview_module") 
  
  playerDeepDiveTabServer("player_deep_dive_module", 
                          nba_stats_df_main = nba_stats_df_global, 
                          player_url_map_main = player_url_map_global, 
                          team_aesthetics_main = team_aesthetics_global)
  
  playerComparisonTabServer("player_comparison_module",
                            nba_stats_df_main = nba_stats_df_global) 
  
  mod_goat_discourse_server("goat_discourse_module", 
                            active_tab = reactive(input$main_nav), 
                            tab_value = "goat_discourse_tab_value")
  
  mod_historical_trends_server("historical_trends_module")
  mod_predictor_server("predictor_module")
  
  mod_about_server("about_module")
}

shinyApp(ui = ui, server = server)