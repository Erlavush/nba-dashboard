# --- START FILE: app.R (Complete and Updated for ALL modules including the rebuilt mod_goat_discourse) ---

# Library calls are typically handled in global.R.
# If global.R is sourced automatically by Shiny (as it should be),
# no explicit library() calls are needed here.

# --- UI Definition ---
ui <- fluidPage(
  # --- Theme Definition (from bslib) ---
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "darkly",
    bg = "#000000", 
    fg = "#f0f0f0", 
    primary = "#1d428a", # NBA Blue
    base_font = bslib::font_google("Fira Code", local = FALSE),
    heading_font = bslib::font_google("Fira Code", local = FALSE)
  ) %>%
    # Example of adding a global CSS rule via bslib (adjust as needed)
    bslib::bs_add_rules("h3, h4 { margin-bottom: 1rem; }"), 
  
  # --- HEAD Section for Custom Links & Meta Tags ---
  tags$head(
    # Preconnect to Google Fonts for performance
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = TRUE),
    # Link to Fira Code font (ensure it's the one you want)
    tags$link(href = "https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;700&display=swap", rel = "stylesheet"),
    
    # --- Link to Custom CSS ---
    # Assumes addResourcePath('www_files', 'www') is correctly defined in global.R
    # This allows referencing files in 'www' directory with 'www_files/' prefix.
    tags$link(rel = "stylesheet", type = "text/css", href = "www_files/styles.css"),
    
    # --- Slick JS/CSS for Carousel (Currently Commented Out) ---
    # If you plan to use the Slick carousel (e.g., for news in mod_welcome), uncomment these.
    # Assumes addResourcePath('slick_libs', 'www/libs/slick') is in global.R
    # tags$link(rel = "stylesheet", type = "text/css", href = "slick_libs/slick.css"),
    # tags$link(rel = "stylesheet", type = "text/css", href = "slick_libs/slick-theme.css")
    # Note: Slick JS is often better placed at the end of the body, or loaded via htmlDependency if more control is needed.
  ), # --- End HEAD Section ---
  
  
  # --- BODY Content: Navbar Page for Main App Structure ---
  navbarPage(
    # --- Navbar Title with Logo ---
    title = tagList(
      # Ensure 'nba-logo.png' is in your 'www' folder.
      # The 'www_files/' prefix is used because of addResourcePath in global.R.
      tags$img(src = "www_files/nba-logo.png", 
               height = "30px", 
               style = "margin-top: -5px; margin-right: 10px; vertical-align: middle;"),
      "NBA Dashboard"
    ),
    # --- Navbar Settings ---
    id = "main_nav", # Crucial ID for observing active tab in server for deferred loading
    collapsible = TRUE, # Enables collapsing on smaller screens
    
    # --- Welcome Tab ---
    tabPanel(
      title = "Welcome",
      value = "welcome_tab", # Unique identifier for this tab
      icon = icon("house-chimney"), 
      mod_welcome_ui("welcome_page") # Call the module UI
    ),
    
    # --- Current Season Player Stats Comparison Tab ---
    tabPanel(
      title = "Current Season Player Comparison",
      value = "player_comp_tab", 
      icon = icon("people-arrows"), 
      mod_player_stats_ui("player_stats_page") # Call the module UI
    ),
    
    # --- Historical Player Stats Tab ---
    tabPanel(
      title = "Historical Player Stats",
      value = "historical_player_stats_tab",
      icon = icon("history"),
      mod_historical_player_stats_ui("historical_player_stats_page")
    ),
    
    # --- Game Predictor Tab ---
    tabPanel(
      title = "Game Predictor",
      value = "game_predictor_tab",
      icon = icon("chart-line"), # Example icon
      mod_game_predictor_ui("game_predictor_page")
    ),
    
    # --- Player Performance Prediction Tab ---
    tabPanel(
      title = "Player Prediction",
      value = "player_perf_tab",
      icon = icon("bullseye"), # Example icon
      mod_player_performance_ui("player_perf_page")
    ),
    
    # --- GOAT Discourse Tab (Rebuilt) ---
    tabPanel(
      title = "GOAT Discourse",
      value = "goat_discourse_tab",         
      icon = icon("comments"), # Font Awesome icon for comments/discourse
      mod_goat_discourse_ui("goat_discourse_page") # Call the rebuilt module's UI
    )
    # --- END: GOAT Discourse Tab ---
    
  ) # --- end navbarPage ---
  
  # --- Optional: JS links at end of body ---
  # If using JS libraries like Slick and not deferring, place script tags here.
  # This is generally better for page load performance.
  # tags$script(src = "slick_libs/slick.min.js") # Uncomment if Slick is used
  
) # --- end fluidPage (UI Definition) ---


# --- Server Definition ---
server <- function(input, output, session) {
  
  # --- Reactive to track the active tab value ---
  # This reads the 'value' attribute from the currently selected tabPanel
  # in the navbarPage with id "main_nav".
  active_tab <- reactive({
    input$main_nav 
  })
  
  # --- Call Module Servers ---
  # Each module server is called with its unique ID and, where applicable,
  # the active_tab reactive and its specific tab_value for deferred loading.
  
  # Welcome module server (loads immediately or on its own internal logic)
  mod_welcome_server("welcome_page")
  
  # Player Stats module server (uses deferred loading based on active_tab)
  mod_player_stats_server("player_stats_page",
                          active_tab = active_tab,
                          tab_value = "player_comp_tab")
  
  # Historical Player Stats module server (uses deferred loading)
  mod_historical_player_stats_server("historical_player_stats_page",
                                     active_tab = active_tab,
                                     tab_value = "historical_player_stats_tab")
  
  # Game Predictor module server (uses deferred loading)
  mod_game_predictor_server("game_predictor_page",
                            active_tab = active_tab,
                            tab_value = "game_predictor_tab")
  
  # Player Performance module server (uses deferred loading)
  mod_player_performance_server("player_perf_page",
                                active_tab = active_tab,
                                tab_value = "player_perf_tab")
  
  # GOAT Discourse module server (rebuilt, uses deferred loading)
  mod_goat_discourse_server("goat_discourse_page",
                            active_tab = active_tab,
                            tab_value = "goat_discourse_tab") 
  
} # --- end server ---

# --- Run Application ---
# This line launches the Shiny app using the defined UI and server.
shinyApp(ui = ui, server = server)
# --- END FILE: app.R ---