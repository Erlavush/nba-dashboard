
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_historical_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Historical NBA Statistical Trends", style = "text-align:center; margin-bottom:10px;"),
    p("See how NBA league-average stats changed over time, for both Regular Season and Playoffs.",
      br(), 
      "Note: 3-point data starts from 1979-80. Early era Pace data may vary.",
      style = "text-align:center; font-style:italic; color:#aaa; margin-bottom:20px; font-size:0.9em;"
    ),
    
    fluidRow(
      column(width = 6,
             h4("Regular Season Trends", style = "color: #ffffff; border-bottom: 1px solid #444; padding-bottom: 5px; margin-bottom: 15px; text-align:center;"),
             
             h5("3-Point Shooting Volume", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("season_3pa_plot"), height = "300px"), type = 6, color = "#1d428a"),
             br(),
             
             h5("3-Point Shooting Efficiency", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("season_3pct_plot"), height = "300px"), type = 6, color = "#1d428a"),
             br(),
             
             h5("2-Point Shooting Volume (Proxy for Rim/Midrange)", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("season_2pa_plot"), height = "300px"), type = 6, color = "#1d428a"),
             br(),
             
             h5("2-Point Shooting Efficiency (Proxy for Rim/Midrange)", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("season_2pct_plot"), height = "300px"), type = 6, color = "#1d428a"),
             br(),
             
             h5("Pace of Play (Possessions per 48 min)", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("season_pace_plot"), height = "300px"), type = 6, color = "#1d428a")
      ),
      
      column(width = 6,
             h4("Playoff Trends", style = "color: #ffffff; border-bottom: 1px solid #444; padding-bottom: 5px; margin-bottom: 15px; text-align:center;"),
             
             h5("3-Point Shooting Volume", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("playoff_3pa_plot"), height = "300px"), type = 6, color = "#1d428a"),
             br(),
             
             h5("3-Point Shooting Efficiency", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("playoff_3pct_plot"), height = "300px"), type = 6, color = "#1d428a"),
             br(),
             
             h5("2-Point Shooting Volume (Proxy for Rim/Midrange)", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("playoff_2pa_plot"), height = "300px"), type = 6, color = "#1d428a"),
             br(),
             
             h5("2-Point Shooting Efficiency (Proxy for Rim/Midrange)", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("playoff_2pct_plot"), height = "300px"), type = 6, color = "#1d428a"),
             br(),
             
             h5("Pace of Play (Possessions per 48 min)", style = "text-align:center; color: #e0e0e0;"),
             shinycssloaders::withSpinner(plotlyOutput(ns("playoff_pace_plot"), height = "300px"), type = 6, color = "#1d428a")
      )
    ), 
    uiOutput(ns("data_source_notes")) 
  ) # End tagList
}

# --- Server Function ---
#' Historical Trends Module Server Function (Modified for Immediate Load)
#' @param id Internal parameters for {shiny}.
#' @noRd
mod_historical_trends_server <- function(id) { # REMOVED active_tab and tab_value parameters
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv_trends <- reactiveValues(
      season_trend_data = NULL,
      playoff_trend_data = NULL,
      data_loaded = FALSE,
      loading_error = NULL
    )
    
    # Helper function to load and clean trend data
    load_and_clean_trend_data <- function(file_path, data_label = "Data") {
      print(glue::glue("MOD_HIST_TRENDS: Attempting to load {data_label} from: {file_path}"))
      if (!file.exists(file_path)) {
        stop(glue::glue("File not found: {file_path}"))
      }
      
      # Read CSV, skipping the first header line and treating all columns as character initially
      raw_data <- tryCatch({
        readr::read_csv(file_path, skip = 1, col_types = cols(.default = "c"), show_col_types = FALSE)
      }, error = function(e) {
        stop(glue::glue("Error reading CSV '{basename(file_path)}': {e$message}"))
      })
      
      if (nrow(raw_data) == 0) {
        print(glue::glue("MOD_HIST_TRENDS: No data rows in {basename(file_path)} after initial read."))
        return(tibble()) # Return empty tibble if no data
      }
      
      # Define columns to convert to numeric (using names directly from the CSV)
      numeric_cols <- c(
        "Age", "G", "MP", "FG", "FGA", "3P", "3PA", "FT", "FTA",
        "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS",
        "FG%", "3P%", "FT%", "Pace", "eFG%", "TOV%", "ORB%", "FT/FGA", "ORtg", "TS%"
      )
      # Ensure only existing columns are selected for numeric conversion
      cols_to_convert <- intersect(numeric_cols, names(raw_data))
      
      cleaned_data <- raw_data %>%
        # Parse Season to YearEnd (numeric end year of the season)
        mutate(
          YearEnd = suppressWarnings(
            as.integer(stringr::str_sub(Season, 1, 4)) + 1 # e.g., "2023-24" -> 2023 + 1 = 2024
          )
        ) %>%
        # Convert statistical columns to numeric, coercing errors to NA
        mutate(across(all_of(cols_to_convert), ~suppressWarnings(as.numeric(as.character(.))))) %>%
        # Filter out rows where YearEnd could not be parsed (essential for plotting)
        filter(!is.na(YearEnd)) %>%
        # Calculate 2-Point stats
        mutate(
          `2P` = FG - `3P`,       # Using backticks for column names with special chars
          `2PA` = FGA - `3PA`,
          `2P%` = ifelse(`2PA` > 0, `2P` / `2PA`, NA_real_)
        ) %>%
        arrange(YearEnd) # Ensure data is sorted by year for line plots
      
      print(glue::glue("MOD_HIST_TRENDS: Successfully loaded and cleaned {nrow(cleaned_data)} rows for {data_label}."))
      return(cleaned_data)
    } # End load_and_clean_trend_data
    
    # --- MODIFIED: Initial Data Loading ---
    # This observe block will run once when the module is initialized.
    observe({
      req(!rv_trends$data_loaded) # Ensure this runs only once
      print(glue::glue("MOD_HIST_TRENDS: Initializing and loading trend data immediately..."))
      rv_trends$loading_error <- NULL
      
      tryCatch({
        rv_trends$season_trend_data <- load_and_clean_trend_data(
          file_path = "data/NBA Season Trend Analysis Data.csv",
          data_label = "Regular Season Trends"
        )
        rv_trends$playoff_trend_data <- load_and_clean_trend_data(
          file_path = "data/NBA Playoff Trend Analysis Data.csv",
          data_label = "Playoff Trends"
        )
        rv_trends$data_loaded <- TRUE # Set flag AFTER successful load
        print("MOD_HIST_TRENDS: Both season and playoff trend datasets loaded and processed.")
      }, error = function(e) {
        rv_trends$loading_error <- paste("Error loading or processing trend data:", e$message)
        print(glue::glue("MOD_HIST_TRENDS: {rv_trends$loading_error}"))
        rv_trends$data_loaded <- TRUE # Set to true to stop re-triggering, error will be displayed
      })
    }) # End observe for initial data load
    
    # Generic Plotting Function
    create_trend_plot <- function(data, y_var, y_label, title_suffix, plot_color, start_year = NULL, y_format = NULL) {
      req(data, nrow(data) > 0, y_var %in% names(data))
      
      plot_data <- data %>%
        filter(!is.na(!!sym(y_var)))
      
      if (!is.null(start_year)) {
        plot_data <- plot_data %>% filter(YearEnd >= start_year)
      }
      
      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = list(text = glue::glue("No data available for {y_label} {title_suffix}"), font = list(color="#aaaaaa")),
                                paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "", color="#aaaaaa"),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "", color="#aaaaaa"))
        )
      }
      
      y_axis_config <- list(
        title = list(text = y_label, font = list(color = "#f0f0f0")),
        tickfont = list(color = "#f0f0f0"),
        gridcolor = "rgba(100,100,100,0.3)",
        zerolinecolor = "rgba(150,150,150,0.5)"
      )
      if (!is.null(y_format) && y_format == "percent") {
        y_axis_config$tickformat <- ".0%"
      }
      
      p <- plotly::plot_ly(data = plot_data, x = ~YearEnd, y = ~get(y_var),
                           type = 'scatter', mode = 'lines+markers',
                           line = list(color = plot_color, width = 2.5),
                           marker = list(color = plot_color, size = 6, symbol = "circle"),
                           hoverinfo = 'text',
                           text = ~paste0("Season: ", YearEnd -1, "-", substr(YearEnd, 3, 4), "<br>",
                                          y_label, ": ",
                                          if(!is.null(y_format) && y_format == "percent") scales::percent(get(y_var), accuracy = 0.1) else round(get(y_var), 2))) %>%
        plotly::layout(
          title = list(text = glue::glue("{y_label} Trend {title_suffix}"), font = list(color = "#ffffff")),
          xaxis = list(title = list(text = "Season Ending In", font = list(color = "#f0f0f0")),
                       tickfont = list(color = "#f0f0f0"),
                       gridcolor = "rgba(100,100,100,0.3)",
                       zerolinecolor = "rgba(150,150,150,0.5)"),
          yaxis = y_axis_config,
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          legend = list(font = list(color = "#f0f0f0"), bgcolor = 'rgba(0,0,0,0.1)'),
          hovermode = "x unified",
          font = list(family = "Fira Code, monospace")
        )
      return(p)
    }
    
    # --- Render Season Plots ---
    output$season_3pa_plot <- renderPlotly({
      req(rv_trends$data_loaded)
      if(!is.null(rv_trends$loading_error)) return(NULL)
      req(rv_trends$season_trend_data)
      create_trend_plot(rv_trends$season_trend_data, "3PA", "3-Point Attempts", "(Season)", "#4A90E2", start_year = 1980)
    })
    output$season_3pct_plot <- renderPlotly({
      req(rv_trends$data_loaded); if(!is.null(rv_trends$loading_error)) return(NULL); req(rv_trends$season_trend_data)
      create_trend_plot(rv_trends$season_trend_data, "3P%", "3-Point FG%", "(Season)", "#50E3C2", start_year = 1980, y_format = "percent")
    })
    output$season_2pa_plot <- renderPlotly({
      req(rv_trends$data_loaded); if(!is.null(rv_trends$loading_error)) return(NULL); req(rv_trends$season_trend_data)
      create_trend_plot(rv_trends$season_trend_data, "2PA", "2-Point Attempts", "(Season)", "#F5A623")
    })
    output$season_2pct_plot <- renderPlotly({
      req(rv_trends$data_loaded); if(!is.null(rv_trends$loading_error)) return(NULL); req(rv_trends$season_trend_data)
      create_trend_plot(rv_trends$season_trend_data, "2P%", "2-Point FG%", "(Season)", "#F8E71C", y_format = "percent")
    })
    output$season_pace_plot <- renderPlotly({
      req(rv_trends$data_loaded); if(!is.null(rv_trends$loading_error)) return(NULL); req(rv_trends$season_trend_data)
      create_trend_plot(rv_trends$season_trend_data, "Pace", "Pace", "(Season)", "#BD10E0")
    })
    
    # --- Render Playoff Plots ---
    output$playoff_3pa_plot <- renderPlotly({
      req(rv_trends$data_loaded)
      if(!is.null(rv_trends$loading_error)) return(NULL)
      req(rv_trends$playoff_trend_data)
      create_trend_plot(rv_trends$playoff_trend_data, "3PA", "3-Point Attempts", "(Playoffs)", "#4A90E2", start_year = 1980)
    })
    output$playoff_3pct_plot <- renderPlotly({
      req(rv_trends$data_loaded); if(!is.null(rv_trends$loading_error)) return(NULL); req(rv_trends$playoff_trend_data)
      create_trend_plot(rv_trends$playoff_trend_data, "3P%", "3-Point FG%", "(Playoffs)", "#50E3C2", start_year = 1980, y_format = "percent")
    })
    output$playoff_2pa_plot <- renderPlotly({
      req(rv_trends$data_loaded); if(!is.null(rv_trends$loading_error)) return(NULL); req(rv_trends$playoff_trend_data)
      create_trend_plot(rv_trends$playoff_trend_data, "2PA", "2-Point Attempts", "(Playoffs)", "#F5A623")
    })
    output$playoff_2pct_plot <- renderPlotly({
      req(rv_trends$data_loaded); if(!is.null(rv_trends$loading_error)) return(NULL); req(rv_trends$playoff_trend_data)
      create_trend_plot(rv_trends$playoff_trend_data, "2P%", "2-Point FG%", "(Playoffs)", "#F8E71C", y_format = "percent")
    })
    output$playoff_pace_plot <- renderPlotly({
      req(rv_trends$data_loaded); if(!is.null(rv_trends$loading_error)) return(NULL); req(rv_trends$playoff_trend_data)
      create_trend_plot(rv_trends$playoff_trend_data, "Pace", "Pace", "(Playoffs)", "#BD10E0")
    })
    
    # --- UI for Data Source Notes ---
    output$data_source_notes <- renderUI({
      tags$div(style = "text-align: center; font-size: 0.85em; color: #888888; margin-top: 25px; padding: 10px; border-top: 1px solid #333333;",
               # --- SIMPLIFIED TEXT BELOW ---
               p(HTML(
                 "Data fetched from  Basketball-Reference.com.<br>
                 2-Point stats are FG minus 3P (includes rim & midrange)."
               ))
      )
    })
    
  }) # End moduleServer
}
# --- END FILE: modules/mod_historical_trends.R (Full, Complete, Updated for Immediate Load) ---