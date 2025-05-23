
mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "about-container",
        
        # PROJECT INFO
        div(class = "about-section project-info",
            h3("Project Information", class = "about-header"),
            p(strong("Purpose:"), "Learning Evidence 1 for the course CS 226 – DATA ANALYTICS – STAT USING R."),
            p(strong("Professor:"), "Jamal Kay B. Rogers"),
            p(strong("Course & Section:"), "BSCS-DS 2A"),
            p(strong(""), "University of Southeastern Philippines")
        ),
        
        hr(class = "about-hr"),
        
        # --- NEW SECTION: Dashboard Features & Analytical Insights ---
        div(class = "about-section dashboard-features",
            h3("Dashboard Features & Analytical Insights", class = "about-header"),
            p("This NBA Dashboard is designed to provide fans, analysts, and NBA enthusiasts with a comprehensive platform to explore various aspects of the National Basketball Association. Here's what each section offers:"),
            
            h4("Overview Tab", class="about-header-small" , style="text-align:left; margin-top:15px; margin-bottom:5px; color:#00AEEF;"), # Changed color and alignment
            p(strong("Purpose:"), "Provides a real-time overview of the NBA, offering a quick look at the latest news, daily scores, current league standings, top player statistics, and the playoff picture."),
            p(strong("What it Addresses:"), "Helps users stay updated with the day-to-day happenings in the NBA and understand the current competitive landscape."),
            
            h4("Player Deep Dive Tab", class="about-header-small", style="text-align:left; margin-top:15px; margin-bottom:5px; color:#00AEEF;"),
            p(strong("Purpose:"), "Offers an in-depth look at individual NBA players, combining biographical information (scraped from NBA.com where available) with detailed season average statistics from our dataset (24-25_nba-stats.csv)."),
            p(strong("What it Addresses:"), "Allows users to thoroughly research a specific player's background, current performance metrics, and key stats, all presented with team-specific theming."),
            
            h4("Player Comparison Tab", class="about-header-small", style="text-align:left; margin-top:15px; margin-bottom:5px; color:#00AEEF;"),
            p(strong("Purpose:"), "Enables side-by-side statistical comparison of two selected NBA players based on their season averages, visualized through radar charts, bar charts, and detailed tables."),
            p(strong("What it Addresses:"), "Helps users analyze relative strengths and weaknesses of different players, aiding in debates, fantasy sports decisions, or general player evaluation using aggregated data from the 24-25_nba-stats.csv dataset."),
            
            h4("Goat Discourse Tab", class="about-header-small", style="text-align:left; margin-top:15px; margin-bottom:5px; color:#00AEEF;"),
            p(strong("Purpose:"), "Analyzes public sentiment and key discussion themes from Twitter data (datasets: kd_goat_tw.csv, kobe_goat_tw.csv, lebron_goat_tw.csv, mj_goat_tw.csv) related to NBA legends often part of the 'Greatest Of All Time' (GOAT) debate."),
            p(strong("What it Addresses:"), "Provides insights into how these players that are considered as GREATEST OF ALL TIME (GOAT) are perceived and discussed in public forums, using natural language processing for sentiment analysis and word frequency. Users can explore individual player discourse or compare two legends."),
            
            h4("Historical NBA Trends Tab", class="about-header-small", style="text-align:left; margin-top:15px; margin-bottom:5px; color:#00AEEF;"),
            p(strong("Purpose:"), "Visualizes league-average statistical trends over NBA history for both regular season and playoffs, using data from Basketball-Reference.com (stored in 'NBA Playoff Trend Analysis Data.csv' and 'NBA Season Trend Analysis Data.csv')."),
            p(strong("What it Addresses:"), "Allows users to understand the evolution of NBA gameplay, identify significant shifts in strategy (e.g., the rise of the 3-pointer), and observe how the game has changed over decades."),
            
            h4("NBA Predictor Tab", class="about-header-small", style="text-align:left; margin-top:15px; margin-bottom:5px; color:#00AEEF;"),
            p(strong("Purpose:"), "Predicts the win probability for a hypothetical NBA game between two selected teams on a specified date. The prediction is based on a logistic regression model trained on historical game data (1980 up to the 2022-2023 season)."),
            p(strong("What it Addresses:"), "Offers a data-driven forecast for potential game outcomes, considering team statistics (season-to-date and recent form using a 10-game rolling window) leading up to the hypothetical matchup. Expected predictor names for the model are stored in predictor_names.rds."),
            
            p(style="margin-top:15px;", "This dashboard aims to be a somewhat engaging and fun tool for exploring NBA data through various analytical perspectives ^^")
        ),
        
        hr(class = "about-hr"),
        
        
        # PROFILE OF STUDENT 1 and 2
        div(class = "about-section student-profiles",
            h3("Brainstormed and made by", class = "about-header"),
            fluidRow(
              column(width = 6,
                     div(class = "profile-card",
                         tags$img(src = "student1_placeholder.png", 
                                  alt = "Student 1 Profile Picture", 
                                  class = "profile-pic",
                                  onerror = "this.onerror=null; this.src='nba-logo.png';"), 
                         h4("Kent Paulo R. Delgado", class = "profile-name"),
                         p(class = "profile-bio", 
                           "I am kent, kent stop falling in love with you :<"
                         )
                     )
              ),
              column(width = 6,
                     div(class = "profile-card",
                         tags$img(src = "student2_placeholder.png", 
                                  alt = "Student 2 Profile Picture", 
                                  class = "profile-pic",
                                  onerror = "this.onerror=null; this.src='nba-logo.png';"),
                         h4("Earl Josh B. Delgado", class = "profile-name"),
                         p(class = "profile-bio", 
                           "Hi there, its me EARLLLLL, but you can call me Earl. :>"
                         )
                     )
              )
            )
        ),
        
        hr(class = "about-hr"),
        
        # DATA SOURCESS
        div(class = "about-section data-sources",
            h3("Data Sources", class = "about-header"),
            p("The data used in this dashboard was primarily collected from the following sources (click to open):"),
            tags$ul(
              class = "data-source-list",
              tags$li(
                tags$a(href = "https://www.kaggle.com/datasets/wyattowalsh/basketball?select=csv", target = "_blank", "NBA Database (Kaggle)"),
                " - Used for the historical trends analyis and game predictor part."
              ),
              tags$li(
                tags$a(href = "https://www.kaggle.com/datasets/eduardopalmieri/nba-player-stats-season-2425", target = "_blank", "NBA 24-25 Stats Dataset (Kaggle)"),
                " - Used for the player stats cards and player comparisons."
              ),
              tags$li(
                tags$a(href = "https://scholarship.claremont.edu/cmc_theses/2733/", target = "_blank", "NBA GOAT Twitter Dataset (from a thesis)"),
                " - Used for the 'Goat Discourse' analysis, providing tweets related to specific NBA legends."
              ),
              tags$li(
                tags$a(href = "https://www.basketball-reference.com/", target = "_blank", "Basketball-Reference.com"),
                " - Used for NBA standings, league average trends (season and playoffs), and cross-referencing player statistics. Some data was scraped and stored locally in a csv file."
              ),
              tags$li(
                tags$a(href = "https://www.nba.com/", target = "_blank", "NBA.com"),
                " - Used for fetching the latest NBA news, player biographical information, and team logos."
              ),
              tags$li("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
            )
        ),
        
        hr(class = "about-hr"),
        
        div(class = "about-section",
            h4("Disclaimer", class="about-header-small"),
            p("This dashboard is created for educational purposes as part of a course requirement. All data and trademarks belong to their respective owners :>")
        )
    )
  )
}

mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print(paste0(Sys.time(), " [ABOUT_MOD_SERVER] Instance started for '", id, "'."))
  })
}
