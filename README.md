![image](https://github.com/user-attachments/assets/4db4fc18-7dee-494f-b2c9-677a3ca2592d)# NBA Dashboard - CS 226 Data Analytics Project

![NBA Dashboard - Overview Tab Screenshot](![image](https://github.com/user-attachments/assets/6a25b153-e46b-49c8-9cba-3d1b3b03aaf0))
)
)

## Project Description

This NBA Dashboard is an interactive web application built with R and Shiny. It serves as a comprehensive platform for exploring various facets of the National Basketball Association, from real-time updates and player statistics to historical trends and public discourse analysis. This project was developed as a learning evidence for the CS 226 – Data Analytics – Stat Using R course at the University of Southeastern Philippines.

## Features

The dashboard is organized into several key modules:

1.  **Overview**:
    *   **Latest NBA News**: Stay updated with the most recent headlines.
    *   **Daily Scoreboard**: Check scores for games on any selected date.
    *   **League Standings**: View current Eastern and Western Conference standings.
    *   **League Leaders**: See top performers in Points, Rebounds, Assists, Steals, and Blocks.
    *   **Playoff Bracket**: Visualize the current (or a sample) NBA playoff picture.
    ![NBA Dashboard - Overview Tab Highlights](![image](https://github.com/user-attachments/assets/1a2c25a2-0fb2-4c60-84ac-96b901f1d801))
)
)
    *(Optional: Add another screenshot highlighting a specific part of the Overview, e.g., standings or scoreboard)*

2.  **Player Deep Dive**:
    *   Select any NBA player to view their detailed biographical information (scraped from NBA.com).
    *   See detailed season average statistics, presented with dynamic team-specific theming.
    ![NBA Dashboard - Player Deep Dive Screenshot](![image](https://github.com/user-attachments/assets/1af1e5fe-8b6f-48d3-95ca-5e4efc066a97))
)

3.  **Player Comparison**:
    *   Compare two NBA players side-by-side based on their season averages.
    *   Visualizations include radar charts for key stats, bar charts for selected metrics, and detailed statistical tables.
    ![NBA Dashboard - Player Comparison Screenshot](![image](https://github.com/user-attachments/assets/fb6ad49a-990f-44bc-ad35-1ea82d2ba538))
)

4.  **Goat Discourse**:
    *   Analyze public sentiment and key discussion themes from Twitter data related to NBA legends (Michael Jordan, LeBron James, Kobe Bryant, Kevin Durant).
    *   Features sentiment distribution donut charts, prominent term word clouds, and example positive/negative mentions.
    *   Compare the discourse around two selected legends.
    ![NBA Dashboard - Goat Discourse Screenshot](![image](https://github.com/user-attachments/assets/642fb7ee-5856-4b18-a24d-271b06a9e2da)
)

5.  **Historical NBA Trends**:
    *   Visualize league-average statistical trends over NBA history for both regular season and playoffs.
    *   Explore changes in 3-point shooting (volume and efficiency), 2-point shooting, and pace of play.
    ![NBA Dashboard - Historical Trends Screenshot](![image](https://github.com/user-attachments/assets/3d4207c0-c6a2-4dd6-be89-db0066ee8fc4))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
6.  **NBA Predictor**:
    *   Predict the win probability for a hypothetical NBA game between two selected teams on a specified date.
    *   The prediction is based on a logistic regression model trained on historical game data.
    *   View key team statistics leading into the hypothetical matchup.
    ![NBA Dashboard - Predictor Screenshot](![image](https://github.com/user-attachments/assets/2fcc437e-b869-44f5-9c7f-e5b7efcade48))
)

7.  **About**:
    *   Provides information about the project, its purpose, the developers, data sources, and an explanation of each dashboard tab's functionality.
    ![NBA Dashboard - About Tab Screenshot](![image](https://github.com/user-attachments/assets/8f185d72-d909-4b91-9ab8-77337d5d4885))
)

## Technologies Used

*   **R**: Core programming language.
*   **Shiny**: Web application framework.
*   **bslib**: For Bootstrap 5 theming and UI components.
*   **dplyr, tidyr, purrr**: For data manipulation.
*   **ggplot2, plotly, echarts4r, wordcloud2, fmsb**: For data visualization.
*   **rvest, httr**: For web scraping.
*   **tidytext, textdata**: For text analysis in the Goat Discourse module.
*   **renv**: For project dependency management.

## Data Sources

Data for this dashboard was primarily sourced from:
*   **Kaggle**: NBA Database (for historical trends, predictor), NBA 24-25 Stats Dataset, NBA GOAT Twitter Dataset.
*   **Basketball-Reference.com**: NBA standings, league average trends.
*   **NBA.com**: Latest NBA news, player biographical information, team logos.

## How to Run Locally

1.  Clone this repository:
    ```bash
    git clone https://github.com/Erlavush/nba-dashboard.git
    cd nba-dashboard
    ```
2.  Open `nba-dashboard.Rproj` in RStudio.
3.  Install `renv` if you haven't already: `install.packages("renv")`
4.  Restore the project environment: `renv::restore()`
    *   This will install all the R packages listed in `renv.lock` to a project-local library.
5.  Run the Shiny application by opening `app.R` and clicking "Run App" in RStudio, or by running `shiny::runApp()` in the R console.

## Online Access

The dashboard is deployed online and can be accessed here:
**[https://earlavush.shinyapps.io/earlkent-nbadashb/]**

## Project Team
*   **Kent Paulo R. Delgado**
*   **Earl Josh B. Delgado**

Course: CS 226 – DATA ANALYTICS – STAT USING R (BSCS-DS 2A)
Professor: Jamal Kay B. Rogers
University of Southeastern Philippines
