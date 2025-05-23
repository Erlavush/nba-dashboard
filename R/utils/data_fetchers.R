# R/utils/data_fetchers.R

print("--- data_fetchers.R: File Sourced ---")

# --- Fetch NBA.com News (WORKING WEB SCRAPER) ---
fetch_nba_dot_com_news <- function(num_headlines = 10) {
  target_url <- "https://www.nba.com/news"; base_url <- "https://www.nba.com"
  news_data <- NULL
  print(paste0(Sys.time(), " [FETCH_NEWS_FUNC] Attempting fetch news from: ", target_url))
  
  # Introduce a small delay before any network request
  Sys.sleep(runif(1, 0.3, 0.8)) 
  
  selectors <- list(
    list(item = "div[class*='ArticleTile_tile']", link = "article > a", headline = "h4[class*='ArticleTile_tileTitle']", image = "img[class*='VideoThumbnail_image']"),
    list(item = "div.ArticleTile_tile__y70gI", link = "article > a", headline = "h4.ArticleTile_tileTitle__aA8g7", image = "img.VideoThumbnail_image__fa2Go"),
    list(item = ".default-tile", link = ".default-tile__link", headline = ".default-tile__headline", image = ".default-tile__image img")
  )
  
  for (i in 1:length(selectors)) {
    selector_set <- selectors[[i]]
    print(paste0(Sys.time(), " [FETCH_NEWS_FUNC] Trying selector set # ", i))
    fetched_items <- tryCatch({
      response <- httr::GET(target_url, 
                            httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.159 Safari/537.36 RShinyApp"), 
                            httr::timeout(20),
                            httr::add_headers( 
                              "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
                              "Accept-Language" = "en-US,en;q=0.9",
                              "Cache-Control" = "max-age=0",
                              "Upgrade-Insecure-Requests" = "1" 
                            ))
      httr::stop_for_status(response, task = "fetch NBA.com news page")
      page_content <- httr::content(response, "text", encoding = "UTF-8") %>% rvest::read_html()
      page_content %>% rvest::html_elements(selector_set$item)
    }, error = function(e) {
      warning(paste0(Sys.time(), " [FETCH_NEWS_FUNC] Error with selector set #", i, ": ", e$message))
      NULL
    })
    
    if (!is.null(fetched_items) && length(fetched_items) > 0) {
      print(paste0(Sys.time(), " [FETCH_NEWS_FUNC] Found ", length(fetched_items), " items with selector set #", i))
      current_news_data <- fetched_items %>%
        purrr::map_dfr(function(item_node) { 
          tryCatch({
            link_node <- item_node %>% rvest::html_element(selector_set$link)
            link <- if (!is.na(link_node)) rvest::html_attr(link_node, "href") else NA_character_
            headline_node <- item_node %>% rvest::html_element(selector_set$headline)
            headline <- if (!is.na(headline_node)) rvest::html_text(headline_node, trim = TRUE) else NA_character_
            img_node <- item_node %>% rvest::html_element(selector_set$image)
            image_url <- if (!is.na(img_node)) rvest::html_attr(img_node, "src") else NA_character_
            
            if (any(is.na(c(link, headline))) || nchar(stringr::str_trim(link %||% "")) == 0 || nchar(stringr::str_trim(headline %||% "")) < 5) return(NULL) 
            if (stringr::str_starts(link, "/")) link <- paste0(base_url, link)
            else if (!stringr::str_starts(link, "http")) return(NULL) 
            
            final_image_url <- if (is.na(image_url) || !stringr::str_starts(image_url, "http")) FALLBACK_IMAGE_URL else image_url
            tibble::tibble(headline = stringr::str_trim(headline), link = link, image_url = final_image_url)
          }, error = function(e_map) { warning(paste0(Sys.time(), " [FETCH_NEWS_FUNC] Error mapping an item: ", e_map$message)); NULL })
        })
      
      if (!is.null(current_news_data) && inherits(current_news_data, "data.frame") && "link" %in% names(current_news_data)) {
        news_data <- current_news_data %>% dplyr::distinct(link, .keep_all = TRUE) 
        if (nrow(news_data) > 0) {
          news_data <- news_data %>% dplyr::slice_head(n = num_headlines)
          print(paste0(Sys.time(), " [FETCH_NEWS_FUNC] Successfully processed ", nrow(news_data), " headlines using selector set #", i))
          return(news_data) 
        }
      }
    }
  } 
  warning(paste0(Sys.time(), " [FETCH_NEWS_FUNC] Failed to fetch news using all available selectors or data was empty."))
  return(NULL) 
}


# --- Helper function to process a single conference table from Basketball-Reference ---
process_conference_table <- function(html_content, table_id, conference_name_in_header) {
  conference_table_node <- html_content %>% rvest::html_element(paste0("#", table_id))
  if (is.na(conference_table_node)) {
    warning(paste0(Sys.time(), " [FETCH_STANDINGS_FUNC] ", conference_name_in_header, " Conference table #", table_id, " not found."))
    return(list(df = NULL, error_msg = paste(conference_name_in_header, "Conference table not found.")))
  }
  
  rows <- conference_table_node %>% rvest::html_elements("tbody tr.full_table")
  if (length(rows) == 0) { 
    rows <- conference_table_node %>% rvest::html_elements("tbody tr")
  }
  if (length(rows) == 0) {
    warning(paste0(Sys.time(), " [FETCH_STANDINGS_FUNC] No data rows found in ", conference_name_in_header, " table #", table_id))
    return(list(df = NULL, error_msg = paste("No data rows found for", conference_name_in_header, "Conference.")))
  }
  
  all_row_data <- purrr::map_df(rows, function(row) {
    team_cell_node <- row %>% rvest::html_element("th[data-stat='team_name']") 
    team_name_link_node <- team_cell_node %>% rvest::html_element("a")
    
    team_name_text <- team_name_link_node %>% rvest::html_text(trim = TRUE) %||% (team_cell_node %>% rvest::html_text(trim = TRUE))
    team_href <- team_name_link_node %>% rvest::html_attr("href") %||% NA_character_
    
    team_abbr <- NA_character_
    if (!is.na(team_href) && stringr::str_detect(team_href, "/teams/([A-Z0-9]{3})/")) {
      team_abbr <- stringr::str_match(team_href, "/teams/([A-Z0-9]{3})/")[1, 2]
    }
    
    wins_text <- row %>% rvest::html_element("td[data-stat='wins']") %>% rvest::html_text(trim = TRUE)
    losses_text <- row %>% rvest::html_element("td[data-stat='losses']") %>% rvest::html_text(trim = TRUE)
    wl_pct_text <- row %>% rvest::html_element("td[data-stat='win_loss_pct']") %>% rvest::html_text(trim = TRUE)
    gb_text <- row %>% rvest::html_element("td[data-stat='gb']") %>% rvest::html_text(trim = TRUE)
    
    tibble::tibble(
      Team_Abbr = team_abbr,
      Team = team_name_text,
      W_text = wins_text,
      L_text = losses_text,
      `W/L%_text` = wl_pct_text,
      GB_text = gb_text
    )
  })
  
  if (nrow(all_row_data) == 0) {
    warning(paste0(Sys.time(), " [FETCH_STANDINGS_FUNC] Could not extract any data rows for ", conference_name_in_header))
    return(list(df = NULL, error_msg = paste("Failed to extract data for", conference_name_in_header, "Conference.")))
  }
  
  conf_df_processed <- all_row_data %>%
    dplyr::filter(!is.na(Team) & Team != "" & !is.na(W_text)) %>% 
    dplyr::mutate(
      Team = stringr::str_remove(Team, "\\*$"), 
      W = suppressWarnings(as.integer(W_text)),
      L = suppressWarnings(as.integer(L_text)),
      `W/L%` = suppressWarnings(as.numeric(`W/L%_text`)),
      GB_raw = dplyr::if_else(GB_text == "—" | is.na(GB_text), "0.0", GB_text),
      GB = suppressWarnings(as.numeric(GB_raw)),
      Logo_Path = dplyr::case_when(
        !is.na(Team_Abbr) ~ paste0("team_logos/", Team_Abbr, ".png"),
        TRUE ~ "team_logos/NBA.png" 
      )
    ) %>%
    dplyr::select(Logo_Path, Team, W, L, `W/L%`, GB) 
  
  print(paste0(Sys.time(), " [FETCH_STANDINGS_FUNC] Successfully processed ", conference_name_in_header, " Conference standings. Rows: ", nrow(conf_df_processed)))
  return(list(df = conf_df_processed, error_msg = NULL))
}


# --- Fetch Standings Data from Basketball-Reference.com ---
scrape_standings_data <- function(season_identifier = "current") {
  # Introduce a small delay
  Sys.sleep(runif(1, 0.5, 1.5)) 
  
  target_url <- "https://www.basketball-reference.com/friv/standings.fcgi"
  print(paste0(Sys.time(), " [FETCH_STANDINGS_FUNC] Attempting to fetch standings from: ", target_url))
  
  results <- list(data = list(east_standings = NULL, west_standings = NULL), error = NULL)
  
  tryCatch({
    response <- httr::GET(target_url, 
                          httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.159 Safari/537.36 RShinyApp"), 
                          httr::timeout(20),
                          httr::add_headers( 
                            "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
                            "Accept-Language" = "en-US,en;q=0.9",
                            "Cache-Control" = "max-age=0",
                            "Upgrade-Insecure-Requests" = "1"
                          ))
    httr::stop_for_status(response, task = "fetch basketball-reference standings page")
    page_content <- httr::content(response, "text", encoding = "UTF-8") %>% rvest::read_html()
    
    east_result <- process_conference_table(page_content, "standings_e", "Eastern Conference")
    results$data$east_standings <- east_result$df
    if (!is.null(east_result$error_msg)) {
      results$error <- paste(results$error %||% "", east_result$error_msg, sep = "\n")
    }
    
    west_result <- process_conference_table(page_content, "standings_w", "Western Conference")
    results$data$west_standings <- west_result$df
    if (!is.null(west_result$error_msg)) {
      results$error <- paste(results$error %||% "", west_result$error_msg, sep = "\n")
    }
    
    if (!is.null(results$error)) {
      results$error <- stringr::str_trim(results$error)
      if (results$error == "") results$error <- NULL
    }
    
    if (is.null(results$data$east_standings) && is.null(results$data$west_standings) && is.null(results$error)) {
      results$error <- "Failed to fetch or process any standings data."
    }
    
  }, error = function(e) {
    error_msg <- paste0("General error fetching or parsing standings: ", e$message)
    warning(paste0(Sys.time(), " [FETCH_STANDINGS_FUNC] ", error_msg))
    results$error <- error_msg
    results$data$east_standings <- NULL
    results$data$west_standings <- NULL
  })
  
  return(results)
}


# --- Fetch Scoreboard Data from ESPN.com ---
scrape_scoreboard_data <- function(fetch_date) {
  if (!inherits(fetch_date, "Date")) {
    fetch_date <- tryCatch(as.Date(fetch_date), error = function(e) Sys.Date())
  }
  
  date_str_url <- format(fetch_date, "%Y%m%d")
  target_url <- glue::glue("https://www.espn.com/nba/scoreboard/_/date/{date_str_url}")
  
  print(paste0(Sys.time(), " [FETCH_SCOREBOARD_ESPN_FUNC] Attempting to fetch scoreboard from: ", target_url))
  
  results <- list(data = NULL, error = NULL, 
                  current_date_display = format(fetch_date, "%B %d, %Y")) 
  
  Sys.sleep(runif(1, 0.7, 1.5)) 
  
  tryCatch({
    response <- httr::GET(target_url, 
                          httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.159 Safari/537.36 RShinyApp"), 
                          httr::timeout(25),
                          httr::add_headers( 
                            "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
                            "Accept-Language" = "en-US,en;q=0.9",
                            "Cache-Control" = "max-age=0",
                            "Upgrade-Insecure-Requests" = "1"
                          ))
    
    if (httr::status_code(response) >= 400) {
      results$error <- paste("Failed to fetch ESPN scoreboard. Status code:", httr::status_code(response), "URL:", target_url)
      warning(paste0(Sys.time(), " [FETCH_SCOREBOARD_ESPN_FUNC] ", results$error))
      return(results)
    }
    
    page_content_raw <- httr::content(response, "text", encoding = "UTF-8")
    
    parsed_html_for_date_check <- tryCatch(rvest::read_html(page_content_raw), error = function(e) NULL)
    
    if (is.null(parsed_html_for_date_check)) {
      results$error <- "Failed to parse HTML content from ESPN."
      warning(paste0(Sys.time(), " [FETCH_SCOREBOARD_ESPN_FUNC] ", results$error))
      return(results)
    }
    
    # Update current_date_display from page header first
    date_header_node <- parsed_html_for_date_check %>% rvest::html_element("h3.Card__Header__Title") 
    if(is.na(date_header_node)) { 
      date_header_node <- parsed_html_for_date_check %>% rvest::html_element(".Scoreboard__Date") 
    }
    if(!is.na(date_header_node)) {
      results$current_date_display <- date_header_node %>% rvest::html_text(trim = TRUE) %||% results$current_date_display
    }
    
    
    if (stringr::str_detect(page_content_raw, "No games scheduled") || stringr::str_detect(page_content_raw, "Scoreboard is empty")) {
      results$error <- paste0("No games scheduled on ESPN for ", results$current_date_display, ".")
      print(paste0(Sys.time(), " [FETCH_SCOREBOARD_ESPN_FUNC] ", results$error))
      return(results)
    }
    
    game_sections <- parsed_html_for_date_check %>% rvest::html_elements("section.Scoreboard[id]") 
    if (length(game_sections) == 0) { 
      game_sections <- parsed_html_for_date_check %>% rvest::html_elements(".sb-scorecard-container") 
      if (length(game_sections) == 0) {
        results$error <- "No game sections found on ESPN page, or structure changed significantly."
        print(paste0(Sys.time(), " [FETCH_SCOREBOARD_ESPN_FUNC] ", results$error))
        return(results)
      }
    }
    
    games_data_list <- purrr::map(game_sections, function(game_node) {
      away_node <- game_node %>% rvest::html_element("li.ScoreboardScoreCell__Item--away")
      home_node <- game_node %>% rvest::html_element("li.ScoreboardScoreCell__Item--home")
      
      if(is.na(away_node) || is.na(home_node)){
        competitor_nodes <- game_node %>% rvest::html_elements("li.ScoreboardScoreCell__Item")
        if (length(competitor_nodes) != 2) return(NULL)
        away_node <- competitor_nodes[[1]]
        home_node <- competitor_nodes[[2]]
      }
      
      parse_competitor <- function(comp_node) {
        team_name_node <- comp_node %>% rvest::html_element(".ScoreCell__TeamName")
        team_name <- team_name_node %>% rvest::html_text(trim = TRUE) %||% "N/A"
        
        team_link_node <- comp_node %>% rvest::html_element("a.AnchorLink[href*='/nba/team/_/name/']")
        team_href <- team_link_node %>% rvest::html_attr("href") %||% NA_character_
        team_abbr_raw <- NA_character_
        if (!is.na(team_href)) {
          match_val <- stringr::str_match(team_href, "/nba/team/_/name/([a-zA-Z0-9]+)/") 
          if (nrow(match_val) > 0 && !is.na(match_val[1,2])) {
            team_abbr_raw <- toupper(match_val[1,2])
          }
        }
        local_logo_path <- if(!is.na(team_abbr_raw)) paste0("team_logos/", team_abbr_raw, ".png") else "team_logos/NBA.png"
        
        score_node <- comp_node %>% rvest::html_element(".ScoreCell__Score")
        score <- score_node %>% rvest::html_text(trim = TRUE) %>% suppressWarnings(as.integer())
        
        is_winner <- comp_node %>% rvest::html_attr("class") %>% stringr::str_detect("ScoreboardScoreCell__Item--winner")
        
        record_node <- comp_node %>% rvest::html_element(".ScoreboardScoreCell__RecordContainer .ScoreboardScoreCell__Record")
        record_main_text <- record_node %>% rvest::html_text(trim = TRUE) %||% NA_character_
        record <- stringr::str_extract(record_main_text, "^[0-9]+-[0-9]+") %||% NA_character_
        
        list(
          name = team_name,
          abbr = team_abbr_raw, 
          logo_path = local_logo_path,
          score = score,
          is_winner = is_winner,
          record = record
        )
      }
      
      away_data <- parse_competitor(away_node)
      home_data <- parse_competitor(home_node)
      
      game_status_node <- game_node %>% rvest::html_element(".ScoreCell__Time") %||% 
        (game_node %>% rvest::html_element(".gameNote")) %||% 
        (game_node %>% rvest::html_element("div[class*='status-detail']")) 
      game_status <- game_status_node %>% rvest::html_text(trim = TRUE) %||% "Scheduled"
      
      if (stringr::str_detect(game_status, "/")) {
        game_status <- stringr::str_split(game_status, "/")[[1]][1] 
      }
      
      tibble::tibble(
        team_a_name = away_data$name,
        team_a_abbr = away_data$abbr,
        team_a_logo = away_data$logo_path, 
        team_a_score = away_data$score,
        team_a_is_winner = away_data$is_winner,
        team_a_record = away_data$record,
        
        team_b_name = home_data$name,
        team_b_abbr = home_data$abbr,
        team_b_logo = home_data$logo_path, 
        team_b_score = home_data$score,
        team_b_is_winner = home_data$is_winner,
        team_b_record = home_data$record,
        
        game_status = game_status
      )
    })
    
    results$data <- dplyr::bind_rows(games_data_list)
    
    if(nrow(results$data %||% data.frame()) > 0) { 
      print(paste0(Sys.time(), " [FETCH_SCOREBOARD_ESPN_FUNC] Successfully processed ", nrow(results$data), " games for ", results$current_date_display))
      if (nrow(results$data) > 0) {
        print("---- ESPN Scraper: Sample Local Logo Paths Generated ----")
        print(head(results$data$team_a_logo, 2))
        print(head(results$data$team_b_logo, 2))
      }
    } else if (is.null(results$error)) { 
      results$error <- results$error %||% "No games found, or data could not be processed."
      print(paste0(Sys.time(), " [FETCH_SCOREBOARD_ESPN_FUNC] ", results$error))
    }
    
  }, error = function(e) {
    error_msg <- paste0("General error fetching or parsing ESPN scoreboard: ", e$message, " URL: ", target_url)
    warning(paste0(Sys.time(), " [FETCH_SCOREBOARD_ESPN_FUNC] ", error_msg))
    results$error <- error_msg
  })
  
  return(results)
}


# --- Load Static League Leaders Data (No Images) ---
scrape_league_leaders_data <- function(season_identifier = "current_static") {
  print(paste0(Sys.time(), " [LOAD_STATIC_LEADERS_FUNC] Loading static league leaders data (no images)."))
  
  static_data_path <- "data/static_league_leaders.Rds" 
  
  if (fs::file_exists(static_data_path)) {
    leaders_data <- tryCatch({
      readRDS(static_data_path)
    }, error = function(e) {
      warning(paste("Error loading static_league_leaders.Rds:", e$message))
      NULL
    })
    
    expected_categories <- c("PTS", "REB", "AST", "STL", "BLK")
    if (!is.null(leaders_data) && is.list(leaders_data) && 
        all(expected_categories %in% names(leaders_data)) &&
        all(sapply(leaders_data[expected_categories], function(df) inherits(df, "data.frame")))) {
      
      expected_cols_in_df <- c("Rank", "Player", "Team", "Stat")
      all_dfs_valid <- TRUE
      for(cat_name in expected_categories) {
        if(!is.null(leaders_data[[cat_name]]) && !all(expected_cols_in_df %in% names(leaders_data[[cat_name]]))) {
          warning(paste("Dataframe for category", cat_name, "is missing expected columns."))
          all_dfs_valid <- FALSE
          break
        }
      }
      
      if(all_dfs_valid){
        return(list(data = leaders_data, error = NULL))
      } else {
        warning("One or more static league leader dataframes have incorrect structure.")
        return(list(data = setNames(vector("list", length(expected_categories)), expected_categories), 
                    error = "Static league leaders data has incorrect structure."))
      }
      
    } else {
      warning("Static league leaders data is missing, malformed, or doesn't contain all expected categories.")
      return(list(data = setNames(vector("list", length(expected_categories)), expected_categories), 
                  error = "Static league leaders data is missing or invalid."))
    }
  } else {
    warning(paste("Static league leaders file not found at:", static_data_path))
    return(list(data = setNames(vector("list", length(expected_categories)), expected_categories), 
                error = "Static league leaders file not found. Please run the processing script."))
  }
}

# --- Function to provide static playoff bracket data ---
get_static_playoff_bracket_data <- function() {
  print(paste0(Sys.time(), " [GET_STATIC_PLAYOFF_BRACKET] Returning hardcoded playoff data based on image."))
  
  static_data <- tibble::tribble(
    ~conference, ~round, ~round_numeric, ~matchup_id_in_round, ~team1_seed, ~team1_name, ~team1_logo, ~team1_is_winner_visual, ~team2_seed, ~team2_name, ~team2_logo, ~team2_is_winner_visual, ~series_score_text, ~game_time_info,
    
    # --- Western Conference ---
    # First Round (round_numeric = 1)
    "West", "First Round", 1, 1, "1", "Thunder", "team_logos/OKC.png", TRUE,  "8", "Grizzlies", "team_logos/MEM.png", FALSE, "OKC WINS 4-0", "Series Ended",
    "West", "First Round", 1, 2, "4", "Nuggets", "team_logos/DEN.png", TRUE,  "5", "Clippers", "team_logos/LAC.png", FALSE, "DEN WINS 4-3", "Series Ended",
    "West", "First Round", 1, 3, "3", "Lakers", "team_logos/LAL.png", FALSE, "6", "Timberwolves", "team_logos/MIN.png", TRUE,  "MIN WINS 4-1", "Series Ended",
    "West", "First Round", 1, 4, "2", "Rockets", "team_logos/HOU.png", FALSE, "7", "Warriors", "team_logos/GSW.png", TRUE,  "GSW WINS 4-3", "Series Ended",
    
    # Conference Semifinals (round_numeric = 2)
    "West", "Conference Semifinals", 2, 1, "1", "Thunder", "team_logos/OKC.png", TRUE,  "4", "Nuggets", "team_logos/DEN.png", FALSE, "OKC WINS 4-3", "Series Ended",
    "West", "Conference Semifinals", 2, 2, "6", "Timberwolves", "team_logos/MIN.png", TRUE,  "7", "Warriors", "team_logos/GSW.png", FALSE, "MIN WINS 4-1", "Series Ended",
    
    # Conference Finals (round_numeric = 3)
    "West", "Conference Finals", 3, 1, "1", "Thunder", "team_logos/OKC.png", FALSE, "6", "Timberwolves", "team_logos/MIN.png", FALSE, "OKC LEADS 2-0", "SAT 7:30 PM ET",
    
    # --- Eastern Conference ---
    # First Round (round_numeric = 1)
    "East", "First Round", 1, 1, "1", "Cavaliers", "team_logos/CLE.png", TRUE,  "8", "Heat", "team_logos/MIA.png", FALSE, "CLE WINS 4-0", "Series Ended",
    "East", "First Round", 1, 2, "4", "Pacers", "team_logos/IND.png", TRUE,  "5", "Bucks", "team_logos/MIL.png", FALSE, "IND WINS 4-1", "Series Ended",
    "East", "First Round", 1, 3, "3", "Knicks", "team_logos/NYK.png", TRUE,  "6", "Pistons", "team_logos/DET.png", FALSE, "NYK WINS 4-2", "Series Ended",
    "East", "First Round", 1, 4, "2", "Celtics", "team_logos/BOS.png", TRUE,  "7", "Magic", "team_logos/ORL.png", FALSE, "BOS WINS 4-1", "Series Ended",
    
    # Conference Semifinals (round_numeric = 2)
    "East", "Conference Semifinals", 2, 1, "1", "Cavaliers", "team_logos/CLE.png", FALSE, "4", "Pacers", "team_logos/IND.png", TRUE,  "IND WINS 4-1", "Series Ended",
    "East", "Conference Semifinals", 2, 2, "3", "Knicks", "team_logos/NYK.png", TRUE,  "2", "Celtics", "team_logos/BOS.png", FALSE, "NYK WINS 4-2", "Series Ended",
    
    # Conference Finals (round_numeric = 3)
    "East", "Conference Finals", 3, 1, "4", "Pacers", "team_logos/IND.png", FALSE, "3", "Knicks", "team_logos/NYK.png", FALSE, "IND LEADS 1-0", "FRI 8:00 PM ET",
    
    # --- NBA Finals --- (round_numeric = 4)
    "Finals", "NBA Finals", 4, 1, NA_character_, "TBD (West)", "team_logos/NBA.png", FALSE, NA_character_, "TBD (East)", "team_logos/NBA.png", FALSE, "JUN 05 8:30 PM ET", "NBA Finals Game 1"
  )
  
  return(static_data)
}

# --- Scrape Playoff Bracket Data ---
scrape_playoff_bracket_data <- function(season_year_for_url = NULL) { 
  print(paste0(Sys.time(), " [STATIC_PLAYOFF_BRACKET_FUNC] Called. Returning static data."))
  
  static_data <- get_static_playoff_bracket_data()
  
  if (!is.null(static_data) && inherits(static_data, "data.frame") && nrow(static_data) > 0) {
    results <- list(data = static_data, error = NULL)
    print(paste0(Sys.time(), " [STATIC_PLAYOFF_BRACKET_FUNC] Successfully prepared static playoff data. Series: ", nrow(results$data)))
  } else {
    results <- list(data = tibble::tibble(), error = "Static playoff data is empty or invalid.")
    print(paste0(Sys.time(), " [STATIC_PLAYOFF_BRACKET_FUNC] Failed to prepare static data: ", results$error))
  }
  
  return(results)
}


print("--- data_fetchers.R: Revising scrape_nba_player_bio ---")



print("--- data_fetchers.R: Reverting Player Info Block scraping to simpler, original logic ---")

scrape_nba_player_bio <- function(player_url) {
  message(paste("Attempting to scrape NBA player bio:", player_url))
  tryCatch({
    response <- httr::GET(
      player_url, 
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.159 Safari/537.36 RShinyAppScraper"),
      httr::timeout(25),
      httr::add_headers( 
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
        "Accept-Language" = "en-US,en;q=0.9",
        "Cache-Control" = "no-cache",
        "Pragma" = "no-cache"
      )
    )
    
    if (httr::http_error(response)) {
      warning(paste("HTTP error for URL:", player_url, "Status:", httr::status_code(response)))
      return(NULL)
    }
    
    page_content_text <- httr::content(response, "text", encoding = "UTF-8")
    page <- rvest::read_html(page_content_text)
    if (grepl("butler", player_url, ignore.case = TRUE)) { 
      writeLines(as.character(page), "jimmy_butler_debug.html")
      message("Saved Jimmy Butler's HTML to jimmy_butler_debug.html")
    }
    if (is.null(page)) {
      warning(paste("Failed to parse page content for URL:", player_url))
      return(NULL)
    }
    
    bio_data <- list()
    

    headshot_selectors <- c(
      "img.PlayerImage_image__wH_0c", 
      "img.PlayerSummary_playerImage__sysif",
      "div[class*='PlayerSummary_playerImageContainer'] img",
      "//img[contains(@alt, 'Headshot') or contains(@alt, 'headshot')]",
      "div.PlayerSummary_lhs__Zv4pc img"
    )
    for (sel in headshot_selectors) {
      node <- if(startsWith(sel, "//")) page %>% rvest::html_node(xpath = sel) else page %>% rvest::html_node(sel)
      if(!is.na(node)) {
        bio_data$headshot_url <- node %>% rvest::html_attr("src")
        if(!is.null(bio_data$headshot_url) && !is.na(bio_data$headshot_url) && bio_data$headshot_url != "") break
      }
    }
    if (is.null(bio_data$headshot_url)) bio_data$headshot_url <- NA_character_
    
    # --- Player Info Blocks  ---
   
    info_item_nodes <- page %>% rvest::html_nodes("div.PlayerSummary_playerInfo__om2G4 div.PlayerSummary_playerInfoGroup__qPKOq")
    if (length(info_item_nodes) == 0) {
      info_item_nodes <- page %>% rvest::html_nodes("div.PlayerSummary_playerInfo__om2G4") 
      if (length(info_item_nodes) > 0) {
        message("Using fallback selector for player info blocks: div.PlayerSummary_playerInfo__om2G4")
      }
    }
    if (length(info_item_nodes) == 0) { 
      info_item_nodes <- page %>% rvest::html_nodes("div[class*='PlayerSummary_playerInfoItem']")
      if (length(info_item_nodes) > 0) {
        message("Using fallback selector for player info blocks: div[class*='PlayerSummary_playerInfoItem']")
      }
    }
    
    
    if (length(info_item_nodes) > 0) {
      message(paste("Found", length(info_item_nodes), "potential player info item blocks."))
      for (block in info_item_nodes) {
        label_node <- block %>% rvest::html_node("p.PlayerSummary_playerInfoLabel__hb5fs, dt") 
        value_node <- block %>% rvest::html_node("p.PlayerSummary_playerInfoValue__JS8_v, dd")
        
        if(is.na(label_node) && (grepl("Label", block %>% html_attr("class") %||% "") || html_name(block) == "dt")){
          label_node <- block
        }
        if(is.na(value_node) && (grepl("Value", block %>% html_attr("class") %||% "") || html_name(block) == "dd")){
          value_node <- block
        }
        
        label <- label_node %>% rvest::html_text() %>% stringr::str_trim() %>% toupper()
        value <- value_node %>% rvest::html_text() %>% stringr::str_trim()
        
        if (!is.na(label) && label != "" && !is.na(value) && value != "") {
          clean_label <- gsub(":", "", label)
          message(paste("Found info item - Label:", clean_label, "Value:", value))
          if (clean_label == "HEIGHT") bio_data$Height <- value
          else if (clean_label == "WEIGHT") bio_data$Weight <- value
          else if (clean_label == "COUNTRY") bio_data$Country <- value
          else if (clean_label == "LAST ATTENDED") bio_data$`Last Attended` <- value
          else if (clean_label == "AGE") bio_data$Age <- value
          else if (clean_label == "BIRTHDATE") bio_data$Birthdate <- value
          else if (clean_label == "DRAFT") bio_data$Draft <- value
          else if (clean_label == "EXPERIENCE") bio_data$Experience <- value
          else bio_data[[clean_label]] <- value 
        }
      }
    } else {
      message("Could not find 'PlayerSummary_playerInfo__om2G4' (or similar) blocks for detailed bio items.")
    }
    # --- End of Player Info Blocks ---
    
    if (!is.null(bio_data$Age) && !is.na(bio_data$Age) && grepl("^\\d+$", bio_data$Age)) {
      bio_data$Age <- paste(bio_data$Age, "years")
    }
    if (!is.null(bio_data$Experience) && !is.na(bio_data$Experience) && 
        !grepl("Years|Rookie", bio_data$Experience, ignore.case = TRUE) && 
        grepl("^\\d+$", bio_data$Experience)) {
      bio_data$Experience <- paste(bio_data$Experience, "Years")
    }
    
    team_jersey_pos_node <- page %>% rvest::html_node("p.PlayerSummary_mainInnerInfo__jv3LO, p[class*='PlayerSummary_mainInnerInfo']")
    if(!is.na(team_jersey_pos_node)){
      team_jersey_pos_text <- team_jersey_pos_node %>% rvest::html_text() %>% stringr::str_trim()
      jersey_match <- stringr::str_extract(team_jersey_pos_text, "#\\s*\\d+")
      if(!is.na(jersey_match)) bio_data$Jersey <- gsub("#\\s*", "", jersey_match)
      parts <- stringr::str_split(team_jersey_pos_text, "\\s*\\|\\s*", simplify = TRUE) %>% stringr::str_trim()
      if(length(parts) >= 3) {
        potential_pos <- parts[length(parts)]
        if (potential_pos %in% c("G", "F", "C", "G-F", "F-G", "F-C", "C-F", "Guard", "Forward", "Center")) {
          bio_data$Position <- potential_pos
        }
      } else if (length(parts) == 2 && !grepl("#", parts[1]) && !grepl("#",parts[2])) { 
        if (parts[2] %in% c("G", "F", "C", "G-F", "F-G", "F-C", "C-F", "Guard", "Forward", "Center")) {
          bio_data$Position <- parts[2]
        }
      } else if (length(parts) == 1 && !grepl("#", parts[1])) { 
        if (parts[1] %in% c("G", "F", "C", "G-F", "F-G", "F-C", "C-F", "Guard", "Forward", "Center")) {
          bio_data$Position <- parts[1]
        }
      }
    } else {
      message("Could not find 'PlayerSummary_mainInnerInfo' for jersey/position.")
    }
    
    if(is.null(bio_data$Position) || is.na(bio_data$Position)){
      pos_node <- page %>% rvest::html_node("div.PlayerSummary_playerPosition__p63ZM p.PlayerSummary_playerInfoValue__JS8_v, span[data-testid='playerProfilePosition'], div[data-testid='playerProfilePositionBlockValue']")
      if(!is.na(pos_node)){
        bio_data$Position <- pos_node %>% rvest::html_text(trim=TRUE)
      }
    }
    
    if (is.null(bio_data$headshot_url) || is.na(bio_data$headshot_url) || bio_data$headshot_url == "") {
      bio_data$headshot_url <- "www/nba-logo.png" 
    } else if (!grepl("^https?:", bio_data$headshot_url)) {
      if(stringr::str_starts(bio_data$headshot_url, "/")) {
        bio_data$headshot_url <- paste0("https://www.nba.com", bio_data$headshot_url)
      } else {
        if (grepl("^\\d+\\.png$", basename(bio_data$headshot_url))) {
          bio_data$headshot_url <- paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", basename(bio_data$headshot_url))
        } else {
          message(paste("Uncertain how to form full URL for headshot:", bio_data$headshot_url))
          bio_data$headshot_url <- "www/nba-logo.png" 
        }
      }
    }
    

    message("--- Debugging FullBio Scraping ---")
    bio_container_selectors <- c(
      "div.PlayerBio_player_bio__kIsc_",                      
      "div[class*='PlayerBio_playerBio__paragraphs_container']", 
      "section[data-testid='playerBio']",                       
      "div[class*='PlayerSummary_bio']",                        
      "article[class*='Article_article__']"                     
    )
    
    bio_container_node <- NA 
    for(sel in bio_container_selectors) {
      message(paste("Trying bio container selector:", sel))
      current_node_try <- page %>% rvest::html_node(sel) 
      if(!is.na(current_node_try)) {
        bio_container_node <- current_node_try
        message(paste("Found bio container with selector:", sel))
        break
      }
    }
    
    full_bio_text_parts <- c()
    if (!is.na(bio_container_node)) { 
      section_paragraphs <- bio_container_node %>% rvest::html_nodes("p") 
      message(paste("Found", length(section_paragraphs), "<p> tags within the bio container."))
      
      if (length(section_paragraphs) > 0) {
        section_content_list <- section_paragraphs %>% rvest::html_text(trim = TRUE)
        section_content_list <- section_content_list[nchar(section_content_list) > 0] # Filter empty
        
        if (length(section_content_list) > 0) {
          full_bio_text_parts <- paste(section_content_list, collapse = "\n\n")
          bio_data$FullBio <- full_bio_text_parts
          message(paste("Extracted FullBio text from <p> tags, length:", nchar(bio_data$FullBio)))
        } else {
          message("No non-empty <p> tags found in bio container. Trying all text from container.")
         
          all_text_from_container <- bio_container_node %>% rvest::html_text(trim = TRUE)
          all_text_from_container <- gsub("\\n\\s*\\n+", "\n\n", all_text_from_container) 
          if (nchar(all_text_from_container) > 10) { 
            bio_data$FullBio <- all_text_from_container
            message(paste("Extracted FullBio using all_text_from_container, length:", nchar(bio_data$FullBio)))
          } else {
            message("Fallback all_text_from_container was too short or empty.")
          }
        }
      } else { # No <p> tags found in container
        message("No <p> tags found in bio container. Trying all text from container.")
        all_text_from_container <- bio_container_node %>% rvest::html_text(trim = TRUE)
        all_text_from_container <- gsub("\\n\\s*\\n+", "\n\n", all_text_from_container)
        if (nchar(all_text_from_container) > 10) { 
          bio_data$FullBio <- all_text_from_container
          message(paste("Extracted FullBio using all_text_from_container, length:", nchar(bio_data$FullBio)))
        } else {
          message("Fallback all_text_from_container was too short or empty.")
        }
      }
    } else {
      message("Could not find any bio text container using common selectors.")
    }
    message("--- Finished Debugging FullBio Scraping ---")
    
    if (is.null(bio_data$FullBio) || is.na(bio_data$FullBio) || bio_data$FullBio == "") {
      bio_data$FullBio <- "Bio text not found or structure changed." 
    }
    
    message(paste("Final scraped items for", player_url, ":", paste(names(bio_data), collapse=", ")))
    return(bio_data)
    
  }, error = function(e) {
    warning(paste("General error in scrape_nba_player_bio for", player_url, ":", e$message))
    return(NULL) 
  })
}