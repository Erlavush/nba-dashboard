# --- START FILE: modules/mod_welcome.R (Complete and Updated with Dropdown Year Changes) ---

# --- START: Section 1, Part 1: get_wins_needed function ---

# To be placed at the top of mod_welcome.R or in a separate sourced utility file.

# --- Helper function to determine wins needed to advance in a series ---
get_wins_needed <- function(season_end_year, round_number) {
  # Ensure inputs are numeric
  season_end_year <- suppressWarnings(as.numeric(season_end_year))
  round_number <- suppressWarnings(as.numeric(round_number))
  
  if (is.na(season_end_year) || is.na(round_number)) {
    warning("get_wins_needed: Invalid season_end_year or round_number. Defaulting to 4 wins.")
    return(4)
  }
  
  # Era 1: 1983-84 to 2001-02 (First Round = Best of 5)
  # season_end_year 1984 corresponds to 1983-84 season
  if (season_end_year >= 1984 && season_end_year <= 2002) {
    if (round_number == 1) {
      return(3) # 3 wins needed for First Round
    } else {
      return(4) # 4 wins needed for other rounds (Conf Semis, Conf Finals, NBA Finals)
    }
  }
  # Era 2: 1975-76 to 1982-83 (First Round = Best of 3)
  # season_end_year 1976 corresponds to 1975-76 season
  else if (season_end_year >= 1976 && season_end_year <= 1983) {
    if (round_number == 1) {
      return(2) # 2 wins needed for First Round
    } else {
      return(4) # 4 wins needed for other rounds
    }
  }
  # Modern Era (2002-03 onwards) and default for other rounds in older eras
  else {
    return(4) # Default is 4 wins needed
  }
}
# --- END: Section 1, Part 1: get_wins_needed function ---

# --- START: Section 1, Part 2: get_series_winner function ---

# To be placed in mod_welcome.R, after get_wins_needed or in a sourced utility file.

# --- Helper function to determine the winner of a series ---
get_series_winner <- function(series_row, season_end_year, round_number_of_series) {
  # Initial checks for series_row validity
  if (is.null(series_row) ||
      (!is.list(series_row) && !is.data.frame(series_row)) ||
      (is.data.frame(series_row) && nrow(series_row) == 0)) {
    return(NULL)
  }
  
  # If series_row is a data frame, take the first row as a list
  if (is.data.frame(series_row)) {
    series_row <- as.list(series_row[1, , drop = FALSE])
  }
  
  # Ensure necessary columns exist
  if (!all(c("high_seed_wins", "low_seed_wins", "high_seed_team_name", "low_seed_team_name") %in% names(series_row))) {
    warning("get_series_winner: series_row is missing required name/wins columns.")
    return(NULL)
  }
  
  # Validate season_end_year and round_number_of_series before calling get_wins_needed
  s_year_num <- suppressWarnings(as.numeric(season_end_year))
  r_num_series_num <- suppressWarnings(as.numeric(round_number_of_series))
  
  if (is.null(s_year_num) || is.na(s_year_num) ||
      is.null(r_num_series_num) || is.na(r_num_series_num)) {
    warning("get_series_winner: Missing or invalid season_end_year or round_number_of_series. Cannot determine winner accurately.")
    return(NULL)
  }
  
  wins_needed <- get_wins_needed(
    season_end_year = s_year_num,
    round_number = r_num_series_num
  )
  
  hs_wins <- suppressWarnings(as.integer(series_row$high_seed_wins))
  ls_wins <- suppressWarnings(as.integer(series_row$low_seed_wins))
  
  if (!is.na(hs_wins) && hs_wins == wins_needed) {
    return(series_row$high_seed_team_name)
  } else if (!is.na(ls_wins) && ls_wins == wins_needed) {
    return(series_row$low_seed_team_name)
  } else {
    return(NULL) # No winner determined based on wins_needed
  }
}
# --- END: Section 1, Part 2: get_series_winner function ---

# --- START: Section 1, Part 3: get_series_for_slot function ---

# To be placed in mod_welcome.R, after get_series_winner or in a sourced utility file.

# --- Helper function to get series data for a specific bracket slot ---
get_series_for_slot <- function(
    bracket_data_raw,
    season_end_year, # Argument to pass to get_series_winner
    conference_filter,
    round_num_filter,
    matchup_identifier, # For R1: "1v8", "4v5", etc. For R2: 1 or 2. For R3/R4: 1.
    r1_west_results = list(), # Results from Round 1 West
    r1_east_results = list(), # Results from Round 1 East
    r2_west_results = list(), # Results from Round 2 West
    r2_east_results = list(), # Results from Round 2 East
    r3_west_results = list(), # Results from Round 3 West
    r3_east_results = list()  # Results from Round 3 East
) {
  
  # Basic validation of inputs
  if (!is.data.frame(bracket_data_raw) || nrow(bracket_data_raw) == 0) return(NULL)
  if (is.null(conference_filter) || is.null(round_num_filter) || is.null(matchup_identifier)) return(NULL)
  
  # Ensure season_end_year is valid if we are looking for rounds > 1 (which need get_series_winner)
  current_season_end_year <- suppressWarnings(as.numeric(season_end_year))
  if (round_num_filter > 1 && (is.null(current_season_end_year) || is.na(current_season_end_year))) {
    warning(glue::glue("get_series_for_slot: season_end_year ('{season_end_year}') is required and must be numeric for rounds > 1."))
    return(NULL)
  }
  
  # Filter the raw bracket data for the specific conference and round
  filtered_data_for_round_conf <- bracket_data_raw %>%
    dplyr::filter(conference == conference_filter, round_number == round_num_filter)
  
  # --- Logic for Round 1 ---
  if (round_num_filter == 1) {
    if (is.character(matchup_identifier) && grepl("^\\d+v\\d+$", matchup_identifier)) {
      seeds <- tryCatch(as.integer(strsplit(matchup_identifier, "v")[[1]]), error = function(e) NULL)
      if (!is.null(seeds) && length(seeds) == 2 && !any(is.na(seeds))) {
        seed1 <- min(seeds)
        seed2 <- max(seeds)
        # Find the series matching these two seeds
        found_series <- filtered_data_for_round_conf %>%
          dplyr::filter(
            (!is.na(high_seed_seed) & !is.na(low_seed_seed)) &
              ( (high_seed_seed == seed1 & low_seed_seed == seed2) | (high_seed_seed == seed2 & low_seed_seed == seed1) )
          )
        if (nrow(found_series) >= 1) return(found_series[1, , drop = FALSE])
      }
    }
    return(NULL) # Return NULL if no match for Round 1 or invalid identifier
  }
  
  # --- Logic for Rounds > 1 (Conference Semis, Finals, NBA Finals) ---
  # Determine the winners from the prerequisite series of the previous round
  prev_winners <- list(team1 = NULL, team2 = NULL)
  
  tryCatch({
    if (round_num_filter == 2) { # Conference Semifinals
      round_number_of_prereq_series <- 1
      if (conference_filter == "West") {
        if (matchup_identifier == 1) { # Typically, winner of 1v8 vs winner of 4v5
          prev_winners$team1 <- get_series_winner(r1_west_results[["1v8"]], current_season_end_year, round_number_of_prereq_series)
          prev_winners$team2 <- get_series_winner(r1_west_results[["4v5"]], current_season_end_year, round_number_of_prereq_series)
        } else if (matchup_identifier == 2) { # Typically, winner of 3v6 vs winner of 2v7
          prev_winners$team1 <- get_series_winner(r1_west_results[["3v6"]], current_season_end_year, round_number_of_prereq_series)
          prev_winners$team2 <- get_series_winner(r1_west_results[["2v7"]], current_season_end_year, round_number_of_prereq_series)
        }
      } else if (conference_filter == "East") {
        if (matchup_identifier == 1) {
          prev_winners$team1 <- get_series_winner(r1_east_results[["1v8"]], current_season_end_year, round_number_of_prereq_series)
          prev_winners$team2 <- get_series_winner(r1_east_results[["4v5"]], current_season_end_year, round_number_of_prereq_series)
        } else if (matchup_identifier == 2) {
          prev_winners$team1 <- get_series_winner(r1_east_results[["3v6"]], current_season_end_year, round_number_of_prereq_series)
          prev_winners$team2 <- get_series_winner(r1_east_results[["2v7"]], current_season_end_year, round_number_of_prereq_series)
        }
      }
    } else if (round_num_filter == 3) { # Conference Finals
      round_number_of_prereq_series <- 2
      if (conference_filter == "West") {
        prev_winners$team1 <- get_series_winner(r2_west_results[[1]], current_season_end_year, round_number_of_prereq_series) # Winner of first West Conf. Semi
        prev_winners$team2 <- get_series_winner(r2_west_results[[2]], current_season_end_year, round_number_of_prereq_series) # Winner of second West Conf. Semi
      } else if (conference_filter == "East") {
        prev_winners$team1 <- get_series_winner(r2_east_results[[1]], current_season_end_year, round_number_of_prereq_series)
        prev_winners$team2 <- get_series_winner(r2_east_results[[2]], current_season_end_year, round_number_of_prereq_series)
      }
    } else if (round_num_filter == 4) { # NBA Finals
      round_number_of_prereq_series <- 3
      # Matchup identifier for Finals is typically 1
      if (matchup_identifier == 1) {
        prev_winners$team1 <- get_series_winner(r3_west_results[[1]], current_season_end_year, round_number_of_prereq_series) # Winner of West Conf. Finals
        prev_winners$team2 <- get_series_winner(r3_east_results[[1]], current_season_end_year, round_number_of_prereq_series) # Winner of East Conf. Finals
      }
    }
  }, error = function(e) {
    warning(paste("Error in get_series_for_slot while determining previous winners for R", round_num_filter,
                  conference_filter, "matchup", matchup_identifier, ":", e$message))
    # prev_winners will remain NULL for team1 and/or team2
  })
  
  # If both previous winners were determined, find their series in the current round's data
  if (!is.null(prev_winners$team1) && !is.null(prev_winners$team2)) {
    team1_name <- prev_winners$team1
    team2_name <- prev_winners$team2
    
    found_series <- filtered_data_for_round_conf %>%
      dplyr::filter(
        (high_seed_team_name == team1_name & low_seed_team_name == team2_name) |
          (high_seed_team_name == team2_name & low_seed_team_name == team1_name)
      )
    if (nrow(found_series) >= 1) return(found_series[1, , drop = FALSE])
  }
  
  return(NULL) # Return NULL if previous winners couldn't be determined or their matchup not found
}
# --- END: Section 1, Part 3: get_series_for_slot function ---

# --- START: Section 1, Part 4: create_team_html_new function ---

# To be placed in mod_welcome.R, after get_series_for_slot or in a sourced utility file.

# --- Helper function to create HTML for a single team in a matchup ---
create_team_html_new <- function(name, seed, wins = NULL, logo_url = NULL,
                                 is_winner = FALSE, is_tbd_team = FALSE) {
  
  team_classes <- "team"
  if (is_winner) {
    team_classes <- paste(team_classes, "winner") # Add 'winner' class if applicable
  }
  
  # If the team slot is TBD, render a placeholder
  if (is_tbd_team) {
    return(tags$div(class = "tbd-team-placeholder", "TBD"))
  }
  
  # Determine logo content: actual image or placeholder
  logo_content_html <- if (!is.null(logo_url) &&
                           nzchar(logo_url) &&
                           !grepl("fallback|placeholder|default_logo_path", logo_url, ignore.case = TRUE) &&
                           (startsWith(logo_url, "http") || startsWith(logo_url, "www_files/"))) {
    # Valid logo URL provided
    tags$div(class = "team-logo-container",
             tags$img(src = logo_url, class="actual-team-logo", alt = name %||% "Team Logo", loading = "lazy")
    )
  } else {
    # Fallback to a placeholder (e.g., Font Awesome icon or text)
    # Ensure Font Awesome is loaded in your app's UI if using an icon like below.
    # Or replace with a simple text placeholder like "LOGO".
    tags$div(class = "team-logo-placeholder", HTML("")) # Unicode for basketball icon (U+1F3C0)
  }
  
  # Construct the team div
  tags$div(class = team_classes,
           tags$span(class = "team-seed", seed %||% ""), # Display seed, or empty if NA/null
           logo_content_html,
           tags$span(class = "team-name", name %||% "TBD"), # Display team name, or "TBD"
           if (!is.null(wins) && !is.na(wins)) { # Display wins if available
             tags$span(class = "team-score", wins)
           } else {
             NULL # Don't display score span if wins is null/NA
           }
  )
}
# --- END: Section 1, Part 4: create_team_html_new function ---

# --- START: Section 1, Part 5: create_matchup_html_new function ---

# To be placed in mod_welcome.R, after create_team_html_new or in a sourced utility file.

# --- Helper function to create HTML for a matchup box ---
create_matchup_html_new <- function(series_data,
                                    is_tbd_slot = FALSE,
                                    team_logo_map, # Pass the reactive team_logo_mapping() result
                                    is_finals_matchup = FALSE, # Optional, for specific Finals styling if needed
                                    fallback_image_url_global # Pass the global fallback_image_url
) {
  
  # If the entire slot is TBD (no series data yet for this position in the bracket)
  # or if series_data is invalid, render a TBD placeholder for the whole matchup.
  if (is_tbd_slot || is.null(series_data) || !is.data.frame(series_data) || nrow(series_data) == 0) {
    return(
      tags$div(class = "tbd-slot-container", # Use specific class for styling TBD matchup boxes
               create_team_html_new(name = NULL, seed = NULL, wins = NULL, logo_url = NULL, is_tbd_team = TRUE),
               create_team_html_new(name = NULL, seed = NULL, wins = NULL, logo_url = NULL, is_tbd_team = TRUE)
      )
    )
  }
  
  # Extract the first row of series_data as a list
  series_row <- as.list(series_data[1, , drop = FALSE])
  
  # Determine if high seed or low seed is the winner based on series_status
  # This relies on series_status being correctly formatted (e.g., "Team Name Wins X-Y")
  # Escape special characters in team names if they might appear in regex
  escaped_hs_name <- gsub("([\\(\\)])", "\\\\\\1", series_row$high_seed_team_name %||% "") # Escape parentheses
  escaped_ls_name <- gsub("([\\(\\)])", "\\\\\\1", series_row$low_seed_team_name %||% "")
  
  is_hs_winner <- if (!is.na(series_row$series_status) && nzchar(escaped_hs_name)) {
    grepl(paste0("^", escaped_hs_name, " Wins"), series_row$series_status, perl = TRUE)
  } else { FALSE }
  
  is_ls_winner <- if (!is.na(series_row$series_status) && nzchar(escaped_ls_name)) {
    grepl(paste0("^", escaped_ls_name, " Wins"), series_row$series_status, perl = TRUE)
  } else { FALSE }
  
  # Get team logos from the map, using global fallback if not found
  hs_logo_url <- team_logo_map[[series_row$high_seed_team_name]] %||% fallback_image_url_global
  ls_logo_url <- team_logo_map[[series_row$low_seed_team_name]] %||% fallback_image_url_global
  
  matchup_div_class <- "matchup"
  # if (is_finals_matchup) { matchup_div_class <- paste(matchup_div_class, "finals-matchup") } # Optional class
  
  tags$div(class = matchup_div_class,
           create_team_html_new(name = series_row$high_seed_team_name,
                                seed = series_row$high_seed_seed,
                                wins = series_row$high_seed_wins,
                                logo_url = hs_logo_url,
                                is_winner = is_hs_winner,
                                is_tbd_team = (is.null(series_row$high_seed_team_name) || series_row$high_seed_team_name == "TBD")
           ),
           create_team_html_new(name = series_row$low_seed_team_name,
                                seed = series_row$low_seed_seed,
                                wins = series_row$low_seed_wins,
                                logo_url = ls_logo_url,
                                is_winner = is_ls_winner,
                                is_tbd_team = (is.null(series_row$low_seed_team_name) || series_row$low_seed_team_name == "TBD")
           ),
           # Display the series status text if available
           if (!is.null(series_row$series_status) && nzchar(series_row$series_status)) {
             tags$div(class = "series-result-info", series_row$series_status)
           } else {
             NULL
           }
  )
}
# --- END: Section 1, Part 5: create_matchup_html_new function ---

# --- START: Section 2, Part 1: fetch_realgm_playoff_bracket_for_season function ---

# To be placed in mod_welcome.R, likely after the playoff helper functions from Section 1.

# --- Fetch Playoff Bracket for a Specific Season (from RealGM) ---
fetch_realgm_playoff_bracket_for_season <- function(season_end_year) {
  # Ensure season_end_year is numeric for comparisons
  current_season_end_year_num <- suppressWarnings(as.numeric(season_end_year))
  
  print(paste0(Sys.time(), " [FETCH_BRACKET] Called for season_end_year: ", current_season_end_year_num))
  
  if (is.na(current_season_end_year_num) || current_season_end_year_num < 1947 || current_season_end_year_num > (as.numeric(format(Sys.Date(), "%Y")) + 3)) { # Allow a bit more future buffer
    print(paste0(Sys.time(), " [FETCH_BRACKET] ERROR: Invalid season_end_year: ", season_end_year))
    return(NULL)
  }
  
  target_url <- glue::glue("https://basketball.realgm.com/nba/playoffs/brackets/{current_season_end_year_num}")
  print(paste0(Sys.time(), " [FETCH_BRACKET] Target URL: ", target_url))
  
  tryCatch({
    response <- httr::GET(target_url,
                          httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"),
                          httr::timeout(25)) # Increased timeout
    
    print(paste0(Sys.time(), " [FETCH_BRACKET] httr::GET status for ", target_url, ": ", httr::status_code(response)))
    httr::stop_for_status(response, task = paste("fetch RealGM playoff bracket page for season", current_season_end_year_num))
    
    page_content <- httr::content(response, "text", encoding = "UTF-8") %>% rvest::read_html()
    print(paste0(Sys.time(), " [FETCH_BRACKET] Page content read for season: ", current_season_end_year_num))
    
    # Locate the main bracket table
    main_bracket_table <- page_content %>% rvest::html_element("div.bracket-container > div.bracket > table.bracket")
    
    if (is.na(main_bracket_table)) {
      print(paste0(Sys.time(), " [FETCH_BRACKET] (Season ", current_season_end_year_num, ") ERROR: Could not find 'table.bracket' on RealGM page."))
      return(NULL)
    }
    
    main_rows <- main_bracket_table %>% rvest::html_elements("tr")
    if (length(main_rows) == 0) {
      print(paste0(Sys.time(), " [FETCH_BRACKET] (Season ", current_season_end_year_num, ") ERROR: Found 'table.bracket' but it contains no 'tr' elements."))
      return(NULL)
    }
    
    all_series_data <- list()
    current_round_text_from_page <- "Unknown Round" # Stores the text directly from page
    
    for (i in 1:length(main_rows)) {
      row_node <- main_rows[[i]]
      
      # Update current_round_text_from_page if a new round header is found
      round_div_node <- row_node %>% rvest::html_element("div.round") # Check direct child first
      if (is.na(round_div_node)) { # If not direct, check if nested in a td (common structure)
        round_div_node <- row_node %>% rvest::html_element("td > div.round")
      }
      if (!is.na(round_div_node)) {
        new_round_text_candidate <- round_div_node %>% rvest::html_text(trim = TRUE)
        if (nzchar(new_round_text_candidate)) { # Only update if non-empty
          current_round_text_from_page <- new_round_text_candidate
        }
      }
      
      # Process series if a series table is found in the current row
      series_table_node <- row_node %>% rvest::html_element("table.bracket_game")
      if (!is.na(series_table_node)) {
        team_rows_nodes <- series_table_node %>% rvest::html_elements("tr")
        
        if (length(team_rows_nodes) == 2) { # Expecting two teams per series
          team_data_list <- list()
          for (j in 1:2) {
            team_row_node <- team_rows_nodes[[j]]
            
            seed_raw <- team_row_node %>% rvest::html_element("td.seed") %>% rvest::html_text(trim = TRUE) %||% NA_character_
            
            # Try to get team name from link first, then from plain text
            name_node_a <- team_row_node %>% rvest::html_element("td.name > a")
            team_name_val <- if (!is.na(name_node_a)) {
              rvest::html_text(name_node_a, trim = TRUE)
            } else {
              team_row_node %>% rvest::html_element("td.name") %>% rvest::html_text(trim = TRUE) %||% NA_character_
            }
            
            wins_raw <- team_row_node %>% rvest::html_element("td.score") %>% rvest::html_text(trim = TRUE) %||% "0"
            
            # Standardize TBD entries
            if (is.na(team_name_val) || nchar(team_name_val) == 0 || toupper(team_name_val) == "TBD") {
              team_name_val <- "TBD"
              seed_raw <- NA_character_ # No seed for TBD
              wins_raw <- "0"          # No wins for TBD
            }
            
            conf_letter <- stringr::str_sub(seed_raw, 1, 1) # Works even if seed_raw is NA
            seed_number_val <- suppressWarnings(as.integer(stringr::str_extract(seed_raw, "\\d+"))) # Extracts digits
            
            conference_val <- dplyr::case_when(
              conf_letter == "E" ~ "East",
              conf_letter == "W" ~ "West",
              TRUE ~ NA_character_ # If no E/W prefix, conference is initially NA
            )
            wins_val <- suppressWarnings(as.integer(wins_raw))
            if (is.na(wins_val)) wins_val <- 0 # Default to 0 if conversion fails
            
            team_data_list[[j]] <- list(name = team_name_val, seed = seed_number_val, conference = conference_val, wins = wins_val)
          }
          
          team1 <- team_data_list[[1]]
          team2 <- team_data_list[[2]]
          
          # Determine round_number based on current_round_text_from_page
          # This needs to be robust for historical variations.
          round_num_val <- dplyr::case_when(
            stringr::str_detect(tolower(current_round_text_from_page), "opening round|first round|conference quarterfinals") ~ 1L,
            stringr::str_detect(tolower(current_round_text_from_page), "conference semi-finals|conference semifinals|division semifinals") ~ 2L,
            stringr::str_detect(tolower(current_round_text_from_page), "conference finals|division finals") ~ 3L,
            stringr::str_detect(tolower(current_round_text_from_page), "nba finals") ~ 4L,
            TRUE ~ NA_integer_ # Default if no match
          )
          
          # Infer conference if not set by seed (e.g., for NBA Finals or if seeds are missing)
          series_conference_val <- team1$conference %||% team2$conference %||% NA_character_
          if (is.na(series_conference_val) && !is.na(current_round_text_from_page)) {
            if (stringr::str_detect(tolower(current_round_text_from_page), "western")) series_conference_val <- "West"
            if (stringr::str_detect(tolower(current_round_text_from_page), "eastern")) series_conference_val <- "East"
          }
          if (round_num_val == 4L || grepl("nba finals", tolower(current_round_text_from_page), fixed = TRUE)) {
            series_conference_val <- "Finals" # Standardize Finals conference
            if(is.na(round_num_val)) round_num_val <- 4L # Ensure Finals round number is 4
          }
          
          # Skip if essential info is missing or if both teams are TBD for this series slot
          if (is.na(round_num_val) || is.na(series_conference_val) || (team1$name == "TBD" && team2$name == "TBD")) {
            next
          }
          
          # Calculate series status using get_wins_needed
          wins_needed_for_series <- get_wins_needed(current_season_end_year_num, round_num_val)
          status_text_val <- "TBD" # Default status
          
          if (team1$name != "TBD" && team2$name != "TBD") { # Only calculate detailed status if both teams are known
            if (!is.na(team1$wins) && !is.na(team2$wins)) {
              if (team1$wins == wins_needed_for_series) {
                status_text_val <- paste0(team1$name, " Wins ", team1$wins, "-", team2$wins)
              } else if (team2$wins == wins_needed_for_series) {
                status_text_val <- paste0(team2$name, " Wins ", team2$wins, "-", team1$wins)
              } else if (team1$wins > 0 || team2$wins > 0) { # Series in progress
                if (team1$wins > team2$wins) status_text_val <- paste0(team1$name, " Leads ", team1$wins, "-", team2$wins)
                else if (team2$wins > team1$wins) status_text_val <- paste0(team2$name, " Leads ", team2$wins, "-", team1$wins)
                else status_text_val <- paste0("Series Tied ", team1$wins, "-", team2$wins)
              } else { # Both wins are 0
                status_text_val <- "Series 0-0"
              }
            }
          } else if (team1$name != "TBD" || team2$name != "TBD") { # One team known, other TBD
            status_text_val <- "Matchup Partially Set" # Or keep as "TBD"
          }
          # If both are TBD, status_text_val remains "TBD"
          
          # Determine high seed vs low seed for consistent storage
          hs_team_name_val <- team1$name; hs_wins_val <- team1$wins; hs_seed_val <- team1$seed
          ls_team_name_val <- team2$name; ls_wins_val <- team2$wins; ls_seed_val <- team2$seed
          
          # Only attempt reordering if both seeds are valid numbers
          if (!is.na(team1$seed) && !is.na(team2$seed) && team1$seed != team2$seed) {
            if (team1$seed > team2$seed) { # Higher number is lower seed
              hs_team_name_val <- team2$name; hs_wins_val <- team2$wins; hs_seed_val <- team2$seed
              ls_team_name_val <- team1$name; ls_wins_val <- team1$wins; ls_seed_val <- team1$seed
            }
          } else if (is.na(team1$seed) && !is.na(team2$seed) && team1$name == "TBD") {
            # If team1 is TBD (so seed is NA) and team2 has a seed, make team2 the high_seed for now.
            hs_team_name_val <- team2$name; hs_wins_val <- team2$wins; hs_seed_val <- team2$seed
            ls_team_name_val <- team1$name; ls_wins_val <- team1$wins; ls_seed_val <- team1$seed
          }
          # If both seeds are NA, or one is NA and that team is not TBD, or seeds are equal, original order is kept.
          
          all_series_data[[length(all_series_data) + 1]] <- tibble::tibble(
            round_number = round_num_val,
            conference = series_conference_val,
            high_seed_team_name = hs_team_name_val,
            high_seed_wins = as.integer(hs_wins_val),
            high_seed_seed = as.integer(hs_seed_val),
            low_seed_team_name = ls_team_name_val,
            low_seed_wins = as.integer(ls_wins_val),
            low_seed_seed = as.integer(ls_seed_val),
            series_status = status_text_val
          )
        }
      }
    } # End loop over main_rows
    
    if (length(all_series_data) > 0) {
      bracket_df <- dplyr::bind_rows(all_series_data) %>%
        dplyr::filter(!is.na(conference), !is.na(round_number)) %>% # Ensure core fields are not NA
        # Deduplicate: A series is unique by its round, conference, and the two teams involved (regardless of high/low seed order)
        dplyr::mutate(
          team_pair = purrr::map2_chr(high_seed_team_name, low_seed_team_name, ~ paste(sort(c(.x, .y)), collapse = " vs "))
        ) %>%
        dplyr::distinct(round_number, conference, team_pair, .keep_all = TRUE) %>%
        dplyr::select(-team_pair) # Remove temporary helper column
      
      attr(bracket_df, "season_scraped") <- current_season_end_year_num
      print(paste0(Sys.time(), " [FETCH_BRACKET] (Season ", current_season_end_year_num, ") SUCCESS: Processed ", nrow(bracket_df), " distinct series."))
      return(bracket_df)
    } else {
      print(paste0(Sys.time(), " [FETCH_BRACKET] (Season ", current_season_end_year_num, ") ERROR: No series data extracted after parsing."))
      return(NULL)
    }
    
  }, error = function(e) {
    print(paste0(Sys.time(), " [FETCH_BRACKET] (Season ", current_season_end_year_num, ") OVERALL TRY-CATCH ERROR: ", e$message))
    # print(rlang::trace_back()) # Uncomment for detailed traceback during debugging
    return(NULL)
  })
}
# --- END: Section 2, Part 1: fetch_realgm_playoff_bracket_for_season function ---

# --- START: Section 2, Part 2: Other Data Fetching Functions ---
# (fetch_nba_dot_com_news, fetch_scoreboard_data, fetch_standings_data, fetch_leader_category)

# --- Fetch NBA.com News ---
fetch_nba_dot_com_news <- function(num_headlines = 8) {
  target_url <- "https://www.nba.com/news"; base_url <- "https://www.nba.com"
  news_data <- NULL; fallback_image_url <- "www_files/nba-logo.png" # Ensure this path is correct relative to app root
  print(paste("MOD_WELCOME NEWS SCRAPE: Attempting fetch news from:", target_url))
  # List of potential selectors, try them in order
  selectors <- list(
    list(item = "div.ArticleTile_tile__y70gI", link = "article > a", headline = "h4.ArticleTile_tileTitle__aA8g7", image = "img.VideoThumbnail_image__fa2Go"), # Newer NBA.com structure
    list(item = ".default-tile", link = ".default-tile__link", headline = ".default-tile__headline", image = ".default-tile__image img") # Older potential structure
  )
  fetched_items <- NULL
  for (selector_set in selectors) {
    fetched_items <- tryCatch({
      response <- httr::GET(target_url, httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"), httr::timeout(20))
      httr::stop_for_status(response, task = "fetch NBA.com news page")
      page_content <- httr::content(response, "text", encoding = "UTF-8") %>% rvest::read_html()
      page_content %>% rvest::html_elements(selector_set$item)
    }, error = function(e) {
      warning(paste("MOD_WELCOME NEWS SCRAPE: Error during fetch/parse with a selector set:", e$message))
      NULL
    })
    
    if (!is.null(fetched_items) && length(fetched_items) > 0) {
      news_data <- fetched_items %>%
        purrr::map_dfr(function(item) {
          tryCatch({
            link_node <- item %>% rvest::html_element(selector_set$link)
            link <- if (!is.na(link_node)) rvest::html_attr(link_node, "href") else NA_character_
            headline_node <- item %>% rvest::html_element(selector_set$headline)
            headline <- if (!is.na(headline_node)) rvest::html_text(headline_node, trim = TRUE) else NA_character_
            img_node <- item %>% rvest::html_element(selector_set$image)
            image_url <- if (!is.na(img_node)) rvest::html_attr(img_node, "src") else NA_character_
            
            if (is.na(link) || is.na(headline) || nchar(stringr::str_trim(link)) == 0 || nchar(stringr::str_trim(headline)) < 5) {
              return(NULL) # Skip if essential info is missing
            }
            # Resolve relative URLs
            if (stringr::str_starts(link, "/")) {
              link <- paste0(base_url, link)
            } else if (!stringr::str_starts(link, "http")) {
              return(NULL) # Skip if not a full or relative URL
            }
            # Handle image URL (fallback if missing or not absolute)
            final_image_url <- if (is.na(image_url) || !stringr::str_starts(image_url, "http")) {
              fallback_image_url
            } else {
              image_url
            }
            tibble::tibble(headline = stringr::str_trim(headline), link = link, image_url = final_image_url)
          }, error = function(e_map) { NULL }) # Return NULL for items that error during mapping
        })
      
      # Ensure news_data is a dataframe and has unique links
      if (!is.null(news_data) && inherits(news_data, "data.frame") && "link" %in% names(news_data)) {
        news_data <- news_data %>% dplyr::distinct(link, .keep_all = TRUE)
      } else {
        news_data <- NULL # Reset if not valid
      }
      
      if (!is.null(news_data) && nrow(news_data) > 0) {
        news_data <- news_data %>% dplyr::slice_head(n = num_headlines)
        print(paste("MOD_WELCOME NEWS SCRAPE: Successfully fetched", nrow(news_data), "headlines."))
        return(news_data) # Return as soon as valid data is found
      }
    }
  } # End loop over selectors
  warning("MOD_WELCOME NEWS SCRAPE: Failed to fetch news using all available selectors.")
  return(NULL) # Return NULL if no selectors worked
}


# --- Fetch Scoreboard Data ---
fetch_scoreboard_data <- function(fetch_date) {
  current_date_string <- format(fetch_date, "%Y-%m-%d")
  current_date_espn <- format(fetch_date, "%Y%m%d")
  data_sb <- NULL
  v3_success <- FALSE
  espn_success <- FALSE
  error_log <- list(v3 = "Not attempted", espn = "Not attempted")
  
  # Attempt 1: hoopR::nba_scoreboardv3
  v3_result_sb <- tryCatch({
    hoopR::nba_scoreboardv3(game_date = current_date_string)
  }, error = function(e) {
    error_log$v3 <<- paste("API Call Error:", e$message)
    list(error = TRUE, message = e$message) # Return an error list
  })
  
  if (!is.list(v3_result_sb) || !isTRUE(v3_result_sb$error)) { # Check if it's NOT an error list
    if ("scoreboard" %in% names(v3_result_sb) && !is.null(v3_result_sb$scoreboard) &&
        is.data.frame(v3_result_sb$scoreboard) && nrow(v3_result_sb$scoreboard) > 0) {
      sb_data_v3 <- v3_result_sb$scoreboard
      req_cols_v3 <- c("game_id", "game_status_text", "game_et", "home_team_name", "home_team_tricode", "home_score", "away_team_name", "away_team_tricode", "away_score")
      opt_cols_v3 <- c("home_wins", "home_losses", "away_wins", "away_losses")
      
      if(!all(req_cols_v3 %in% names(sb_data_v3))) {
        error_log$v3 <- paste("Processing Error: Missing required columns in V3 data:", paste(setdiff(req_cols_v3, names(sb_data_v3)), collapse=", "))
      } else {
        has_records_v3 <- all(opt_cols_v3 %in% names(sb_data_v3))
        processed_v3 <- tryCatch({
          sb_data_v3 %>%
            dplyr::select(
              gameId = game_id, gameStatus = game_status_text, gameTime = game_et,
              homeTeamName = home_team_name, homeTeamAbbr = home_team_tricode, homeScore = home_score,
              awayTeamName = away_team_name, awayTeamAbbr = away_team_tricode, awayScore = away_score,
              dplyr::any_of(opt_cols_v3)
            ) %>%
            dplyr::mutate(
              homeLogo = glue::glue("https://cdn.nba.com/logos/nba/{homeTeamAbbr}/global/L/logo.svg"),
              awayLogo = glue::glue("https://cdn.nba.com/logos/nba/{awayTeamAbbr}/global/L/logo.svg"),
              homeRecord = if(has_records_v3 && "home_wins" %in% names(.) && "home_losses" %in% names(.)) paste0("(", home_wins, "-", home_losses, ")") else NA_character_,
              awayRecord = if(has_records_v3 && "away_wins" %in% names(.) && "away_losses" %in% names(.)) paste0("(", away_wins, "-", away_losses, ")") else NA_character_,
              dplyr::across(c(homeScore, awayScore), ~ suppressWarnings(as.integer(tidyr::replace_na(.x, NA))))
            ) %>%
            dplyr::select(gameId, gameStatus, gameTime, homeTeamName, homeTeamAbbr, homeScore, homeLogo, homeRecord,
                          awayTeamName, awayTeamAbbr, awayScore, awayLogo, awayRecord)
        }, error = function(e_proc) {
          error_log$v3 <<- paste("Processing Error (V3):", e_proc$message)
          NULL
        })
        if (!is.null(processed_v3)) { data_sb <- processed_v3; v3_success <- TRUE; error_log$v3 <- "Success"; }
      }
    } else { error_log$v3 <- "V3 API returned no game rows or unexpected data format." }
  }
  
  if (!v3_success) {
    espn_result_sb <- tryCatch({
      hoopR::espn_nba_scoreboard(season = current_date_espn)
    }, error = function(e) {
      error_log$espn <<- paste("API Call Error:", e$message)
      list(error = TRUE, message = e$message)
    })
    
    if (!is.list(espn_result_sb) || !isTRUE(espn_result_sb$error)) {
      if (is.data.frame(espn_result_sb) && nrow(espn_result_sb) > 0) {
        espn_data <- espn_result_sb
        req_cols_espn <- c("game_id", "status_name", "start_date", "home_team_name", "home_team_abb", "home_score", "home_team_logo", "away_team_name", "away_team_abb", "away_score", "away_team_logo")
        opt_cols_espn <- c("home_record", "away_record")
        
        if(!all(req_cols_espn %in% names(espn_data))) {
          error_log$espn <- paste("Processing Error: Missing required columns in ESPN data:", paste(setdiff(req_cols_espn, names(espn_data)), collapse=", "))
        } else {
          has_records_espn <- all(opt_cols_espn %in% names(espn_data))
          processed_espn <- tryCatch({
            espn_data %>%
              dplyr::select(
                gameId = game_id, gameStatus = status_name, gameTime = start_date,
                homeTeamName = home_team_name, homeTeamAbbr = home_team_abb, homeScore = home_score, homeLogo = home_team_logo,
                awayTeamName = away_team_name, awayTeamAbbr = away_team_abb, awayScore = away_score, awayLogo = away_team_logo,
                dplyr::any_of(opt_cols_espn)
              ) %>%
              dplyr::mutate(
                homeRecord = if(has_records_espn && "home_record" %in% names(.)) paste0("(", home_record, ")") else NA_character_,
                awayRecord = if(has_records_espn && "away_record" %in% names(.)) paste0("(", away_record, ")") else NA_character_,
                dplyr::across(c(homeScore, awayScore), ~ suppressWarnings(as.integer(tidyr::replace_na(.x, NA))))
              )  %>%
              dplyr::select(gameId, gameStatus, gameTime, homeTeamName, homeTeamAbbr, homeScore, homeLogo, homeRecord,
                            awayTeamName, awayTeamAbbr, awayScore, awayLogo, awayRecord)
          }, error = function(e_proc) {
            error_log$espn <<- paste("Processing Error (ESPN):", e_proc$message)
            NULL
          })
          if (!is.null(processed_espn)) { data_sb <- processed_espn; espn_success <- TRUE; error_log$espn <- "Success"; }
        }
      } else { error_log$espn <- "ESPN API returned no game rows or unexpected data format." }
    }
  }
  
  if (v3_success || espn_success) {
    return(list(data = data_sb, error = NULL))
  } else {
    is_v3_no_data_msg <- grepl("no scoreboard v3 data|object 'games' not found", error_log$v3, ignore.case = TRUE)
    is_espn_no_data_msg <- grepl("no scoreboard data available", error_log$espn, ignore.case = TRUE)
    if (is_v3_no_data_msg && (is_espn_no_data_msg || error_log$espn == "Not attempted" || grepl("API Call Error", error_log$espn, ignore.case = TRUE)) ) {
      return(list(data = data.frame(), error = NULL))
    } else {
      final_error_msg <- glue::glue("Scoreboard data unavailable. V3 API status: [{error_log$v3}]. ESPN API status: [{error_log$espn}].")
      return(list(data = NULL, error = final_error_msg))
    }
  }
}

# --- Fetch Standings Data ---
fetch_standings_data <- function(season_slug_to_fetch = "current", fetch_date_for_current_calc = Sys.Date()) {
  standings_data <- NULL; final_error_msg <- NULL
  is_requesting_current <- (season_slug_to_fetch == "current")
  target_season_slug <- season_slug_to_fetch
  previous_season_slug <- NULL
  
  if (is_requesting_current) {
    current_year_st <- as.numeric(format(fetch_date_for_current_calc, "%Y"))
    current_month_st <- as.numeric(format(fetch_date_for_current_calc, "%m"))
    target_season_end_year <- if (current_month_st >= 10) current_year_st + 1 else current_year_st
    target_season_slug <- hoopR::year_to_season(target_season_end_year)
    previous_season_slug <- hoopR::year_to_season(target_season_end_year - 1)
  }
  
  fetch_and_process_one_season <- function(season_slug) {
    local_error <- NULL; local_data <- NULL
    standings_raw <- tryCatch({
      hoopR::nba_leaguestandingsv3(season = season_slug, season_type = "Regular Season")
    }, error = function(e) {
      local_error <<- paste("API Call Error:", e$message); list(error = TRUE, message = e$message)
    })
    if (!is.list(standings_raw) || !isTRUE(standings_raw$error)) {
      if ("Standings" %in% names(standings_raw) && is.data.frame(standings_raw$Standings) && nrow(standings_raw$Standings) > 0) {
        standings_df_st <- standings_raw$Standings
        expected_cols_st <- c("TeamID", "TeamName", "TeamCity", "TeamSlug", "Conference", "PlayoffRank", "WINS", "LOSSES", "WinPCT", "ConferenceGamesBack", "ConferenceRecord", "HOME", "ROAD", "L10", "CurrentStreak")
        if(all(expected_cols_st %in% names(standings_df_st))) {
          proc_result <- tryCatch({
            standings_df_st %>%
              dplyr::select(teamId = TeamID, teamName = TeamName, teamCity = TeamCity, teamAbbr = TeamSlug, conf = Conference, confRank = PlayoffRank, W = WINS, L = LOSSES, Pct = WinPCT, GB = ConferenceGamesBack, confRecord = ConferenceRecord, homeRecord = HOME, awayRecord = ROAD, L10 = L10, Strk = CurrentStreak) %>%
              dplyr::mutate(dplyr::across(c(W, L, Pct, GB), ~ suppressWarnings(as.numeric(.x))), dplyr::across(c(confRank), ~ suppressWarnings(as.integer(.x))), GB = dplyr::case_when(is.na(confRank) ~ NA_real_, confRank == 1 ~ 0.0, TRUE ~ GB)) %>%
              dplyr::arrange(conf, confRank)
          }, error = function(e_proc){ local_error <<- paste("Processing Error:", e_proc$message); NULL })
          if (!is.null(proc_result)) { local_data <- proc_result; local_error <- NULL }
        } else { local_error <- "Processing Error: Missing expected columns in standings." }
      } else { local_error <- "API returned 0 rows or unexpected format for standings." }
    }
    list(data = local_data, error = local_error, season_slug = season_slug)
  }
  
  result1 <- fetch_and_process_one_season(target_season_slug)
  result_final <- result1
  if (is_requesting_current && !is.null(result1$error) && !is.null(previous_season_slug)) {
    result2 <- fetch_and_process_one_season(previous_season_slug)
    if (is.null(result2$error)) { result_final <- result2 }
  }
  if (!is.null(result_final$data)) {
    standings_data <- result_final$data; final_error_msg <- NULL
    attr(standings_data, "season_slug") <- result_final$season_slug
  } else {
    error_msg <- glue::glue("Standings unavailable for {ifelse(is_requesting_current, 'current season', target_season_slug)}.")
    if (!is.null(result1$error)) { error_msg <- glue::glue("{error_msg} Primary attempt ({target_season_slug}): [{result1$error}].") }
    if (exists("result2", inherits = FALSE) && !is.null(result2$error)) { error_msg <- glue::glue("{error_msg} Fallback attempt ({previous_season_slug}): [{result2$error}].") }
    final_error_msg <- error_msg
  }
  return(list(data = standings_data, error = final_error_msg))
}

# --- Fetch League Leaders ---
fetch_leader_category <- function(stat_category, rv_target, fetch_date_for_current_calc, season_slug_override = NULL) {
  processed_leaders <- NULL; final_season_slug_ld <- NULL; final_error_msg <- NULL
  error_attempt1_ld <- "Attempt not made or processing failed."; error_attempt2_ld <- "Attempt not made or processing failed."
  expected_cols_ld <- c("PLAYER_ID", "RANK", "PLAYER", "TEAM")
  is_requesting_current_ld <- is.null(season_slug_override) || season_slug_override == "current"
  target_season_slug_to_try <- season_slug_override
  previous_season_slug_for_fallback_ld <- NULL
  
  if (is_requesting_current_ld) {
    current_year_ld <- as.numeric(format(fetch_date_for_current_calc, "%Y")); current_month_ld <- as.numeric(format(fetch_date_for_current_calc, "%m"))
    target_season_end_year_ld <- if (current_month_ld >= 10) current_year_ld + 1 else current_year_ld
    target_season_slug_to_try <- hoopR::year_to_season(target_season_end_year_ld)
    previous_season_slug_for_fallback_ld <- hoopR::year_to_season(target_season_end_year_ld - 1)
  } else {
    if (is.null(target_season_slug_to_try) || nchar(target_season_slug_to_try) == 0) {
      rv_target$error <- glue::glue("Invalid season for {stat_category} leaders."); rv_target$last_fetch_time <- Sys.time(); return()
    }
  }
  leaders_result_raw <- tryCatch({ hoopR::nba_leagueleaders(season = target_season_slug_to_try, stat_category = stat_category, season_type = "Regular Season", per_mode = "PerGame") }, error = function(e) { error_attempt1_ld <<- paste("API Call Error:", e$message); list(error = TRUE, message = e$message) })
  if (!is.list(leaders_result_raw) || !isTRUE(leaders_result_raw$error)) {
    if ("LeagueLeaders" %in% names(leaders_result_raw) && is.data.frame(leaders_result_raw$LeagueLeaders) && nrow(leaders_result_raw$LeagueLeaders) > 0) {
      leaders_df <- leaders_result_raw$LeagueLeaders
      if ( (stat_category %in% names(leaders_df)) && all(expected_cols_ld %in% names(leaders_df)) ) {
        proc_result_ld <- tryCatch({ leaders_df %>% dplyr::slice_head(n = 5) %>% dplyr::select(playerId = PLAYER_ID, Rank = RANK, Player = PLAYER, Team = TEAM, Value = !!rlang::sym(stat_category)) %>% dplyr::mutate(Value = suppressWarnings(as.numeric(Value))) }, error = function(e_proc) { error_attempt1_ld <<- paste("Processing Error:", e_proc$message); NULL })
        if (!is.null(proc_result_ld)) { processed_leaders <- proc_result_ld; final_season_slug_ld <- target_season_slug_to_try; error_attempt1_ld <- "Success" }
      } else { error_attempt1_ld <- "Processing Error: Missing expected columns in leaders." }
    } else { error_attempt1_ld <- "API returned 0 rows or unexpected format for leaders." }
  }
  if (is.null(processed_leaders) && is_requesting_current_ld && !is.null(previous_season_slug_for_fallback_ld)) {
    leaders_result_raw_prev <- tryCatch({ hoopR::nba_leagueleaders(season = previous_season_slug_for_fallback_ld, stat_category = stat_category, season_type = "Regular Season", per_mode = "PerGame") }, error = function(e) { error_attempt2_ld <<- paste("API Call Error (Fallback):", e$message); list(error = TRUE, message = e$message) })
    if (!is.list(leaders_result_raw_prev) || !isTRUE(leaders_result_raw_prev$error)) {
      if ("LeagueLeaders" %in% names(leaders_result_raw_prev) && is.data.frame(leaders_result_raw_prev$LeagueLeaders) && nrow(leaders_result_raw_prev$LeagueLeaders) > 0) {
        leaders_df_prev <- leaders_result_raw_prev$LeagueLeaders
        if ( (stat_category %in% names(leaders_df_prev)) && all(expected_cols_ld %in% names(leaders_df_prev)) ) {
          proc_result_ld_prev <- tryCatch({ leaders_df_prev %>% dplyr::slice_head(n = 5) %>% dplyr::select(playerId = PLAYER_ID, Rank = RANK, Player = PLAYER, Team = TEAM, Value = !!rlang::sym(stat_category)) %>% dplyr::mutate(Value = suppressWarnings(as.numeric(Value))) }, error = function(e_proc) { error_attempt2_ld <<- paste("Processing Error (Fallback):", e_proc$message); NULL })
          if (!is.null(proc_result_ld_prev)) { processed_leaders <- proc_result_ld_prev; final_season_slug_ld <- previous_season_slug_for_fallback_ld; error_attempt2_ld <- "Success" }
        } else { error_attempt2_ld <- "Processing Error: Missing expected columns (Fallback)." }
      } else { error_attempt2_ld <- "API returned 0 rows or unexpected format (Fallback)." }
    }
  }
  if (!is.null(processed_leaders)) {
    rv_target$data <- processed_leaders; rv_target$error <- NULL
    attr(rv_target$data, "season_slug") <- final_season_slug_ld
  } else {
    rv_target$data <- NULL
    error_msg_final <- glue::glue("{stat_category} leaders unavailable. ")
    if (is_requesting_current_ld) { error_msg_final <- glue::glue("{error_msg_final}Primary ('{target_season_slug_to_try}'): [{error_attempt1_ld}]. Fallback ('{previous_season_slug_for_fallback_ld %||% 'N/A'}'): [{error_attempt2_ld}].")
    } else { error_msg_final <- glue::glue("{error_msg_final}Attempt for '{target_season_slug_to_try}': [{error_attempt1_ld}].") }
    rv_target$error <- error_msg_final
  }
  rv_target$last_fetch_time <- Sys.time()
}
# --- END: Section 2, Part 2: Other Data Fetching Functions ---

# --- START: Section 3: UI Function (mod_welcome_ui) ---

# ==========================================================================
# UI Function
# ==========================================================================
mod_welcome_ui <- function(id){
  ns <- NS(id) # Namespace function
  tagList(
    # --- Row 1: Latest News ---
    fluidRow(
      column(width = 12,
             h4("Latest NBA News", style = "color: #ffffff; border-bottom: 1px solid #444; padding-bottom: 5px; margin-top: 15px; margin-bottom: 15px;"),
             # Spinner for news loading
             shinycssloaders::withSpinner(uiOutput(ns("news_output")), type=6, color="#1d428a")
      )
    ),
    tags$hr(style="margin-top: 0px; margin-bottom: 20px;"), # Horizontal rule after news
    
    # --- Row 2: Scoreboard (Left) and Tabs (Right) ---
    fluidRow(
      # --- Column 1: Scoreboard ---
      column(width = 4,
             # Date Navigation for Scoreboard
             div(class = "date-nav-container",
                 actionButton(ns("prev_day"), label = NULL, icon = icon("angle-left"), class = "date-nav-btn"),
                 tags$div(class="date-display-wrapper",
                          shinycssloaders::withSpinner(uiOutput(ns("current_date_display"), inline = TRUE), type=7, color="#cccccc", size=0.6)
                 ),
                 actionButton(ns("next_day"), label = NULL, icon = icon("angle-right"), class = "date-nav-btn"),
                 # Hidden dateInput for programmatic control if needed, or for direct selection by user (though not primary)
                 tags$div(style = "display: none;", dateInput(ns("select_date"), label = NULL, value = Sys.Date() - 1))
             ),
             h4("Games", style = "color: #ffffff; border-bottom: 1px solid #444; padding-bottom: 5px; margin-top: 15px; margin-bottom: 15px;"),
             # Spinner for scoreboard loading
             shinycssloaders::withSpinner(uiOutput(ns("scoreboard_output")), type=7, color="#cccccc")
      ),
      
      # --- Column 2: Tabset for Standings, Leaders, Playoffs ---
      column(width = 8,
             tabsetPanel(
               id = ns("welcome_subtabs"), # ID for the tabset
               type = "pills",            # Style of tabs
               
               # --- Standings Sub-Tab ---
               tabPanel(title = "Standings", value = ns("subtab_standings"),
                        br(), # Space below tab title
                        fluidRow(
                          column(width = 6, offset = 3, # Center the dropdown
                                 selectInput(ns("standings_season_select"),
                                             label = tags$strong("View Standings for Season:"),
                                             choices = c("Loading..." = ""), # Populated by server
                                             selected = "current", # Default value
                                             width = "100%")
                          )
                        ),
                        tags$hr(style="margin-top: 5px; margin-bottom: 15px;"),
                        shinycssloaders::withSpinner(uiOutput(ns("standings_output")), type=7, color="#cccccc")
               ),
               
               # --- Leaders Sub-Tab ---
               tabPanel(title = "Leaders", value = ns("subtab_leaders"),
                        br(),
                        fluidRow(
                          column(width = 6, offset = 3, # Center the dropdown
                                 selectInput(ns("leaders_season_select"),
                                             label = tags$strong("View Leaders for Season:"),
                                             choices = c("Loading..." = ""), # Populated by server
                                             selected = "current", # Default value
                                             width = "100%")
                          )
                        ),
                        tags$hr(style="margin-top: 5px; margin-bottom: 15px;"),
                        fluidRow(
                          column(6,
                                 shinycssloaders::withSpinner(uiOutput(ns("leaders_pts_output")), type=7, color="#cccccc", size=0.8), tags$br(),
                                 shinycssloaders::withSpinner(uiOutput(ns("leaders_reb_output")), type=7, color="#cccccc", size=0.8), tags$br(),
                                 shinycssloaders::withSpinner(uiOutput(ns("leaders_stl_output")), type=7, color="#cccccc", size=0.8)
                          ),
                          column(6,
                                 shinycssloaders::withSpinner(uiOutput(ns("leaders_ast_output")), type=7, color="#cccccc", size=0.8), tags$br(),
                                 shinycssloaders::withSpinner(uiOutput(ns("leaders_blk_output")), type=7, color="#cccccc", size=0.8)
                                 # Add more leader categories here if needed
                          )
                        )
               ),
               
               # --- Playoffs Sub-Tab ---
               tabPanel(title = "Playoffs", value = ns("subtab_playoffs"),
                        br(),
                        fluidRow(
                          column(width = 6, offset = 3, # Center the dropdown
                                 selectInput(ns("playoffs_season_select"),
                                             label = tags$strong("View Playoff Bracket for Season:"),
                                             choices = c("Loading..." = ""), # Populated by server
                                             # Selected value will be handled by server initialization
                                             width = "100%")
                          )
                        ),
                        tags$hr(style="margin-top: 5px; margin-bottom: 15px;"),
                        shinycssloaders::withSpinner(uiOutput(ns("scraped_bracket_output")), type=7, color="#cccccc")
               )
             ) # End tabsetPanel
      ) # End Column 2
    ) # End Row 2
  ) # End tagList
}
# --- END: Section 3: UI Function (mod_welcome_ui) ---

# --- START: Section 4: Server Function (mod_welcome_server) - Part 1: Reactive Values & Initializations ---

# ==========================================================================
# Server Function
# ==========================================================================
mod_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns # Namespace function
    
    # --- Reactive Values for Storing Data and State ---
    # Scoreboard
    rv_sb <- reactiveValues(
      data = NULL,            # Stores scoreboard data for selected_date
      error = NULL,           # Stores error message if fetch fails
      last_fetch_time = NULL  # Timestamp of the last fetch attempt
    )
    # Standings
    rv_st <- reactiveValues(
      data = NULL,            # Stores standings data for selected season
      error = NULL,
      last_fetch_time = NULL,
      available_seasons = NULL, # Stores choices for standings_season_select
      dropdown_initialized = FALSE # Flag for standings dropdown
    )
    # Leaders Dropdown (common for all leader categories)
    rv_ld_dropdown <- reactiveValues(
      initialized = FALSE     # Flag for leaders_season_select
    )
    # Playoffs Dropdown
    rv_playoffs_dropdown <- reactiveValues(
      initialized = FALSE     # Flag for playoffs_season_select
    )
    # Individual Leader Categories
    rv_ld_pts <- reactiveValues(data = NULL, error = NULL, last_fetch_time = NULL)
    rv_ld_ast <- reactiveValues(data = NULL, error = NULL, last_fetch_time = NULL)
    rv_ld_reb <- reactiveValues(data = NULL, error = NULL, last_fetch_time = NULL)
    rv_ld_blk <- reactiveValues(data = NULL, error = NULL, last_fetch_time = NULL)
    rv_ld_stl <- reactiveValues(data = NULL, error = NULL, last_fetch_time = NULL)
    # News
    rv_news <- reactiveValues(
      data = NULL,
      error = NULL,
      status = "loading", # Can be "loading", "success", "error"
      last_fetch_time = NULL
    )
    # Playoff Bracket (Scraped from RealGM)
    rv_playoff_bracket_scraped <- reactiveValues(
      data = NULL,            # Stores the tibble from fetch_realgm_playoff_bracket_for_season
      error = NULL,
      last_fetch_time = NULL
    )
    
    # --- Other Reactive Elements and Module-Scoped Variables ---
    selected_date <- reactiveVal(Sys.Date() - 1) # For scoreboard, default to yesterday
    headshot_cache <- reactiveValues()             # Cache for player headshots (used in leaders)
    
    # Fallback image URL (ensure this path is correct relative to www/ if not using addResourcePath prefix)
    # If addResourcePath('www_files', 'www') is used in global.R, then "www_files/nba-logo.png" is correct.
    fallback_image_url <- "www_files/nba-logo.png"
    
    # Team Logo Mapping (centralized)
    team_logo_mapping <- reactive({
      list(
        "Atlanta Hawks" = "https://cdn.nba.com/logos/nba/1610612737/global/L/logo.svg",
        "Boston Celtics" = "https://cdn.nba.com/logos/nba/1610612738/global/L/logo.svg",
        "Brooklyn Nets" = "https://cdn.nba.com/logos/nba/1610612751/global/L/logo.svg",
        "Charlotte Hornets" = "https://cdn.nba.com/logos/nba/1610612766/global/L/logo.svg",
        "Chicago Bulls" = "https://cdn.nba.com/logos/nba/1610612741/global/L/logo.svg",
        "Cleveland Cavaliers" = "https://cdn.nba.com/logos/nba/1610612739/global/L/logo.svg",
        "Dallas Mavericks" = "https://cdn.nba.com/logos/nba/1610612742/global/L/logo.svg",
        "Denver Nuggets" = "https://cdn.nba.com/logos/nba/1610612743/global/L/logo.svg",
        "Detroit Pistons" = "https://cdn.nba.com/logos/nba/1610612765/global/L/logo.svg",
        "Golden State Warriors" = "https://cdn.nba.com/logos/nba/1610612744/global/L/logo.svg",
        "Houston Rockets" = "https://cdn.nba.com/logos/nba/1610612745/global/L/logo.svg",
        "Indiana Pacers" = "https://cdn.nba.com/logos/nba/1610612754/global/L/logo.svg",
        "Los Angeles Clippers" = "https://cdn.nba.com/logos/nba/1610612746/global/L/logo.svg",
        "Los Angeles Lakers" = "https://cdn.nba.com/logos/nba/1610612747/global/L/logo.svg",
        "Memphis Grizzlies" = "https://cdn.nba.com/logos/nba/1610612763/global/L/logo.svg",
        "Miami Heat" = "https://cdn.nba.com/logos/nba/1610612748/global/L/logo.svg",
        "Milwaukee Bucks" = "https://cdn.nba.com/logos/nba/1610612749/global/L/logo.svg",
        "Minnesota Timberwolves" = "https://cdn.nba.com/logos/nba/1610612750/global/L/logo.svg",
        "New Orleans Pelicans" = "https://cdn.nba.com/logos/nba/1610612740/global/L/logo.svg",
        "New York Knicks" = "https://cdn.nba.com/logos/nba/1610612752/global/L/logo.svg",
        "Oklahoma City Thunder" = "https://cdn.nba.com/logos/nba/1610612760/global/L/logo.svg",
        "Orlando Magic" = "https://cdn.nba.com/logos/nba/1610612753/global/L/logo.svg",
        "Philadelphia 76ers" = "https://cdn.nba.com/logos/nba/1610612755/global/L/logo.svg",
        "Phoenix Suns" = "https://cdn.nba.com/logos/nba/1610612756/global/L/logo.svg",
        "Portland Trail Blazers" = "https://cdn.nba.com/logos/nba/1610612757/global/L/logo.svg",
        "Sacramento Kings" = "https://cdn.nba.com/logos/nba/1610612758/global/L/logo.svg",
        "San Antonio Spurs" = "https://cdn.nba.com/logos/nba/1610612759/global/L/logo.svg",
        "Toronto Raptors" = "https://cdn.nba.com/logos/nba/1610612761/global/L/logo.svg",
        "Utah Jazz" = "https://cdn.nba.com/logos/nba/1610612762/global/L/logo.svg",
        "Washington Wizards" = "https://cdn.nba.com/logos/nba/1610612764/global/L/logo.svg"
      )
    })
    
    # --- Date Navigation Observers for Scoreboard ---
    observeEvent(input$prev_day, {
      selected_date(selected_date() - 1)
      updateDateInput(session, "select_date", value = selected_date())
    })
    observeEvent(input$next_day, {
      selected_date(selected_date() + 1)
      updateDateInput(session, "select_date", value = selected_date())
    })
    observeEvent(input$select_date, {
      req(input$select_date)
      if (!identical(input$select_date, selected_date())) {
        selected_date(input$select_date)
      }
    }, ignoreInit = TRUE)
    
    # --- Headshot Helper Function (for Leaders) ---
    get_player_headshot_url <- function(player_id) {
      req(player_id)
      cache_key <- as.character(player_id)
      
      if (!is.null(headshot_cache[[cache_key]])) {
        return(headshot_cache[[cache_key]])
      }
      
      default_headshot <- "https://cdn.nba.com/headshots/nba/latest/260x190/fallback.png"
      
      url <- tryCatch({
        hoopR::nba_playerheadshot(player_id = as.numeric(player_id))
      }, error = function(e) {
        warning(paste("Error fetching headshot for player_id", player_id, ":", e$message))
        NULL
      })
      
      final_url <- if (!is.null(url) && grepl("^http", url)) {
        url
      } else {
        default_headshot
      }
      headshot_cache[[cache_key]] <- final_url
      return(final_url)
    }
    
    # --- One-time Initialization for ALL Season Dropdowns ---
    init_done <- reactiveVal(FALSE)
    observe({
      req(!init_done())
      print(paste0(Sys.time(), " [MOD_WELCOME_INIT] One-time: Initializing ALL season dropdowns..."))
      
      tryCatch({
        current_calendar_year <- as.numeric(format(Sys.Date(), "%Y"))
        current_calendar_month <- as.numeric(format(Sys.Date(), "%m"))
        
        latest_season_end_year <- if (current_calendar_month >= 10) {
          current_calendar_year + 1
        } else {
          current_calendar_year
        }
        
        # --- Populate Standings and Leaders Dropdowns ---
        # ***** MODIFICATION FOR STANDINGS/LEADERS IS HERE *****
        first_historical_season_end_year_st_ld <- 1972 # Earliest season for Standings/Leaders is 1971-72 (ends in 1972)
        # ***** END OF MODIFICATION FOR STANDINGS/LEADERS *****
        
        season_end_years_st_ld <- if (latest_season_end_year >= first_historical_season_end_year_st_ld) {
          seq(from = latest_season_end_year, to = first_historical_season_end_year_st_ld, by = -1)
        } else {
          latest_season_end_year
        }
        
        historical_season_slugs_st_ld <- if (length(season_end_years_st_ld) > 0) {
          purrr::map_chr(season_end_years_st_ld, function(year) {
            tryCatch(hoopR::year_to_season(year), error = function(e) NA_character_)
          })
        } else {
          character(0)
        }
        historical_season_slugs_st_ld <- historical_season_slugs_st_ld[!is.na(historical_season_slugs_st_ld) & nzchar(historical_season_slugs_st_ld)]
        
        available_season_choices_st_ld <- stats::setNames(
          as.list(c("current", historical_season_slugs_st_ld)),
          c("Current Season", paste(historical_season_slugs_st_ld, "Season"))
        )
        
        if (length(available_season_choices_st_ld) > 0) {
          updateSelectInput(session, "standings_season_select", choices = available_season_choices_st_ld, selected = "current")
          updateSelectInput(session, "leaders_season_select", choices = available_season_choices_st_ld, selected = "current")
          rv_st$available_seasons <- available_season_choices_st_ld
          rv_st$dropdown_initialized <- TRUE
          rv_ld_dropdown$initialized <- TRUE
          print(paste0(Sys.time(), " [MOD_WELCOME_INIT] Standings/Leaders dropdowns initialized."))
        } else {
          warning("MOD_WELCOME WARNING: [MOD_WELCOME_INIT] No season choices generated for Standings/Leaders dropdowns.")
          updateSelectInput(session, "standings_season_select", choices = c("Error loading" = ""), selected = "")
          updateSelectInput(session, "leaders_season_select", choices = c("Error loading" = ""), selected = "")
        }
        
        # --- Populate Playoffs Dropdown ---
        # ***** MODIFICATION FOR PLAYOFFS IS HERE *****
        first_playoff_season_end_year <- 1984 # Earliest season to show is 1983-84 (ends in 1984)
        # ***** END OF MODIFICATION FOR PLAYOFFS *****
        
        playoff_season_end_years <- if (latest_season_end_year >= first_playoff_season_end_year) {
          seq(from = latest_season_end_year, to = first_playoff_season_end_year, by = -1)
        } else {
          latest_season_end_year
        }
        
        if (length(playoff_season_end_years) > 0) {
          playoff_season_choices_list <- stats::setNames(
            as.list(playoff_season_end_years),
            paste0(playoff_season_end_years - 1, "-", stringr::str_sub(as.character(playoff_season_end_years), -2), " Season")
          )
          default_playoff_selection <- if (latest_season_end_year < first_playoff_season_end_year && length(playoff_season_end_years) > 0) {
            playoff_season_end_years[1]
          } else if (latest_season_end_year >= first_playoff_season_end_year) {
            latest_season_end_year
          } else {
            NULL
          }
          
          if(!is.null(default_playoff_selection)){
            updateSelectInput(session, "playoffs_season_select", choices = playoff_season_choices_list, selected = default_playoff_selection)
            print(paste0(Sys.time(), " [MOD_WELCOME_INIT] Playoff season dropdown initialized. Defaulting to ", default_playoff_selection, "."))
          } else {
            updateSelectInput(session, "playoffs_season_select", choices = c("No valid seasons" = ""), selected = "")
            warning("MOD_WELCOME WARNING: [MOD_WELCOME_INIT] No valid default selection for playoff seasons.")
          }
          rv_playoffs_dropdown$initialized <- TRUE
        } else {
          warning("MOD_WELCOME WARNING: [MOD_WELCOME_INIT] No playoff season choices generated for dropdown after year adjustment.")
          updateSelectInput(session, "playoffs_season_select", choices = c("Error loading seasons" = ""), selected = "")
        }
        
        init_done(TRUE)
        print(paste0(Sys.time(), " [MOD_WELCOME_INIT] ALL Season dropdowns initialization attempt complete."))
        
      }, error = function(e) {
        print(paste0(Sys.time(), " [MOD_WELCOME_INIT] ERROR during one-time dropdown population: ", e$message))
        updateSelectInput(session, "standings_season_select", choices = c("Init Error" = ""), selected = "")
        updateSelectInput(session, "leaders_season_select", choices = c("Init Error" = ""), selected = "")
        updateSelectInput(session, "playoffs_season_select", choices = c("Init Error" = ""), selected = "")
        init_done(TRUE)
      })
    })
    # --- END: Section 4: Server Function (mod_welcome_server) - Part 1: Reactive Values & Initializations ---
    
    # --- START: Section 5: Server Function (mod_welcome_server) - Part 2: Data Fetching Observers ---
    
    auto_refresh_timer <- reactiveTimer(900000) # 15 minutes
    
    observeEvent(auto_refresh_timer(), {
      print(paste0(Sys.time(), " [AUTO_REFRESH] Auto-refresh triggered for Welcome Tab data."))
      current_system_date <- Sys.Date()
      
      news_result <- fetch_nba_dot_com_news(num_headlines = 10)
      if(!is.null(news_result) && is.data.frame(news_result) && nrow(news_result) > 0){
        rv_news$data <- news_result
        rv_news$error <- NULL
        rv_news$status <- "success"
      } else {
        if (is.null(isolate(rv_news$data))) {
          rv_news$error <- "Failed to fetch news headlines or no news items were found."
          rv_news$status <- "error"
        } else {
          print("MOD_WELCOME NEWS (Auto-Refresh): Fetch failed, retaining previously fetched data.")
        }
      }
      rv_news$last_fetch_time <- Sys.time()
      
      standings_season_to_fetch <- isolate(input$standings_season_select %||% "current")
      st_result <- fetch_standings_data(season_slug_to_fetch = standings_season_to_fetch, fetch_date_for_current_calc = current_system_date)
      rv_st$data <- st_result$data
      rv_st$error <- st_result$error
      rv_st$last_fetch_time <- Sys.time()
      
      leader_season_slug_to_fetch <- isolate(input$leaders_season_select %||% "current")
      fetch_leader_category("PTS", rv_ld_pts, current_system_date, leader_season_slug_to_fetch)
      fetch_leader_category("AST", rv_ld_ast, current_system_date, leader_season_slug_to_fetch)
      fetch_leader_category("REB", rv_ld_reb, current_system_date, leader_season_slug_to_fetch)
      fetch_leader_category("BLK", rv_ld_blk, current_system_date, leader_season_slug_to_fetch)
      fetch_leader_category("STL", rv_ld_stl, current_system_date, leader_season_slug_to_fetch)
      
      print(paste0(Sys.time(), " [AUTO_REFRESH_PLAYOFFS] Attempting to fetch CURRENT Playoff Bracket for auto-update..."))
      current_calendar_year_for_refresh <- as.numeric(format(Sys.Date(), "%Y"))
      current_calendar_month_for_refresh <- as.numeric(format(Sys.Date(), "%m"))
      current_nba_season_end_year_for_refresh <- if (current_calendar_month_for_refresh >= 10) {
        current_calendar_year_for_refresh + 1
      } else {
        current_calendar_year_for_refresh
      }
      print(paste0(Sys.time(), " [AUTO_REFRESH_PLAYOFFS] Determined current_nba_season_end_year for refresh: ", current_nba_season_end_year_for_refresh))
      
      current_season_bracket_data <- fetch_realgm_playoff_bracket_for_season(season_end_year = current_nba_season_end_year_for_refresh)
      
      user_selected_playoff_year_char_rf <- isolate(input$playoffs_season_select)
      is_user_selection_valid_rf <- !is.null(user_selected_playoff_year_char_rf) &&
        nzchar(user_selected_playoff_year_char_rf) &&
        grepl("^[0-9]+$", user_selected_playoff_year_char_rf) &&
        isTRUE(isolate(rv_playoffs_dropdown$initialized))
      
      user_selected_playoff_year_rf <- if(is_user_selection_valid_rf) as.numeric(user_selected_playoff_year_char_rf) else NULL
      
      should_update_displayed_bracket_rf <- !is_user_selection_valid_rf ||
        (!is.null(user_selected_playoff_year_rf) && user_selected_playoff_year_rf == current_nba_season_end_year_for_refresh)
      
      if (should_update_displayed_bracket_rf) {
        if (!is.null(current_season_bracket_data) && is.data.frame(current_season_bracket_data) && nrow(current_season_bracket_data) > 0) {
          rv_playoff_bracket_scraped$data <- current_season_bracket_data
          rv_playoff_bracket_scraped$error <- NULL
          print(paste0(Sys.time(), " [AUTO_REFRESH_PLAYOFFS] Updated displayed playoff bracket with current season data (", current_nba_season_end_year_for_refresh, ")."))
        } else {
          rv_playoff_bracket_scraped$error <- paste("Failed to auto-refresh current playoff bracket data for season ending", current_nba_season_end_year_for_refresh, ". RealGM might not have data yet or an error occurred.")
          print(paste0(Sys.time(), " [AUTO_REFRESH_PLAYOFFS] Error fetching current playoff bracket (", current_nba_season_end_year_for_refresh, ") for display update."))
        }
        rv_playoff_bracket_scraped$last_fetch_time <- Sys.time()
      } else {
        print(paste0(Sys.time(), " [AUTO_REFRESH_PLAYOFFS] Current season data (", current_nba_season_end_year_for_refresh, ") fetched, but user is viewing historical season ", user_selected_playoff_year_rf %||% "N/A", ". Displayed bracket unchanged by this auto-refresh."))
      }
      print(paste0(Sys.time(), " [AUTO_REFRESH] Auto-refresh cycle complete."))
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    observe({
      date_to_fetch <- selected_date()
      req(date_to_fetch)
      
      print(paste0(Sys.time(), " [SCOREBOARD_DATE_CHANGE] Date changed to: ", format(date_to_fetch, "%Y-%m-%d"), ". Fetching scoreboard..."))
      sb_result <- fetch_scoreboard_data(date_to_fetch)
      rv_sb$data <- sb_result$data
      rv_sb$error <- sb_result$error
      rv_sb$last_fetch_time <- Sys.time()
    })
    
    observeEvent(input$standings_season_select, {
      req(isTRUE(isolate(rv_st$dropdown_initialized)),
          input$standings_season_select,
          nzchar(input$standings_season_select))
      
      selected_s_slug <- input$standings_season_select
      print(paste0(Sys.time(), " [STANDINGS_DROPDOWN_EVENT] Dropdown changed to: ", selected_s_slug, ". Fetching standings..."))
      
      st_res <- fetch_standings_data(season_slug_to_fetch = selected_s_slug, fetch_date_for_current_calc = Sys.Date())
      rv_st$data <- st_res$data
      rv_st$error <- st_res$error
      rv_st$last_fetch_time <- Sys.time()
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$leaders_season_select, {
      req(isTRUE(isolate(rv_ld_dropdown$initialized)),
          input$leaders_season_select,
          nzchar(input$leaders_season_select))
      
      selected_leader_season_slug <- input$leaders_season_select
      current_system_date_for_leaders <- Sys.Date()
      
      print(paste0(Sys.time(), " [LEADERS_DROPDOWN_EVENT] Dropdown changed to: ", selected_leader_season_slug, ". Fetching all leader categories..."))
      
      fetch_leader_category("PTS", rv_ld_pts, current_system_date_for_leaders, selected_leader_season_slug)
      fetch_leader_category("AST", rv_ld_ast, current_system_date_for_leaders, selected_leader_season_slug)
      fetch_leader_category("REB", rv_ld_reb, current_system_date_for_leaders, selected_leader_season_slug)
      fetch_leader_category("BLK", rv_ld_blk, current_system_date_for_leaders, selected_leader_season_slug)
      fetch_leader_category("STL", rv_ld_stl, current_system_date_for_leaders, selected_leader_season_slug)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$playoffs_season_select, {
      req(isTRUE(isolate(rv_playoffs_dropdown$initialized)))
      
      selected_val_char <- input$playoffs_season_select
      print(paste0(Sys.time(), " [PLAYOFF_DROPDOWN_EVENT] Triggered. Value from dropdown: '", selected_val_char %||% "NULL_OR_EMPTY", "'"))
      
      if (is.null(selected_val_char) || !nzchar(selected_val_char) || !grepl("^[0-9]{4}$", selected_val_char)) {
        print(paste0(Sys.time(), " [PLAYOFF_DROPDOWN_EVENT] Invalid or empty selection ('", selected_val_char, "'). No fetch initiated by this event."))
        return()
      }
      
      selected_season_end_year_to_fetch <- as.numeric(selected_val_char)
      print(paste0(Sys.time(), " [PLAYOFF_DROPDOWN_EVENT] Calling fetch_realgm_playoff_bracket_for_season with year: ", selected_season_end_year_to_fetch))
      
      bracket_result <- fetch_realgm_playoff_bracket_for_season(season_end_year = selected_season_end_year_to_fetch)
      
      if (!is.null(bracket_result) && is.data.frame(bracket_result) && nrow(bracket_result) > 0) {
        print(paste0(Sys.time(), " [PLAYOFF_DROPDOWN_EVENT] Fetch successful for ", selected_season_end_year_to_fetch, ". Rows: ", nrow(bracket_result)))
        rv_playoff_bracket_scraped$data <- bracket_result
        rv_playoff_bracket_scraped$error <- NULL
      } else {
        print(paste0(Sys.time(), " [PLAYOFF_DROPDOWN_EVENT] Fetch FAILED or returned no data for ", selected_season_end_year_to_fetch))
        rv_playoff_bracket_scraped$error <- paste("Failed to retrieve or parse playoff bracket data for season ending", selected_season_end_year_to_fetch, ". Data may not be available on RealGM or an error occurred.")
        rv_playoff_bracket_scraped$data <- NULL
      }
      rv_playoff_bracket_scraped$last_fetch_time <- Sys.time()
      print(paste0(Sys.time(), " [PLAYOFF_DROPDOWN_EVENT] rv_playoff_bracket_scraped$error after fetch: ", rv_playoff_bracket_scraped$error %||% "NULL (No error)"))
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    # --- END: Section 5: Server Function (mod_welcome_server) - Part 2: Data Fetching Observers ---
    
    # --- START: Section 6: Server Function (mod_welcome_server) - Part 3: Output Rendering ---
    
    output$current_date_display <- renderUI({
      req(selected_date())
      tags$h5(format(selected_date(), "%a, %b %d, %Y"), style="margin:0; white-space:nowrap; color: #ffffff; font-weight: bold;")
    })
    
    output$news_output <- renderUI({
      req(rv_news$last_fetch_time)
      
      if (rv_news$status == "loading" && is.null(rv_news$data)) {
        return(tags$div(style="min-height: 180px; display:flex; align-items:center; justify-content:center; color:#aaa;", "Loading news..."))
      }
      if (rv_news$status == "error" || is.null(rv_news$data) || !is.data.frame(rv_news$data) || nrow(rv_news$data) == 0) {
        error_message <- rv_news$error %||% "No current news headlines found or an error occurred."
        return(tags$div(class="validation-error-message", style="text-align:center;", error_message))
      }
      
      news_cards_html <- tryCatch({
        rv_news$data %>%
          purrr::pmap(function(headline, link, image_url, ...) {
            img_onerror_js <- sprintf("this.onerror=null; this.src='%s'; this.style.objectFit='contain';", fallback_image_url)
            tags$a(
              href = link %||% "#", target = "_blank", class = "news-card", rel = "noopener noreferrer",
              tags$img(src = image_url %||% fallback_image_url, class = "news-image", alt = headline %||% "NBA News", loading = "lazy", onerror = img_onerror_js),
              tags$div(class = "news-headline", headline %||% "Untitled Article")
            )
          })
      }, error = function(e){
        list(tags$div(class="validation-error-message", "Error displaying news items."))
      })
      tagList(tags$div(class = "news-scroll-container", news_cards_html))
    })
    
    output$scoreboard_output <- renderUI({
      req(!is.null(rv_sb$last_fetch_time))
      
      if (!is.null(rv_sb$error)) {
        return(tags$div(class = "validation-error-message", rv_sb$error))
      }
      if (is.null(rv_sb$data) || (is.data.frame(rv_sb$data) && nrow(rv_sb$data) == 0) ) {
        date_msg <- format(selected_date() %||% Sys.Date(), '%a, %b %d')
        return(tags$p(glue::glue("No games found for {date_msg}."), style="color: #aaaaaa; text-align: center; padding: 10px;"))
      }
      
      game_cards <- tryCatch({
        purrr::pmap(
          .l = rv_sb$data,
          .f = function(gameStatus, gameTime, homeTeamName, awayTeamName, homeTeamAbbr, awayTeamAbbr, homeLogo, awayLogo, homeScore, awayScore, homeRecord, awayRecord, ...) {
            game_status_text <- gameStatus %||% "Scheduled"
            game_time_text <- gameTime %||% ""
            home_name <- homeTeamName %||% "Home"; away_name <- awayTeamName %||% "Away"
            home_abbr <- homeTeamAbbr %||% "HMT"; away_abbr <- awayTeamAbbr %||% "AWT"
            home_logo_url <- homeLogo %||% fallback_image_url; away_logo_url <- awayLogo %||% fallback_image_url
            home_s <- homeScore; away_s <- awayScore
            home_rec <- homeRecord %||% ""; away_rec <- awayRecord %||% ""
            
            game_status_display <- dplyr::case_when(
              grepl("Final|FT", game_status_text, ignore.case = TRUE) ~ "Final",
              grepl("^[0-9]{1,2}:[0-9]{2}\\s*(AM|PM)?(\\s+ET)?$", game_status_text, ignore.case = TRUE) ~ game_status_text,
              grepl("Qtr|Half|OT|Halftime", game_status_text, ignore.case = TRUE) ~ game_status_text,
              !is.na(game_time_text) & nchar(game_time_text) > 4 ~ tryCatch(format(lubridate::parse_date_time(game_time_text, orders=c("Ymd HMS z", "Ymd HMS", "Ymd HM", "Ymd H", "ymd_hms"), tz="UTC"), format="%I:%M %p ET"), error=function(e) game_time_text),
              TRUE ~ game_status_text
            )
            home_score_display <- if (is.na(home_s)) "--" else as.character(home_s)
            away_score_display <- if (is.na(away_s)) "--" else as.character(away_s)
            
            tags$div(class = "sb-game-card",
                     tags$div(class = "sb-top-bar",
                              tags$span(class = "sb-game-date", paste("NBA ·", format(selected_date() %||% Sys.Date(), "%a, %b %d"))),
                              tags$span(class = "sb-game-status", game_status_display)
                     ),
                     tags$div(class = "sb-main-content",
                              tags$div(class = "sb-team sb-away-team",
                                       tags$img(src = away_logo_url, class="sb-team-logo", alt = away_abbr, loading = "lazy", onerror = sprintf("this.onerror=null; this.src='%s';", fallback_image_url)),
                                       tags$div(class = "sb-team-details", tags$span(class = "sb-team-name", away_name), tags$span(class = "sb-team-record", away_rec))
                              ),
                              tags$div(class = "sb-score-area",
                                       tags$span(class = "sb-score", away_score_display),
                                       tags$span(class = "sb-score-separator", HTML("–")), # en-dash
                                       tags$span(class = "sb-score", home_score_display)
                              ),
                              tags$div(class = "sb-team sb-home-team",
                                       tags$div(class = "sb-team-details", tags$span(class = "sb-team-name", home_name), tags$span(class = "sb-team-record", home_rec)),
                                       tags$img(src = home_logo_url, class="sb-team-logo", alt = home_abbr, loading = "lazy", onerror = sprintf("this.onerror=null; this.src='%s';", fallback_image_url))
                              )
                     )
            )
          })
      }, error = function(e){
        print(paste("MOD_WELCOME: Error rendering scoreboard cards:", e$message))
        tags$div(class = "validation-error-message", "Error displaying scoreboard details.")
      })
      tagList(game_cards)
    })
    
    output$standings_output <- renderUI({
      req(isTRUE(rv_st$dropdown_initialized) || !is.null(rv_st$error))
      
      if (!is.null(rv_st$error)) {
        return(tags$div(class = "validation-error-message", rv_st$error))
      }
      if (is.null(rv_st$data) || (is.data.frame(rv_st$data) && nrow(rv_st$data) == 0)) {
        season_label_attr <- attr(rv_st$data, "season_slug") %||% input$standings_season_select %||% "Selected"
        if (season_label_attr != "current" && !is.null(rv_st$available_seasons) && season_label_attr %in% unlist(rv_st$available_seasons)) {
          season_label_attr <- names(rv_st$available_seasons)[unlist(rv_st$available_seasons) == season_label_attr][1] %||% season_label_attr
        } else if (season_label_attr == "current") {
          season_label_attr <- "Current Season"
        }
        return(tags$p(glue::glue("No standings data found for {season_label_attr}."), style="color: #aaaaaa; text-align: center; padding:10px;"))
      }
      
      east_data <- rv_st$data %>% dplyr::filter(conf == "East") %>% dplyr::select(Rank="confRank", Team="teamName", W, L, Pct, GB, Strk="Strk")
      west_data <- rv_st$data %>% dplyr::filter(conf == "West") %>% dplyr::select(Rank="confRank", Team="teamName", W, L, Pct, GB, Strk="Strk")
      
      fetched_season_slug <- attr(rv_st$data, "season_slug")
      display_season_label <- fetched_season_slug
      if (!is.null(rv_st$available_seasons) && !is.null(fetched_season_slug)) {
        matching_name_idx <- which(unlist(rv_st$available_seasons) == fetched_season_slug)
        if (length(matching_name_idx) > 0) {
          display_season_label <- names(rv_st$available_seasons)[matching_name_idx[1]] %||% fetched_season_slug
        }
      }
      display_season_label <- display_season_label %||% "Selected Season"
      
      
      create_standings_table <- function(conf_data, title) {
        required_cols_st <- c("Rank", "Team", "W", "L", "Pct", "GB", "Strk")
        if(is.null(conf_data) || !is.data.frame(conf_data) || nrow(conf_data) == 0) {
          return(tags$div(class="standings-conference", h5(title, class="standings-header"), tags$p(paste("No", title, "data."), style="color:#aaaaaa; text-align:center;")))
        }
        if (!all(required_cols_st %in% names(conf_data))) {
          return(tags$div(class="standings-conference validation-error-message", h5(title, class="standings-header"), tags$p("Standings data format error.")))
        }
        table_rows <- tryCatch({
          purrr::pmap(
            .l = conf_data,
            .f = function(Rank, Team, W, L, Pct, GB, Strk, ...) {
              tags$tr(
                tags$td(Rank %||% "-"),
                tags$td(Team %||% "N/A"),
                tags$td(W %||% "-"),
                tags$td(L %||% "-"),
                tags$td(if(!is.na(Pct) && is.numeric(Pct)) sprintf("%.3f", Pct) else "-"),
                tags$td(dplyr::case_when(is.na(GB) & Rank == 1 ~ "0.0", is.na(GB) ~ "-", !is.na(GB) & is.numeric(GB) & GB >= 0 ~ sprintf("%.1f", GB), TRUE ~ "-" )),
                tags$td(Strk %||% "-")
              )
            })
        }, error = function(e){ list(tags$tr(tags$td(colspan="7", class="validation-error-message", style="text-align: center;", "Error rendering standings rows."))) })
        tags$div(class="standings-conference",
                 h5(title, class="standings-header"),
                 tags$table(class = "standings-table",
                            tags$thead(tags$tr(tags$th("RK"), tags$th("Team"), tags$th("W"), tags$th("L"), tags$th("PCT"), tags$th("GB"), tags$th("STRK"))),
                            tags$tbody(table_rows)
                 )
        )
      }
      tagList(
        tags$p(glue::glue("Displaying Standings for: {display_season_label}"), style="text-align:center; color:#aaa; font-size:0.9em; margin-bottom: 10px;"),
        create_standings_table(east_data, "Eastern Conference"),
        tags$br(),
        create_standings_table(west_data, "Western Conference")
      )
    })
    
    render_leader_list <- function(rv_data_source, category_title) {
      renderUI({
        req(!is.null(rv_data_source$last_fetch_time))
        
        fetched_season_slug_ld <- attr(rv_data_source$data, "season_slug") %||% isolate(input$leaders_season_select) %||% "Selected"
        display_season_label_ld <- fetched_season_slug_ld
        if (!is.null(rv_st$available_seasons) && !is.null(fetched_season_slug_ld)) {
          matching_name_idx_ld <- which(unlist(rv_st$available_seasons) == fetched_season_slug_ld)
          if (length(matching_name_idx_ld) > 0) {
            display_season_label_ld <- names(rv_st$available_seasons)[matching_name_idx_ld[1]] %||% fetched_season_slug_ld
          }
        }
        display_season_label_ld <- display_season_label_ld %||% "Selected Season"
        
        full_category_title <- glue::glue("{category_title} ({display_season_label_ld})")
        
        if (!is.null(rv_data_source$error)) {
          if (grepl("leaders unavailable|Could not retrieve .* leaders|API returned 0 rows", rv_data_source$error, ignore.case = TRUE)) {
            return(tagList(
              h5(full_category_title, class="leaders-header"),
              tags$p(glue::glue("No {tolower(category_title)} data found for the {display_season_label_ld}."), style="color:#aaaaaa; font-size: 0.9em; margin-top: 5px; margin-bottom: 10px; text-align: center;")
            ))
          } else {
            return(tags$div(class = "validation-error-message", paste("Error loading", category_title, ":", rv_data_source$error)))
          }
        }
        if (is.null(rv_data_source$data) || (is.data.frame(rv_data_source$data) && nrow(rv_data_source$data) == 0) ) {
          return(tagList(
            h5(full_category_title, class="leaders-header"),
            tags$p(glue::glue("No {tolower(category_title)} data available for {display_season_label_ld}."), style="color:#aaaaaa; font-size: 0.9em; margin-top: 5px; margin-bottom: 10px; text-align: center;")
          ))
        }
        
        list_items <- tryCatch({
          purrr::pmap(
            .l = rv_data_source$data,
            .f = function(playerId, Rank, Player, Team, Value, ...) {
              headshot_url <- get_player_headshot_url(playerId)
              tags$li(class = "leader-item",
                      tags$span(class="leader-rank", Rank %||% "-"),
                      tags$img(src = headshot_url, class="leader-headshot", alt=Player %||% "Player", loading="lazy", onerror = sprintf("this.onerror=null; this.src='%s';", fallback_image_url)),
                      tags$div(class="leader-details",
                               tags$span(class="leader-name", Player %||% "N/A"),
                               tags$span(class="leader-team", Team %||% "N/A")
                      ),
                      tags$span(class="leader-stat", if(!is.na(Value) && is.numeric(Value)) sprintf("%.1f", Value) else (Value %||% "-"))
              )
            })
        }, error = function(e) {
          list(tags$li(class = "leader-item validation-error-message", style="justify-content: center;", paste("Error displaying", category_title)))
        })
        tagList(h5(full_category_title, class="leaders-header"), tags$ul(class = "leader-list", list_items))
      })
    }
    
    output$leaders_pts_output <- render_leader_list(rv_ld_pts, "Points per game")
    output$leaders_ast_output <- render_leader_list(rv_ld_ast, "Assists per game")
    output$leaders_reb_output <- render_leader_list(rv_ld_reb, "Rebounds per game")
    output$leaders_blk_output <- render_leader_list(rv_ld_blk, "Blocks per game")
    output$leaders_stl_output <- render_leader_list(rv_ld_stl, "Steals per game")
    
    output$scraped_bracket_output <- renderUI({
      req(rv_playoff_bracket_scraped$last_fetch_time)
      
      current_selection_char <- isolate(input$playoffs_season_select)
      scraped_for_season_attr_char <- attr(rv_playoff_bracket_scraped$data, "season_scraped")
      
      effective_season_end_year_char <- scraped_for_season_attr_char %||% current_selection_char
      
      display_title_season_year_num <- suppressWarnings(as.numeric(effective_season_end_year_char))
      
      display_title_season_label <- if (!is.na(display_title_season_year_num)) {
        paste0(display_title_season_year_num - 1, "-", stringr::str_sub(as.character(display_title_season_year_num), -2))
      } else {
        "Selected"
      }
      
      if (!is.null(rv_playoff_bracket_scraped$error)) {
        return(tags$div(class = "validation-error-message", rv_playoff_bracket_scraped$error))
      }
      if (is.null(rv_playoff_bracket_scraped$data) || !is.data.frame(rv_playoff_bracket_scraped$data) || nrow(rv_playoff_bracket_scraped$data) == 0) {
        return(tags$p(glue::glue("No playoff data available to display for the {display_title_season_label} season."), style="color:#aaaaaa; text-align: center; padding: 20px;"))
      }
      
      bracket_data_to_render <- rv_playoff_bracket_scraped$data
      current_team_logos <- team_logo_mapping()
      
      current_bracket_season_end_year <- if (!is.na(display_title_season_year_num)) {
        display_title_season_year_num
      } else {
        warning("Could not determine a valid season end year for rendering playoff bracket slots.")
        return(tags$div(class="validation-error-message", "Error: Bracket season year undetermined."))
      }
      
      required_bracket_cols <- c("round_number", "conference", "high_seed_team_name", "low_seed_team_name", "high_seed_wins", "low_seed_wins", "series_status", "high_seed_seed", "low_seed_seed")
      for(col_name in required_bracket_cols){
        if(!col_name %in% names(bracket_data_to_render)){
          warning(paste0("Missing column '", col_name, "' in playoff bracket data. Adding as NA."))
          if(grepl("wins|seed|number", col_name)) bracket_data_to_render[[col_name]] <- NA_integer_
          else bracket_data_to_render[[col_name]] <- NA_character_
        }
      }
      
      r1_w <- list( "1v8" = get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "West", 1, "1v8"),
                    "4v5" = get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "West", 1, "4v5"),
                    "3v6" = get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "West", 1, "3v6"),
                    "2v7" = get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "West", 1, "2v7"))
      r1_e <- list( "1v8" = get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "East", 1, "1v8"),
                    "4v5" = get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "East", 1, "4v5"),
                    "3v6" = get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "East", 1, "3v6"),
                    "2v7" = get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "East", 1, "2v7"))
      r2_w <- list( get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "West", 2, 1, r1_west_results=r1_w),
                    get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "West", 2, 2, r1_west_results=r1_w))
      r2_e <- list( get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "East", 2, 1, r1_east_results=r1_e),
                    get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "East", 2, 2, r1_east_results=r1_e))
      r3_w <- list(get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "West", 3, 1, r2_west_results=r2_w))
      r3_e <- list(get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "East", 3, 1, r2_east_results=r2_e))
      r4_f <- list(get_series_for_slot(bracket_data_to_render, current_bracket_season_end_year, "Finals", 4, 1, r3_west_results=r3_w, r3_east_results=r3_e))
      
      w_r1_matchups <- tagList(lapply(r1_w, function(s) create_matchup_html_new(s, is.null(s), current_team_logos, fallback_image_url_global = fallback_image_url)))
      e_r1_matchups <- tagList(lapply(r1_e, function(s) create_matchup_html_new(s, is.null(s), current_team_logos, fallback_image_url_global = fallback_image_url)))
      w_r2_matchups <- tagList(lapply(r2_w, function(s) create_matchup_html_new(s, is.null(s), current_team_logos, fallback_image_url_global = fallback_image_url)))
      e_r2_matchups <- tagList(lapply(r2_e, function(s) create_matchup_html_new(s, is.null(s), current_team_logos, fallback_image_url_global = fallback_image_url)))
      w_r3_matchup  <- create_matchup_html_new(r3_w[[1]], is.null(r3_w[[1]]), current_team_logos, fallback_image_url_global = fallback_image_url)
      e_r3_matchup  <- create_matchup_html_new(r3_e[[1]], is.null(r3_e[[1]]), current_team_logos, fallback_image_url_global = fallback_image_url)
      finals_matchup<- create_matchup_html_new(r4_f[[1]], is.null(r4_f[[1]]), current_team_logos, is_finals_matchup=TRUE, fallback_image_url_global = fallback_image_url)
      
      show_first_round_ui_cols <- TRUE
      if (!is.na(current_bracket_season_end_year) && current_bracket_season_end_year <= 1974) {
        show_first_round_ui_cols <- FALSE
      }
      
      tagList(
        tags$h4(glue::glue("{display_title_season_label} NBA Playoffs"), style = "color: #ffffff; text-align: center; margin-bottom: 20px;"),
        tags$div(class = "bracket-container-flex",
                 { if (show_first_round_ui_cols) tags$div(class = "round-column west", tags$div(class = "round-header", HTML("WEST<br>First Round")), w_r1_matchups) },
                 tags$div(class = "round-column west", tags$div(class = "round-header", HTML("WEST<br>Conf. Semifinals")), w_r2_matchups),
                 tags$div(class = "round-column west", tags$div(class = "round-header", HTML("WEST<br>Conf. Finals")), w_r3_matchup),
                 tags$div(class = "finals-column",
                          tags$div(class = "round-header", HTML("NBA<br>Finals")),
                          tags$div(class="finals-logo-area", tags$img(src = fallback_image_url, alt = "NBA Finals Logo", style="width:80px; height:auto;")),
                          finals_matchup
                 ),
                 tags$div(class = "round-column east", tags$div(class = "round-header", HTML("EAST<br>Conf. Finals")), e_r3_matchup),
                 tags$div(class = "round-column east", tags$div(class = "round-header", HTML("EAST<br>Conf. Semifinals")), e_r2_matchups),
                 { if (show_first_round_ui_cols) tags$div(class = "round-column east", tags$div(class = "round-header", HTML("EAST<br>First Round")), e_r1_matchups) }
        )
      )
    })
    
  }) # End moduleServer
}
# --- END: Section 6: Server Function (mod_welcome_server) - Part 3: Output Rendering ---

# --- END FILE: modules/mod_welcome.R (Complete and Updated with Dropdown Year Changes) ---