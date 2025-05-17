# --- START FILE: global.R (Complete and Updated for ALL modules, including new GOAT Discourse) ---
# global.R

print("--- Running global.R ---")

# --- Load Machine Learning Packages FIRST ---
# These are often foundational and can have namespace overlaps if loaded later.
print("--- Loading ML & Core Statistical Packages ---")
library(tidymodels) # Core package for workflows, recipes, etc. (used in model training & potentially by loaded models)
library(ranger)     # Engine for random forest models
library(xgboost)    # Engine for boosted tree models
library(zoo)        # For rollapply (used in model training and some modules)
# library(infer)    # Explicitly load if direct hypothesis testing functions are used in app (currently seems more for training scripts)

# --- Load Core Shiny & General Utility Packages ---
print("--- Loading Core Shiny & General Utility Packages ---")
library(shiny)
library(bslib)      # For theming
library(dplyr)      # For data manipulation (load AFTER tidymodels to avoid mask warnings if any)
library(purrr)      # For functional programming (load AFTER tidymodels)
library(readr)      # For read_csv
library(tidyr)      # For data tidying (pivot_wider, replace_na)
library(stringr)    # For string manipulation
library(lubridate)  # For date/time functions
library(tibble)     # For tibble creation
library(glue)       # For formatted messages/strings
library(scales)     # For formatting (percent, comma, etc.)
library(rlang)      # For advanced R programming, often a dependency
library(htmltools)  # For HTML generation utilities with Shiny

# --- Load Data Fetching / API Packages ---
print("--- Loading Data Fetching / API Packages ---")
library(hoopR)     # For fetching NBA data (used in mod_player_stats, mod_historical_player_stats, mod_welcome)
library(httr)      # For web requests (used in mod_welcome news/bracket scraping)
library(rvest)     # For web scraping (used in mod_welcome news/bracket scraping)
# library(nbastatR) # Commented out, assuming not actively used based on previous files

# --- Load Plotting & Visualization Packages ---
print("--- Loading Plotting & Visualization Packages ---")
library(plotly)    # Interactive plots (can be used for sentiment charts if preferred over echarts4r)
library(echarts4r) # Interactive charts (chosen for sentiment charts in GOAT module)
library(wordcloud)  # For comparison.cloud in GOAT module
library(wordcloud2) # For individual word clouds in GOAT module
library(shinycssloaders) # REQUIRED for spinners across modules
library(vip)       # For variable importance plots (used in model training, potentially in future display)

# --- Load Natural Language Processing (NLP) Packages for GOAT Discourse Module ---
print("--- Loading NLP Packages for GOAT Discourse Module ---")
library(tidytext)   # For text mining functionalities (tokenization, sentiment)
library(textdata)   # For accessing sentiment lexicons (e.g., bing, afinn, nrc)

# --- Optional: Load shinyjs if used for JS interactions ---
# library(shinyjs) # Uncomment if you decide to use shinyjs functions

# --- Define Global Variables & Helper Functions ---

# Rebrand Teams Map (Used by multiple modules and model training scripts)
# It's good to define this once globally.
rebranded_teams <- c(
  "Minneapolis Lakers" = "Los Angeles Lakers",
  "Ft. Wayne Zollner Pistons" = "Detroit Pistons",
  "Rochester Royals" = "Sacramento Kings",
  "Milwaukee Hawks" = "Atlanta Hawks",
  "Tri-Cities Blackhawks" = "Atlanta Hawks",
  "New Orleans Hornets" = "New Orleans Pelicans",
  "Charlotte Bobcats" = "Charlotte Hornets",
  "Seattle SuperSonics" = "Oklahoma City Thunder",
  "New Jersey Nets" = "Brooklyn Nets",
  "Vancouver Grizzlies" = "Memphis Grizzlies",
  "Syracuse Nationals" = "Philadelphia 76ers",
  "Philadelphia Warriors" = "Golden State Warriors",
  "San Diego Rockets" = "Houston Rockets",
  "Cincinnati Royals" = "Sacramento Kings",
  "Baltimore Bullets" = "Washington Wizards",
  "St. Louis Hawks" = "Atlanta Hawks",
  "San Francisco Warriors" = "Golden State Warriors",
  "Buffalo Braves" = "Los Angeles Clippers",
  "Kansas City-Omaha Kings" = "Sacramento Kings",
  "New Orleans Jazz" = "Utah Jazz",
  "New York Nets" = "Brooklyn Nets",
  "Washington Bullets" = "Washington Wizards",
  "Kansas City Kings" = "Sacramento Kings",
  "San Diego Clippers" = "Los Angeles Clippers"
)
print("Global: Rebranded teams map defined.")

# --- Define Resource Paths ---
# This section helps Shiny find files in the www directory reliably.
print("Global: Defining Resource Paths...")
www_dir_path <- file.path(getwd(), 'www') # Assumes app is run from project root

if (!dir.exists(www_dir_path)) {
  warning(paste("Global WARNING: The 'www' directory was not found at:", www_dir_path, "- Resource paths (CSS, images, JS libs) may fail."))
} else {
  # Map the URL prefix 'www_files' to the actual 'www' directory
  # This allows using "www_files/styles.css" or "www_files/nba-logo.png" in UI
  shiny::addResourcePath(prefix = 'www_files', directoryPath = www_dir_path)
  print(paste("Global: Resource path 'www_files' mapped to", www_dir_path))
  
  # Define path for slick libs (if used, currently commented out in app.R)
  slick_libs_path <- file.path(www_dir_path, 'libs', 'slick')
  if(!dir.exists(slick_libs_path)){
    # This is a warning, not an error, as Slick usage might be optional/commented out
    print(paste("Global WARNING: Slick library directory not found at:", slick_libs_path, "- Carousel features may not work if enabled."))
  } else {
    # Map the URL prefix 'slick_libs' to the actual 'www/libs/slick' directory
    shiny::addResourcePath(prefix = 'slick_libs', directoryPath = slick_libs_path)
    print(paste("Global: Resource path 'slick_libs' mapped to", slick_libs_path))
  }
}
print("Global: Resource path definitions complete (or warnings issued).")


# --- Source All Module Files ---
print("Global: Sourcing R module files from 'modules/' directory...")
module_dir <- file.path(getwd(), "modules") # Assumes 'modules' is at project root

if (!dir.exists(module_dir)) {
  stop(paste("CRITICAL Global ERROR: Directory 'modules' does not exist at the expected location:", module_dir, ". Application cannot start."))
} else {
  module_files <- list.files(module_dir, pattern = "^mod_.*\\.R$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(module_files) == 0) {
    warning("Global WARNING: No module files (mod_*.R) found in the 'modules' directory. The application might not have any content.")
  } else {
    print("Global: Found the following module files to source:")
    print(basename(module_files)) # Print just the filenames for brevity
    
    # Source each module file into the global environment
    # This makes their UI and server functions available to app.R
    sapply(module_files, function(file) {
      print(paste("Global: Sourcing module -", basename(file)))
      tryCatch({
        source(file, local = FALSE) # local = FALSE is crucial for functions to be in GlobalEnv
        
        # --- Optional: Dynamic Check for UI Function Existence (for debugging) ---
        module_base_name_check <- basename(file)
        expected_ui_func_check <- sub("\\.R$", "_ui", module_base_name_check, ignore.case = TRUE) # Case-insensitive extension removal
        
        if (startsWith(module_base_name_check, "mod_") && !exists(expected_ui_func_check, mode = "function", envir = .GlobalEnv)) {
          warning(glue::glue("   -> Global WARNING: Sourced {module_base_name_check}, but expected UI function {expected_ui_func_check}() was NOT found in the global environment."))
        }
        # --- End Optional Dynamic Check ---
        
      }, error = function(e) {
        # Stop the app if a module fails to source, as it's critical
        stop(paste("CRITICAL Global ERROR: Failed to source module file '", basename(file), "'. Error: ", e$message))
      })
    })
    print("Global: All found module files sourced.")
  }
}

# --- Final Check for Critical Module UI Functions (in Global Environment) ---
# This helps catch issues if modules were not sourced correctly or if UI functions are misnamed.
print("Global: Verifying existence of critical module UI functions...")
critical_module_ui_functions <- c(
  "mod_welcome_ui",
  "mod_player_stats_ui",
  "mod_historical_player_stats_ui",
  "mod_game_predictor_ui",
  "mod_player_performance_ui",
  "mod_goat_discourse_ui" # Added for the new/rebuilt GOAT module
)

functions_exist_flags <- sapply(critical_module_ui_functions, exists, mode = "function", envir = .GlobalEnv)

if (all(functions_exist_flags)) {
  print("Global: All critical module UI functions verified successfully.")
} else {
  missing_funcs <- names(functions_exist_flags[!functions_exist_flags])
  stop(paste("CRITICAL Global ERROR: The following critical module UI functions are missing after sourcing:", 
             paste(missing_funcs, collapse = ", "), 
             ". Check module files in 'modules/' directory and their sourcing in global.R. Application cannot start."))
}

# --- Other Global Helper Functions (if any) ---
# Define any truly global helper functions below this line if they are used across multiple modules
# and are not specific enough to live within a single module's utils or the module itself.
# Example:
# format_percentage <- function(value, digits = 1) {
#   scales::percent(value, accuracy = 1/10^digits)
# }
# print("Global: Custom helper functions defined (if any).")


print("--- Finished global.R successfully ---")
# --- END FILE: global.R ---