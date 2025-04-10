# global.R

print("--- Running global.R ---")

# --- Load Core Packages ---
# Make sure these are installed! Use install.packages("package_name") if not.
library(shiny)
library(bslib)
library(dplyr)
library(purrr)     # For %||% operator and list manipulation
library(hoopR)     # For fetching NBA data
library(echarts4r) # For interactive radar chart
library(scales)    # For scales::percent()
library(tidyr)     # For replace_na() - Added
library(plotly)
# library(DT) # No longer needed unless used elsewh ere
# library(ggplot2) # No longer needed unless used elsewhere

print("--- Sourcing Modules ---")

# --- Load Modules ---
module_dir <- file.path(getwd(), "modules")
print(paste("Looking for modules in:", module_dir))

if (!dir.exists(module_dir)) {
  warning("Directory 'modules' does not exist at: ", module_dir)
} else {
  module_files <- list.files(module_dir, pattern = "\\.R$", full.names = TRUE)
  print("Found module files:")
  print(module_files)
  
  if (length(module_files) > 0) {
    lapply(module_files, function(file) {
      print(paste("Sourcing:", file))
      tryCatch({
        source(file, local = FALSE)
        # Optional: Add checks here if functions exist after sourcing
        if (basename(file) == "mod_welcome.R" && !exists("mod_welcome_ui", mode = "function")) {
          warning("   -> Sourced mod_welcome.R, but mod_welcome_ui() function NOT found.")
        }
        if (basename(file) == "mod_player_stats.R" && !exists("mod_player_stats_ui", mode = "function")) {
          warning("   -> Sourced mod_player_stats.R, but mod_player_stats_ui() function NOT found.")
        }
      }, error = function(e) {
        warning(paste("ERROR sourcing", file, ":", e$message))
      })
    })
  } else {
    print("No .R files found in the 'modules' directory.")
  }
}

# --- Final Check for Functions ---
if (!exists("mod_welcome_ui", mode = "function")) {
  warning("Checked global env: mod_welcome_ui() DOES NOT exist.")
} else {
  print("Checked global env: mod_welcome_ui() exists.")
}
if (!exists("mod_player_stats_ui", mode = "function")) {
  warning("Checked global env: mod_player_stats_ui() DOES NOT exist.")
} else {
  print("Checked global env: mod_player_stats_ui() exists.")
}

# Define helper function globally (if needed, hoopR might export it directly)
# Ensure year_to_season is available, either via hoopR:: or defined here
# year_to_season <- hoopR::year_to_season

print("--- Finished global.R ---")

