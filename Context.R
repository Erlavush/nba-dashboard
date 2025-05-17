
# --- Script to Consolidate Project Files for LLM Context ---

# 1. Configuration
# --------------------------------------------------------------------------
# Output file name (will be created in the project root)
output_filename <- "project_snapshot.txt"

# Files/directories/extensions to ignore (uses regular expressions)
# Add patterns for things you want to exclude.
# Common examples:
# - .git/ : Git repository internal files
# - .Rproj.user/ : RStudio temporary files
# - .Rhistory : R command history
# - .RData : R workspace data
# - .rds / .rda : Saved R objects (often large/binary)
# - renv/ : renv library cache
# - rsconnect/ : Shinyapps.io deployment files
# - Images/Binary files: .png, .jpg, .jpeg, .gif, .ico, .pdf, .xlsx, .csv (if large/data)
# - The output file itself!
ignore_patterns <- c(
  "\\.Rproj$",              # RStudio project file itself (usually not needed for code context)
  "\\.Rproj\\.user/",       # RStudio temporary directory
  "\\.Rhistory$",
  "\\.RData$",
  "\\.Ruserdata/",
  "renv/",                 # renv directory
  "rsconnect/",            # Deployment files
  "\\.git/",               # Git directory
  "packrat/",              # Packrat directory
  output_filename,         # Ignore the output file itself!
  "\\.png$", "\\.jpg$", "\\.jpeg$", "\\.gif$", "\\.ico$", # Common image formats
  "\\.pdf$", "\\.xlsx$", "\\.zip$", # Other binary/document formats
  "\\.rds$", "\\.rda$",         # Saved R objects (can be large/binary)
  # Add any other specific files or directories you want to ignore:
  # "my_secret_file.R",
  # "data_large/"
  "~$",                    # Temporary Word/Excel files
  "\\.DS_Store$"           # MacOS specific
)

# Combine ignore patterns into a single regex pattern
ignore_regex <- paste(ignore_patterns, collapse = "|")

# Set max file size (in bytes) to include (e.g., 1MB = 1 * 1024 * 1024)
# Set to Inf to include all sizes (be careful with large files!)
max_file_size_bytes <- 1 * 1024 * 1024 # 1 MB limit

# --------------------------------------------------------------------------
# 2. Setup - Load Required Packages
# --------------------------------------------------------------------------
if (!requireNamespace("fs", quietly = TRUE)) {
  install.packages("fs")
}
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(fs)
library(here)

# --------------------------------------------------------------------------
# 3. Define Project Root and Output Path
# --------------------------------------------------------------------------
project_dir <- here::here()
output_file_path <- file.path(project_dir, output_filename)

cat("Project Directory:", project_dir, "\n")
cat("Output File:", output_file_path, "\n")

# --------------------------------------------------------------------------
# 4. Generate Directory Tree (Filtered)
# --------------------------------------------------------------------------
cat("Generating filtered directory tree...\n")

# Define patterns specifically for hiding items from the TREE VIEW
# These often target common hidden/config/temporary files and directories
# We use simple patterns here that dir_tree seems to handle well.
tree_ignore_patterns <- c(
  "^\\.git$",             # .git directory at root
  "^\\.Rproj\\.user$",    # .Rproj.user directory at root
  "^\\.Rhistory$",        # .Rhistory file at root
  "^\\.RData$",           # .RData file at root
  "^\\.Ruserdata$",      # .Ruserdata directory at root
  "^renv$",              # renv directory at root
  "^rsconnect$",         # rsconnect directory at root
  "^packrat$",           # packrat directory at root
  "^\\.DS_Store$",        # MacOS file at root
  "^\\.gitignore$"        # .gitignore file at root
  # Add other top-level items you want hidden from the tree view
  # Note: Deeply nested ignored files might still show parent dirs,
  # but the files themselves won't have content included later.
)

tree_lines <- tryCatch({
  # Capture the output of fs::dir_tree
  # Use 'glob' filtering which is often simpler for directory/file names
  # Use recurse = TRUE first to get all potential items, then filter
  all_tree_items <- fs::dir_info(path = project_dir, recurse = TRUE, type = "any", all = TRUE, fail = FALSE)
  
  # Filter based on the base name matching ignore patterns at the project root
  # And filter based on components matching anywhere (like .Rproj.user deeper)
  # This is a bit more complex to replicate dir_tree's filtering perfectly outside,
  # so we'll stick to dir_tree's built-in filtering which is usually sufficient.
  
  # Let's try dir_tree again with refined regex, focusing on anchoring at the start
  # of a path component if possible, or just the name.
  # The key is invert = TRUE to HIDE matches.
  capture.output(
    fs::dir_tree(
      path = project_dir,
      recurse = TRUE,
      type = "any",
      all = FALSE, # Try with FALSE first - might hide dotfiles by default
      # If needed, switch back to all=TRUE and use regex:
      # all = TRUE,
      # regexp = paste(tree_ignore_patterns, collapse = "|"),
      # invert = TRUE # HIDE paths matching the ignore patterns
    )
  )
  # If all=FALSE doesn't hide enough, uncomment the regexp lines above
  # and potentially adjust tree_ignore_patterns (e.g., remove ^ anchor)
  
}, error = function(e) {
  paste("Error generating directory tree:", e$message)
})

# Add a header explaining the tree view
tree_output <- c(
  "PROJECT DIRECTORY TREE (Filtered View)",
  "(Common config/temporary files like .Rproj.user, .git, .Rhistory are hidden)",
  "======================================================================",
  tree_lines,
  "\n\n" # Add some spacing before file contents
)

# --------------------------------------------------------------------------
# 5. List, Filter, and Read Files
# --------------------------------------------------------------------------
cat("Listing all files recursively...\n")
all_files <- list.files(project_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)

cat("Filtering files based on ignore patterns and size...\n")

# Filter files based on ignore_regex
files_to_include <- grep(ignore_regex, all_files, value = TRUE, invert = TRUE)

# Further filter: Ensure they are actually files (not directories) and check size
final_file_list <- character()
for (f in files_to_include) {
  # Need to handle potential errors if file disappears between list and info
  file_info <- tryCatch(fs::file_info(f), error = function(e) NULL)
  if (!is.null(file_info) && file_info$type == "file") {
    if (is.na(file_info$size) || file_info$size <= max_file_size_bytes) {
      final_file_list <- c(final_file_list, f)
    } else {
      cat("  Skipping (too large):", f, "(Size:", format(file_info$size, units="auto"), ")\n")
    }
  }
}

cat("Found", length(final_file_list), "files to include.\n")

# Prepare list to hold file contents
file_contents_list <- list()

cat("Reading and formatting file contents...\n")
for (file_path in final_file_list) {
  # Make path relative to project root for cleaner output markers
  relative_path <- fs::path_rel(file_path, start = project_dir)
  cat("  Processing:", relative_path, "\n")
  
  tryCatch({
    # Read lines, suppressing warning for files not ending with newline
    file_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8") # Specify UTF-8
    
    # Create start/end markers
    start_marker <- paste0("--- START FILE: ", relative_path, " ---")
    end_marker <- paste0("--- END FILE: ", relative_path, " ---")
    
    # Add to list
    file_contents_list[[file_path]] <- c(start_marker, file_lines, end_marker, "") # Add blank line for spacing
    
  }, error = function(e) {
    cat("  Warning: Could not read file:", relative_path, "- Error:", e$message, "\n")
    # Add a note about the error in the output
    error_marker <- paste0("--- ERROR READING FILE: ", relative_path, " ---")
    error_details <- paste0("--- Error message: ", e$message, " ---")
    file_contents_list[[file_path]] <- c(error_marker, error_details, "")
  })
}

# --------------------------------------------------------------------------
# 6. Combine and Write Output
# --------------------------------------------------------------------------
cat("Combining tree and file contents...\n")

# Flatten the list of file contents
all_formatted_content <- unlist(file_contents_list, use.names = FALSE)

# Combine tree and content
final_output <- c(tree_output, all_formatted_content)

cat("Writing output to:", output_file_path, "\n")
tryCatch({
  writeLines(final_output, output_file_path, useBytes = TRUE) # useBytes helps preserve encoding
  cat("----------------------------------------------------------------------\n")
  cat("Success! Project snapshot written to:", output_filename, "\n")
  cat("You can now copy the contents of this file.\n")
  cat("----------------------------------------------------------------------\n")
}, error = function(e) {
  cat("----------------------------------------------------------------------\n")
  cat("Error writing output file:", e$message, "\n")
  cat("----------------------------------------------------------------------\n")
})
