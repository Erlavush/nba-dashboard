# --- START FILE: modules/mod_goat_discourse.R (Full, Complete, Final Version - KPI Boxes Removed) ---
# Module for GOAT Discourse Analysis using NLP and Text Mining

# --- Libraries (Assumed to be in global.R) ---
# Ensure these are loaded in global.R:
# shiny, dplyr, stringr, readr, tidytext, textdata, wordcloud, wordcloud2,
# tidyr, purrr, glue, shinycssloaders, echarts4r, plotly, lubridate, rlang, scales


# --- Helper UI function to generate individual player analysis layout ---
# DEFINED AT THE TOP LEVEL OF THE MODULE FILE for correct scoping
render_individual_player_analysis_ui_content <- function(ns_for_outputs, player_full_name, analysis_data) {
  # ns_for_outputs will be the `ns` function from the server's module instance.
  # player_full_name is the name of the legend.
  # analysis_data is the direct output from the player_nlp_data_r() reactive.
  
  if (is.null(analysis_data)) {
    return(tags$div(style="min-height: 400px; display:flex; align-items:center; justify-content:center;",
                    tags$p(glue::glue("Select the '{player_full_name}' tab to load and process data."),
                           style="text-align:center; color:#aaa; padding:20px;")))
  }
  if (!is.null(analysis_data$error)) {
    return(tags$div(class="validation-error-message", style="margin:20px;", analysis_data$error))
  }
  if (analysis_data$texts_count == 0 && is.null(analysis_data$error)) {
    return(tags$div(style="min-height: 400px; display:flex; align-items:center; justify-content:center;",
                    tags$p(glue::glue("No processable tweets found for {player_full_name} after filtering."),
                           style="text-align:center; color:#aaa; padding:20px;")))
  }
  
  player_id_safe <- gsub("[^A-Za-z0-9_]", "_", player_full_name)
  
  # KPI value calculations are no longer needed for direct display in boxes,
  # but analysis_data$sentiment_breakdown is still used by the donut chart.
  
  tagList(
    fluidRow(
      class = "goat-main-title-row", 
      column(width = 12,
             h4(glue::glue("Discourse Analysis For: {player_full_name}"),
                style = "text-align: center; text-transform: capitalize; margin-top: 0px !important; margin-bottom: 10px !important;")
      )
    ),
    fluidRow(
      class = "goat-content-row",
      column(width = 5, class="goat-individual-left-col",
             # 1. Sentiment Distribution (Donut Chart)
             h5("Sentiment Distribution", 
                style = "text-align: center; margin-bottom: 5px !important;"), # Top margin controlled by CSS
             div(class="individual-sentiment-donut-container", 
                 style = "margin-bottom: 15px !important;", # Space AFTER donut, BEFORE next element (now HR)
                 shinycssloaders::withSpinner(
                   echarts4r::echarts4rOutput(ns_for_outputs(paste0("sentiment_donut_chart_", player_id_safe)), height = "250px"),
                   type = 6, color = "#1d428a"
                 )
             ),
             # KPI BOXES HAVE BEEN REMOVED FROM HERE
             hr(style="margin-top: 15px; margin-bottom: 10px;"), # This HR will now be after the donut chart
             # 2. Example Positive Mentions (Now directly after the HR following the donut chart)
             div(class="example-phrases-container",
                 h5("Example Positive Mentions", style = "text-align: center; margin-top:0px !important; margin-bottom: 10px;"),
                 shinycssloaders::withSpinner(uiOutput(ns_for_outputs(paste0("positive_phrases_", player_id_safe))), type = 6, color = "#1d428a"),
                 hr(style="margin-top: 10px; margin-bottom: 10px;"),
                 # 3. Example Negative Mentions
                 h5("Example Negative Mentions", style = "text-align: center; margin-top:0px !important; margin-bottom: 10px;"),
                 shinycssloaders::withSpinner(uiOutput(ns_for_outputs(paste0("negative_phrases_", player_id_safe))), type = 6, color = "#1d428a")
             )
      ),
      column(width = 7, class="goat-individual-right-col",
             h5("Prominent Terms in Discussion", 
                style = "text-align: center; margin-bottom: 15px;"), # Top margin controlled by CSS
             div(class="goat-wordcloud-container",
                 shinycssloaders::withSpinner(wordcloud2::wordcloud2Output(ns_for_outputs(paste0("wordcloud_", player_id_safe)), height = "500px"), type = 6, color = "#1d428a")
             )
      )
    )
  )
} # End of render_individual_player_analysis_ui_content definition


# --- UI Function ---
mod_goat_discourse_ui <- function(id) {
  ns <- NS(id) # Main module namespace
  
  legends_list_for_ui <- sort(c(
    "Michael Jordan", "LeBron James", "Kobe Bryant", "Kevin Durant"
  ))
  
  player_A_cloud_color_bright <- "#5CACE2"
  player_B_cloud_color_bright <- "#FF6B6B"
  
  # Main UI structure for the module
  tagList(
    h3("NBA GOAT Discourse Analyzer"),
    p("Explore public discourse and sentiment surrounding NBA legends using Twitter data. ",
      "Analyze individual legends or compare two side-by-side."),
    br(),
    
    tabsetPanel(
      id = ns("analyzer_mode_tabs"),
      type = "pills",
      
      tabPanel(
        title = "Individual Legend Analysis",
        value = ns("tab_individual_analysis"),
        br(),
        p("Select a legend below to view their detailed discourse analysis.", style="text-align:center; font-style:italic; color:#aaa;"),
        
        tabsetPanel(
          id = ns("individual_legend_tabs"),
          type = "tabs",
          tabPanel(title = "Michael Jordan", value = ns("legend_Michael Jordan"),
                   uiOutput(ns("analysis_output_Michael_Jordan"))),
          tabPanel(title = "LeBron James", value = ns("legend_LeBron James"),
                   uiOutput(ns("analysis_output_LeBron_James"))),
          tabPanel(title = "Kobe Bryant", value = ns("legend_Kobe Bryant"),
                   uiOutput(ns("analysis_output_Kobe_Bryant"))),
          tabPanel(title = "Kevin Durant", value = ns("legend_Kevin Durant"),
                   uiOutput(ns("analysis_output_Kevin_Durant")))
        )
      ),
      
      tabPanel(
        title = "Compare Two Legends",
        value = ns("tab_comparison_view"),
        br(),
        wellPanel(
          fluidRow(
            column(5,
                   selectizeInput(ns("compare_player_A"),
                                  label = tags$strong("Select Legend A:"),
                                  choices = legends_list_for_ui,
                                  selected = legends_list_for_ui[1],
                                  options = list(placeholder = 'Select Legend A...'))
            ),
            column(5,
                   selectizeInput(ns("compare_player_B"),
                                  label = tags$strong("Select Legend B:"),
                                  choices = legends_list_for_ui,
                                  selected = legends_list_for_ui[2],
                                  options = list(placeholder = 'Select Legend B...'))
            ),
            column(2, style = "margin-top: 25px; display: flex; align-items: flex-end;",
                   actionButton(ns("run_comparison_button"), "Compare Legends", icon = icon("balance-scale"), class="btn-primary btn-block")
            )
          )
        ),
        hr(),
        h4("Comparative Analysis", style="text-align:center; margin-bottom:15px;"),
        shinycssloaders::withSpinner(uiOutput(ns("comparison_display_output_sentiment_chart_only")), type = 6, color = "#1d428a"),
        
        hr(),
        
        h5("Positive Terms Comparison", style="text-align:center; margin-top: 20px;"),
        fluidRow(
          column(width = 6, class="comparison-column",
                 tags$div(textOutput(ns("positive_cloud_title_A")),
                          style=glue::glue("text-align:center; color: #FFFFFF; background-color: {player_A_cloud_color_bright}; padding: 5px 8px; border-radius: 4px; font-weight: bold; margin-bottom: 5px; font-size: 1.1em;")),
                 div(class = "comparison-wordcloud-plot-container",
                     shinycssloaders::withSpinner(plotOutput(ns("positive_cloud_player_A"), height="380px"), type=6, color=player_A_cloud_color_bright)
                 )
          ),
          column(width = 6, class="comparison-column",
                 tags$div(textOutput(ns("positive_cloud_title_B")),
                          style=glue::glue("text-align:center; color: #FFFFFF; background-color: {player_B_cloud_color_bright}; padding: 5px 8px; border-radius: 4px; font-weight: bold; margin-bottom: 5px; font-size: 1.1em;")),
                 div(class = "comparison-wordcloud-plot-container",
                     shinycssloaders::withSpinner(plotOutput(ns("positive_cloud_player_B"), height="380px"), type=6, color=player_B_cloud_color_bright)
                 )
          )
        ),
        
        hr(),
        
        h5("Negative Terms Comparison", style="text-align:center; margin-top: 20px;"),
        fluidRow(
          column(width = 6, class="comparison-column",
                 tags$div(textOutput(ns("negative_cloud_title_A")),
                          style=glue::glue("text-align:center; color: #FFFFFF; background-color: {player_A_cloud_color_bright}; padding: 5px 8px; border-radius: 4px; font-weight: bold; margin-bottom: 5px; font-size: 1.1em;")),
                 div(class = "comparison-wordcloud-plot-container",
                     shinycssloaders::withSpinner(plotOutput(ns("negative_cloud_player_A"), height="380px"), type=6, color=player_A_cloud_color_bright)
                 )
          ),
          column(width = 6, class="comparison-column",
                 tags$div(textOutput(ns("negative_cloud_title_B")),
                          style=glue::glue("text-align:center; color: #FFFFFF; background-color: {player_B_cloud_color_bright}; padding: 5px 8px; border-radius: 4px; font-weight: bold; margin-bottom: 5px; font-size: 1.1em;")),
                 div(class = "comparison-wordcloud-plot-container",
                     shinycssloaders::withSpinner(plotOutput(ns("negative_cloud_player_B"), height="380px"), type=6, color=player_B_cloud_color_bright)
                 )
          )
        )
      )
    ),
    br(),
    uiOutput(ns("module_status_ui"))
  )
}


# --- Server Function ---
mod_goat_discourse_server <- function(id, active_tab, tab_value) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Server's namespace function
    
    LEGENDS_FOR_ANALYSIS <- sort(c(
      "Michael Jordan", "LeBron James", "Kobe Bryant", "Kevin Durant"
    ))
    
    PLAYER_CSV_MAP <- stats::setNames(
      paste0("data/", c("mj_goat_tw.csv", "lebron_goat_tw.csv", "kobe_goat_tw.csv", "kd_goat_tw.csv")),
      c("Michael Jordan", "LeBron James", "Kobe Bryant", "Kevin Durant")
    )
    
    PLAYER_A_WORD_COLOR_BRIGHT <- "#5CACE2"
    PLAYER_B_WORD_COLOR_BRIGHT <- "#FF6B6B"
    
    rv_goat <- reactiveValues(
      raw_data_cache = list(),
      nlp_results_cache = list(),
      module_initialized_flag = FALSE,
      status_message = "Module initializing...",
      error_message = NULL,
      comparison_results = NULL,
      comparison_error = NULL
    )
    
    load_and_preprocess_player_data <- function(player_name) {
      if (player_name %in% names(rv_goat$raw_data_cache) && !is.null(rv_goat$raw_data_cache[[player_name]])) {
        return(rv_goat$raw_data_cache[[player_name]])
      }
      csv_path <- PLAYER_CSV_MAP[[player_name]]
      if (is.null(csv_path) || !file.exists(csv_path)) {
        stop(glue::glue("CSV file for {player_name} not found at {csv_path %||% 'path_not_defined'}."))
      }
      print(glue::glue("MOD_GOAT: Loading and preprocessing data for {player_name} from {csv_path}"))
      all_cols_char <- readr::cols(.default = readr::col_character())
      raw_df <- readr::read_csv(csv_path, col_types = all_cols_char, locale = readr::locale(encoding = "UTF-8"), na = c("", "NA", "N/A"))
      required_cols <- c("text", "is_retweet", "lang", "status_id", "created_at", "favorite_count", "retweet_count")
      missing_cols <- setdiff(required_cols, names(raw_df))
      if (length(missing_cols) > 0) {
        stop(glue::glue("The following required columns are missing in the CSV for {player_name}: {paste(missing_cols, collapse=', ')}."))
      }
      cleaned_df <- raw_df %>%
        filter(tolower(is_retweet) == "false", tolower(lang) == "en") %>%
        filter(!is.na(status_id)) %>%
        distinct(status_id, .keep_all = TRUE) %>%
        mutate(
          created_at_dt = lubridate::ymd_hms(created_at, tz = "UTC", quiet = TRUE),
          text_for_nlp = text %||% "",
          text_for_nlp = stringr::str_to_lower(text_for_nlp),
          text_for_nlp = stringr::str_remove_all(text_for_nlp, "https?://\\S+|www\\.\\S+"),
          text_for_nlp = stringr::str_remove_all(text_for_nlp, "@\\w+"),
          text_for_nlp = stringr::str_remove_all(text_for_nlp, "#\\S+"),
          text_for_nlp = stringr::str_remove_all(text_for_nlp, "&|<|>"),
          text_for_nlp = stringr::str_replace_all(text_for_nlp, "[^a-zA-Z0-9\\s']", " "),
          text_for_nlp = stringr::str_remove_all(text_for_nlp, "\\b\\d+\\b"),
          text_for_nlp = iconv(text_for_nlp, from = "UTF-8", to = "ASCII//TRANSLIT"),
          text_for_nlp = stringr::str_replace_all(text_for_nlp, "\\s+", " "),
          text_for_nlp = stringr::str_trim(text_for_nlp),
          favorite_count = suppressWarnings(as.integer(favorite_count %||% "0")),
          retweet_count = suppressWarnings(as.integer(retweet_count %||% "0"))
        ) %>%
        filter(nchar(text_for_nlp) > 5) %>%
        select(status_id, original_text = text, text_for_nlp, created_at_dt,
               favorite_count, retweet_count)
      rv_goat$raw_data_cache[[player_name]] <- cleaned_df
      return(cleaned_df)
    }
    
    perform_nlp_analysis <- function(cleaned_player_df, player_name_being_analyzed) {
      if (player_name_being_analyzed %in% names(rv_goat$nlp_results_cache) && !is.null(rv_goat$nlp_results_cache[[player_name_being_analyzed]])) {
        return(rv_goat$nlp_results_cache[[player_name_being_analyzed]])
      }
      print(glue::glue("MOD_GOAT: Performing NLP analysis for {player_name_being_analyzed}"))
      if (is.null(cleaned_player_df) || nrow(cleaned_player_df) == 0) {
        return(list(sentiment_breakdown = tibble(sentiment = c("Positive", "Negative", "Neutral"), count = 0, percentage = 0),
                    word_cloud_data = tibble(word = glue::glue("No data for {player_name_being_analyzed}"), freq = 1, color = "grey", is_placeholder = TRUE),
                    texts_count = 0, positive_phrases = character(0), negative_phrases = character(0), error = NULL,
                    player_name_being_analyzed = player_name_being_analyzed))
      }
      player_name_parts <- tolower(stringr::str_split(player_name_being_analyzed, " ")[[1]])
      custom_stopwords_df <- tibble(word = unique(c("rt", "nba", "game", "games", "player", "players",
                                                    "team", "teams", "season", "playoffs", "finals",
                                                    "basketball", "hoops", "goat", "amp", "twitter",
                                                    "com", "http", "https", "u", "im", "tbh", "lol", "lmao", "pls", "tho",
                                                    "tb", "amp", "gt", "lt", "via", "it's", "don't", "can't", "he's", "she's", "we're", "they're",
                                                    player_name_parts)))
      tokenized_data <- cleaned_player_df %>%
        tidytext::unnest_tokens(word, text_for_nlp, token = "words") %>%
        anti_join(tidytext::stop_words, by = "word") %>%
        anti_join(custom_stopwords_df, by = "word") %>%
        filter(nchar(word) > 2 & !grepl("^[0-9]+$", word))
      if (nrow(tokenized_data) == 0) {
        return(list(sentiment_breakdown = tibble(sentiment = c("Positive", "Negative", "Neutral"), count = 0, percentage = 0),
                    word_cloud_data = tibble(word = glue::glue("No meaningful words for {player_name_being_analyzed}"), freq = 1, color = "grey", is_placeholder = TRUE),
                    texts_count = nrow(cleaned_player_df), positive_phrases = character(0), negative_phrases = character(0), error = NULL,
                    player_name_being_analyzed = player_name_being_analyzed))
      }
      bing_lexicon <- tryCatch(tidytext::get_sentiments("bing"), error = function(e) {NULL})
      if (is.null(bing_lexicon)) stop("CRITICAL: Bing sentiment lexicon failed to load.")
      word_sentiments <- tokenized_data %>%
        inner_join(bing_lexicon, by = "word")
      doc_sentiments <- word_sentiments %>%
        count(status_id, sentiment) %>%
        tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0))
      if (!"positive" %in% names(doc_sentiments)) doc_sentiments$positive <- 0
      if (!"negative" %in% names(doc_sentiments)) doc_sentiments$negative <- 0
      doc_sentiments <- doc_sentiments %>%
        mutate(score = positive - negative,
               sentiment_label = case_when(score > 0 ~ "Positive", score < 0 ~ "Negative", TRUE ~ "Neutral"))
      sentiment_summary_intermediate <- cleaned_player_df %>%
        select(status_id) %>%
        left_join(doc_sentiments %>% select(status_id, sentiment_label), by = "status_id") %>%
        mutate(sentiment_label = tidyr::replace_na(sentiment_label, "Neutral")) %>%
        count(sentiment_label) %>%
        rename(count = n)
      expected_sentiments <- tibble(sentiment_label = c("Positive", "Negative", "Neutral"))
      sentiment_breakdown_final <- expected_sentiments %>%
        left_join(sentiment_summary_intermediate, by = "sentiment_label") %>%
        mutate(count = tidyr::replace_na(count, 0),
               percentage = ifelse(nrow(cleaned_player_df) > 0, (count / nrow(cleaned_player_df)) * 100, 0)) %>%
        rename(sentiment = sentiment_label)
      word_cloud_df <- word_sentiments %>%
        count(word, sentiment, sort = TRUE) %>%
        rename(freq = n) %>%
        mutate(
          color = case_when(
            sentiment == "positive" ~ "#66BB6A",
            sentiment == "negative" ~ "#EF5350",
            TRUE ~ "#9E9E9E"
          ),
          is_placeholder = FALSE
        ) %>%
        filter(freq >= 1) %>%
        slice_head(n = 150)
      if (nrow(word_cloud_df) == 0) {
        word_cloud_df <- tibble(word = glue::glue("No prominent terms for {player_name_being_analyzed}"), freq = 1, color = "grey", is_placeholder = TRUE)
      }
      positive_tweets_df <- cleaned_player_df %>%
        filter(status_id %in% (doc_sentiments %>% filter(sentiment_label == "Positive") %>% pull(status_id)))
      negative_tweets_df <- cleaned_player_df %>%
        filter(status_id %in% (doc_sentiments %>% filter(sentiment_label == "Negative") %>% pull(status_id)))
      positive_phrases_list <- if(nrow(positive_tweets_df) > 0) {
        positive_tweets_df %>% slice_sample(n = min(3, nrow(.)), replace = FALSE) %>% pull(original_text)
      } else { character(0) }
      negative_phrases_list <- if(nrow(negative_tweets_df) > 0) {
        negative_tweets_df %>% slice_sample(n = min(3, nrow(.)), replace = FALSE) %>% pull(original_text)
      } else { character(0) }
      nlp_result <- list(
        sentiment_breakdown = sentiment_breakdown_final,
        word_cloud_data = word_cloud_df,
        texts_count = nrow(cleaned_player_df),
        positive_phrases = positive_phrases_list,
        negative_phrases = negative_phrases_list,
        error = NULL,
        player_name_being_analyzed = player_name_being_analyzed
      )
      rv_goat$nlp_results_cache[[player_name_being_analyzed]] <- nlp_result
      return(nlp_result)
    }
    
    observeEvent(active_tab(), {
      req(active_tab() == tab_value, !rv_goat$module_initialized_flag)
      rv_goat$status_message <- "Module active. Select a legend tab to load data."
      rv_goat$module_initialized_flag <- TRUE
      print("MOD_GOAT: GOAT Discourse module activated. Ready for legend selection.")
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    lapply(LEGENDS_FOR_ANALYSIS, function(player_name_iter) {
      player_id_safe <- gsub("[^A-Za-z0-9_]", "_", player_name_iter)
      
      player_nlp_data_r <- reactive({
        req(input$individual_legend_tabs == ns(paste0("legend_", player_name_iter)))
        current_player_name <- player_name_iter
        
        # print(glue::glue("DEBUG MOD_GOAT ({current_player_name}): `player_nlp_data_r` (reactive) triggered for tab."))
        
        if (current_player_name %in% names(rv_goat$nlp_results_cache) && !is.null(rv_goat$nlp_results_cache[[current_player_name]]$sentiment_breakdown)) {
          # print(glue::glue("DEBUG MOD_GOAT ({current_player_name}): Using cached NLP results."))
          return(rv_goat$nlp_results_cache[[current_player_name]])
        }
        rv_goat$status_message <- glue::glue("Loading and analyzing data for {current_player_name}...")
        rv_goat$error_message <- NULL
        tryCatch({
          cleaned_data <- load_and_preprocess_player_data(current_player_name)
          nlp_results <- perform_nlp_analysis(cleaned_data, current_player_name)
          # print(glue::glue("DEBUG MOD_GOAT ({current_player_name}): NLP analysis complete. Result structure:"))
          # print(str(nlp_results))
          rv_goat$status_message <- glue::glue("Analysis complete for {current_player_name}.")
          return(nlp_results)
        }, error = function(e) {
          err_msg <- glue::glue("Error processing data for {current_player_name}: {e$message}")
          rv_goat$error_message <- err_msg
          # print(glue::glue("DEBUG MOD_GOAT ({current_player_name}): Error in `player_nlp_data_r`: {err_msg}"))
          temp_error_result <- list(error = err_msg, texts_count = 0, player_name_being_analyzed = current_player_name)
          return(temp_error_result)
        })
      })
      
      output[[paste0("analysis_output_", player_id_safe)]] <- renderUI({
        render_individual_player_analysis_ui_content(ns, player_name_iter, player_nlp_data_r())
      })
      
      output[[paste0("wordcloud_", player_id_safe)]] <- wordcloud2::renderWordcloud2({
        nlp_res <- player_nlp_data_r()
        req(nlp_res, is.null(nlp_res$error), !is.null(nlp_res$word_cloud_data))
        word_cloud_df <- nlp_res$word_cloud_data
        if (isTRUE(word_cloud_df$is_placeholder[1])) {
          return(wordcloud2::wordcloud2(data.frame(word = word_cloud_df$word[1], freq = 1),
                                        color = "#aaa", size = 0.7, backgroundColor = "#111111"))
        }
        req("word" %in% names(word_cloud_df), "freq" %in% names(word_cloud_df), "color" %in% names(word_cloud_df))
        req(nrow(word_cloud_df) > 0)
        wordcloud2::wordcloud2(
          data = word_cloud_df[, c("word", "freq")],
          color = word_cloud_df$color,
          backgroundColor = "#111111",
          size = 0.6,
          shape = 'circle',
          fontFamily = "Fira Code, monospace",
          ellipticity = 0.65,
          minRotation = -pi/4, maxRotation = pi/4, rotateRatio = 0.4
        )
      })
      
      output[[paste0("sentiment_donut_chart_", player_id_safe)]] <- echarts4r::renderEcharts4r({
        nlp_res <- player_nlp_data_r()
        req(nlp_res, is.null(nlp_res$error), !is.null(nlp_res$sentiment_breakdown))
        
        sentiment_data_for_chart <- nlp_res$sentiment_breakdown %>%
          filter(count > 0)
        
        if (nrow(sentiment_data_for_chart) == 0) {
          return(
            echarts4r::e_charts() %>%
              echarts4r::e_title(text = "No sentiment data to display", textStyle = list(color = "#888888", fontFamily = "Fira Code, monospace")) %>%
              echarts4r::e_theme_custom('{"color":["#333"], "backgroundColor": "transparent"}')
          )
        }
        
        sentiment_colors <- c(Positive = "#66BB6A", Negative = "#EF5350", Neutral  = "#78909C")
        
        chart <- sentiment_data_for_chart %>%
          echarts4r::e_charts(sentiment) %>%
          echarts4r::e_pie(
            count,
            radius = c("50%", "70%"),
            itemStyle = list(borderRadius = 5, borderColor = '#1e1e1e', borderWidth = 2),
            label = list(
              show = TRUE,
              formatter = "{b}\n{d}%",
              color = "#f0f0f0",
              fontFamily = "Fira Code, monospace"
            ),
            emphasis = list(label = list(show = TRUE, fontSize = 16, fontWeight = "bold"))
          ) %>%
          echarts4r::e_color(unname(sentiment_colors[sentiment_data_for_chart$sentiment])) %>%
          echarts4r::e_legend(show = FALSE) %>%
          echarts4r::e_tooltip(trigger = "item", formatter = "{b}: {c} tweets ({d}%)") %>%
          echarts4r::e_theme_custom('{"textStyle":{"fontFamily":"Fira Code, monospace","color":"#f0f0f0"}, "backgroundColor": "transparent"}')
        
        return(chart)
      })
      
      output[[paste0("positive_phrases_", player_id_safe)]] <- renderUI({
        nlp_res <- player_nlp_data_r()
        req(nlp_res, is.null(nlp_res$error), !is.null(nlp_res$positive_phrases))
        if (length(nlp_res$positive_phrases) == 0) return(tags$p("No distinct positive phrases found.", style="font-size:0.9em; color:#E0E0E0; text-align:center; font-style:italic;"))
        tagList(lapply(nlp_res$positive_phrases, function(phrase) {
          tags$blockquote(class="twitter-phrase positive-phrase-item", phrase)
        }))
      })
      
      output[[paste0("negative_phrases_", player_id_safe)]] <- renderUI({
        nlp_res <- player_nlp_data_r()
        req(nlp_res, is.null(nlp_res$error), !is.null(nlp_res$negative_phrases))
        if (length(nlp_res$negative_phrases) == 0) return(tags$p("No distinct negative phrases found.", style="font-size:0.9em; color:#E0E0E0; text-align:center; font-style:italic;"))
        tagList(lapply(nlp_res$negative_phrases, function(phrase) {
          tags$blockquote(class="twitter-phrase negative-phrase-item", phrase)
        }))
      })
    }) # End lapply
    
    # --- Comparison Tab Logic ---
    observeEvent(input$run_comparison_button, {
      player_A_name <- input$compare_player_A
      player_B_name <- input$compare_player_B
      rv_goat$comparison_results <- NULL
      rv_goat$comparison_error <- NULL
      if (is.null(player_A_name) || player_A_name == "" || is.null(player_B_name) || player_B_name == "") {
        rv_goat$comparison_error <- "Please select two legends to compare."
        return()
      }
      if (player_A_name == player_B_name) {
        rv_goat$comparison_error <- "Please select two different legends."
        return()
      }
      rv_goat$status_message <- glue::glue("Comparing {player_A_name} and {player_B_name}...")
      tryCatch({
        if (!player_A_name %in% names(rv_goat$nlp_results_cache) || is.null(rv_goat$nlp_results_cache[[player_A_name]]$sentiment_breakdown)) {
          cleaned_A <- load_and_preprocess_player_data(player_A_name)
          perform_nlp_analysis(cleaned_A, player_A_name)
        }
        if (!player_B_name %in% names(rv_goat$nlp_results_cache) || is.null(rv_goat$nlp_results_cache[[player_B_name]]$sentiment_breakdown)) {
          cleaned_B <- load_and_preprocess_player_data(player_B_name)
          perform_nlp_analysis(cleaned_B, player_B_name)
        }
        results_A <- rv_goat$nlp_results_cache[[player_A_name]]
        results_B <- rv_goat$nlp_results_cache[[player_B_name]]
        if (!is.null(results_A$error) || !is.null(results_B$error)) {
          stop(paste("Error in NLP results for one or both players:", results_A$error %||% "", results_B$error %||% ""))
        }
        comp_sentiment_data <- bind_rows(
          results_A$sentiment_breakdown %>% mutate(player = player_A_name),
          results_B$sentiment_breakdown %>% mutate(player = player_B_name)
        )
        rv_goat$comparison_results <- list(
          player_A = player_A_name,
          player_B = player_B_name,
          sentiment_data = comp_sentiment_data,
          results_A_nlp = results_A,
          results_B_nlp = results_B,
          texts_count_A = results_A$texts_count,
          texts_count_B = results_B$texts_count
        )
        rv_goat$status_message <- "Comparison complete."
      }, error = function(e) {
        err_msg <- glue::glue("Error generating comparison data: {e$message}")
        rv_goat$comparison_error <- err_msg
        print(err_msg)
      })
    })
    
    output$comparison_display_output_sentiment_chart_only <- renderUI({
      if (!is.null(rv_goat$comparison_error)) {
        return(tags$div(class="validation-error-message", rv_goat$comparison_error))
      }
      comp_res <- rv_goat$comparison_results
      if (is.null(comp_res)) {
        return(tags$p("Select two different legends and click 'Compare Legends'.", style="text-align:center; color:#aaa; margin-top:20px;"))
      }
      tagList(
        h5(glue::glue("Sentiment Comparison: {comp_res$player_A} vs. {comp_res$player_B}"), style="text-align:center;"),
        p(glue::glue("Based on {scales::comma(comp_res$texts_count_A)} tweets for {comp_res$player_A} and {scales::comma(comp_res$texts_count_B)} tweets for {comp_res$player_B}."), style="text-align:center; font-size:0.9em; color:#aaa;"),
        div(class = "goat-sentiment-chart-container",
            plotly::plotlyOutput(ns("comparative_sentiment_chart_plotly"), height="400px")
        )
      )
    })
    
    output$comparative_sentiment_chart_plotly <- plotly::renderPlotly({
      comp_res <- rv_goat$comparison_results
      req(comp_res, !is.null(comp_res$sentiment_data))
      player_A_color_plotly <- "#1d428a"; player_B_color_plotly <- "#c9082a"
      plot_data <- comp_res$sentiment_data %>%
        mutate( player = factor(player, levels = c(comp_res$player_A, comp_res$player_B)),
                percentage = percentage / 100,
                hover_text = paste0("Player: ", player, "<br>", "Sentiment: ", sentiment, "<br>", "Tweets: ", scales::percent(percentage, accuracy=0.1))) %>%
        arrange(player, factor(sentiment, levels = c("Positive", "Neutral", "Negative")))
      plot_colors_vector <- c(player_A_color_plotly, player_B_color_plotly)
      fig <- plotly::plot_ly(data = plot_data, x = ~sentiment, y = ~percentage, color = ~player, colors = plot_colors_vector, type = 'bar', hoverinfo = 'text', text = ~hover_text)
      fig <- fig %>% plotly::layout(barmode = 'group', yaxis = list(title = list(text = "Percentage of Tweets", font = list(color = "#f0f0f0")), tickformat = ".0%", gridcolor = "rgba(100,100,100,0.3)", zerolinecolor = "rgba(150,150,150,0.5)", tickfont = list(color = "#f0f0f0")), xaxis = list(title = list(text = "Sentiment Category", font = list(color = "#f0f0f0")), gridcolor = "rgba(100,100,100,0.3)", tickfont = list(color = "#f0f0f0"), categoryorder = "array", categoryarray = c("Positive", "Neutral", "Negative")), legend = list(orientation = "h", y = 1.15, x = 0.5, xanchor = 'center', bgcolor = 'rgba(0,0,0,0)', font = list(color="#f0f0f0"), traceorder = "normal"), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', font = list(color = "#f0f0f0", family = "Fira Code, monospace"), bargap = 0.2 )
      return(fig)
    })
    
    render_single_comparison_wordcloud <- function(player_nlp_results, sentiment_filter_color, player_main_word_color) {
      req(player_nlp_results, !is.null(player_nlp_results$word_cloud_data))
      
      player_name_for_log <- "Unknown (Comparison Cloud)"
      if (!is.null(rv_goat$comparison_results)) {
        if (!is.null(player_nlp_results) && !is.null(rv_goat$comparison_results$results_A_nlp) && identical(player_nlp_results, rv_goat$comparison_results$results_A_nlp)) {
          player_name_for_log <- rv_goat$comparison_results$player_A
        } else if (!is.null(player_nlp_results) && !is.null(rv_goat$comparison_results$results_B_nlp) && identical(player_nlp_results, rv_goat$comparison_results$results_B_nlp)) {
          player_name_for_log <- rv_goat$comparison_results$player_B
        }
      }
      if(is.null(player_name_for_log) || player_name_for_log == "Unknown (Comparison Cloud)") {
        player_name_for_log <- player_nlp_results$player_name_being_analyzed %||% "Unknown (Comparison Cloud)"
      }
      
      word_data_for_cloud <- player_nlp_results$word_cloud_data %>%
        filter(color == sentiment_filter_color)
      
      if (nrow(word_data_for_cloud) == 0 || isTRUE(word_data_for_cloud$is_placeholder[1])) {
        plot.new(); box(col="#444444")
        text(0.5, 0.5, "No relevant terms found.", col = "#aaaaaa", cex=1.2)
        return()
      }
      
      max_freq_this_cloud <- if(nrow(word_data_for_cloud) > 0) max(word_data_for_cloud$freq, na.rm = TRUE) else 0
      min_cex_val <- 0.6; max_cex_val <- 3.5
      
      if (max_freq_this_cloud <= 0) { cloud_scale_val <- c(0.5, 0.5)
      } else if (max_freq_this_cloud == 1) { cloud_scale_val <- c(1.2, 1.2)
      } else if (max_freq_this_cloud <= 3) { cloud_scale_val <- c(max_cex_val * 0.6, min_cex_val + 0.2)
      } else if (max_freq_this_cloud <= 7) { cloud_scale_val <- c(max_cex_val * 0.8, min_cex_val + 0.1)
      } else { cloud_scale_val <- c(max_cex_val, min_cex_val) }
      if (cloud_scale_val[1] < cloud_scale_val[2]) cloud_scale_val[1] <- cloud_scale_val[2] + 0.2
      
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      tryCatch({
        par(family = "sans", bg = "#111111")
      }, warning = function(w){
      }, error = function(e){
      })
      
      wordcloud::wordcloud(
        words = word_data_for_cloud$word,
        freq = word_data_for_cloud$freq,
        scale = cloud_scale_val,
        max.words = 50,
        random.order = FALSE,
        rot.per = 0.1,
        colors = player_main_word_color
      )
    }
    
    output$positive_cloud_title_A <- renderText({ req(rv_goat$comparison_results$player_A); rv_goat$comparison_results$player_A })
    output$positive_cloud_title_B <- renderText({ req(rv_goat$comparison_results$player_B); rv_goat$comparison_results$player_B })
    output$negative_cloud_title_A <- renderText({ req(rv_goat$comparison_results$player_A); rv_goat$comparison_results$player_A })
    output$negative_cloud_title_B <- renderText({ req(rv_goat$comparison_results$player_B); rv_goat$comparison_results$player_B })
    
    output$positive_cloud_player_A <- renderPlot({
      comp_res <- rv_goat$comparison_results; req(comp_res, comp_res$results_A_nlp)
      render_single_comparison_wordcloud(comp_res$results_A_nlp, "#66BB6A", PLAYER_A_WORD_COLOR_BRIGHT)
    }, bg="transparent", height=380)
    
    output$positive_cloud_player_B <- renderPlot({
      comp_res <- rv_goat$comparison_results; req(comp_res, comp_res$results_B_nlp)
      render_single_comparison_wordcloud(comp_res$results_B_nlp, "#66BB6A", PLAYER_B_WORD_COLOR_BRIGHT)
    }, bg="transparent", height=380)
    
    output$negative_cloud_player_A <- renderPlot({
      comp_res <- rv_goat$comparison_results; req(comp_res, comp_res$results_A_nlp)
      render_single_comparison_wordcloud(comp_res$results_A_nlp, "#EF5350", PLAYER_A_WORD_COLOR_BRIGHT)
    }, bg="transparent", height=380)
    
    output$negative_cloud_player_B <- renderPlot({
      comp_res <- rv_goat$comparison_results; req(comp_res, comp_res$results_B_nlp)
      render_single_comparison_wordcloud(comp_res$results_B_nlp, "#EF5350", PLAYER_B_WORD_COLOR_BRIGHT)
    }, bg="transparent", height=380)
    
    output$module_status_ui <- renderUI({
      current_status <- rv_goat$status_message; current_error <- rv_goat$error_message
      if (!is.null(current_error)) {
        tags$div(class="validation-error-message", current_error)
      } else if (!rv_goat$module_initialized_flag) {
        tags$p(current_status %||% "Initializing...", style="text-align:center; color:#aaa; padding:20px;")
      } else if (!is.null(current_status) &&
                 current_status != "Comparison complete." &&
                 !grepl("Analysis complete for", current_status) &&
                 current_status != "Module active. Select a legend tab to load data." ) {
        tags$p(current_status, style="text-align:center; color:#aaa; font-size:0.9em; padding:10px;")
      } else {
        NULL
      }
    })
    
  }) # end moduleServer
}
# --- END FILE: modules/mod_goat_discourse.R (Full, Complete, Final Version - KPI Boxes Removed) ---