## story-recommender-app

## load required libraries
require(shiny)
require(shinyjs)
require(jsonlite)
require(curl)
require(DT)
require(Biobase)

## read story metadata from web
story_metadata <- fromJSON("http://themeontology.org/json.php?action=storydefinitions")
rownames(story_metadata) <- story_metadata[, 1]
colnames(story_metadata) <- c("StoryID", "Title", "Date", "Summary")
story_metadata <- story_metadata[-1, ]
story_ids <- rownames(story_metadata)
no_of_stories <- nrow(story_metadata)

## read theme data from web
story_themedata <- fromJSON("http://themeontology.org/json.php?action=metathemedata")
themes <- sort(names(story_themedata[[1]]))
no_of_themes <- length(themes)

## construct list of story lists each containing story metadata and themes
story_data <- vector("list", length = no_of_stories)
names(story_data) <- story_ids
for(i in 1 : no_of_stories) {
  mystory_data <- vector("list", length = 7)
  names(mystory_data) <- c("StoryID", "Title", "Date", "Summary", "CentralAndPeripheralThemes", "CentralThemes", "PeripheralThemes")
  mystory_data$StoryID <- story_metadata[i, "StoryID"]
  mystory_data$Title <- story_metadata[i, "Title"]
  mystory_data$Date <- story_metadata[i, "Date"]
  mystory_data$Summary <- story_metadata[i, "Summary"]
  story_data[[i]] <- mystory_data
}
for(i in 1 : no_of_themes) {
  mytheme <- themes[i]
  mytheme_data <- story_themedata[[1]][mytheme][[1]]
  no_of_stories_featuring_mytheme <- nrow(mytheme_data)

  if(no_of_stories_featuring_mytheme > 0) {
    story_ids_for_stories_featuring_mytheme <- unique(mytheme_data[, 1]) 

    for(j in 1 : no_of_stories_featuring_mytheme) {
      story_id_for_story_featuring_mytheme <- story_ids_for_stories_featuring_mytheme[j]
      level_of_mytheme_in_story_featuring_mytheme <- mytheme_data[j, 2]
      story_data[[story_id_for_story_featuring_mytheme]]$CentralAndPeripheralThemes <- append(story_data[[story_id_for_story_featuring_mytheme]]$CentralAndPeripheralThemes, mytheme)

      if(level_of_mytheme_in_story_featuring_mytheme == "choice" || level_of_mytheme_in_story_featuring_mytheme == "major") {
        story_data[[story_id_for_story_featuring_mytheme]]$CentralThemes <- append(story_data[[story_id_for_story_featuring_mytheme]]$CentralThemes, mytheme)
      } else if(level_of_mytheme_in_story_featuring_mytheme == "minor") {
        story_data[[story_id_for_story_featuring_mytheme]]$PeripheralThemes <- append(story_data[[story_id_for_story_featuring_mytheme]]$PeripheralThemes, mytheme)
      }
    }
  }
}

## excise stories without any themes
blacklisted_story_ids <- NULL
for(i in 1 : no_of_stories) {
  mystory_id <- story_ids[i]
  no_of_themes_featured_in_mystory <- length(unlist(story_data[[mystory_id]]$CentralAndPeripheralThemes))
  if(no_of_themes_featured_in_mystory == 0) {
    blacklisted_story_ids <- c(blacklisted_story_ids, mystory_id)
  }
}
story_data[blacklisted_story_ids] <- NULL
story_ids <- names(story_data)
no_of_stories <- length(story_data)

## jury-rig story ids
has_colon_in_story_id <- grepl(":", story_ids)
story_menu_choices <- character(no_of_stories)
for(i in 1 : no_of_stories) {
  if(has_colon_in_story_id[i] == TRUE) {
    story_menu_choices[i] <- names(story_data)[i]
  } else {
    story_menu_choices[i] <- paste0(names(story_data)[i], ": ", story_data[[i]]$Title)
  }
}
story_menu_choices <- c("", story_menu_choices)

## construct theme pairwise similarity matrix
domains <- story_themedata[[5]]
theme_pariwise_distance_data <- fromJSON("http://themeontology.org/json.php?action=themesimilarity")
theme_labels <- theme_pariwise_distance_data[[1]]
sorted_theme_labels <- sort(theme_labels)
no_of_theme_labels <- length(theme_labels)
theme_pariwise_distance_data <- theme_pariwise_distance_data[[2]]
colnames(theme_pariwise_distance_data) <- c("ThemeId1", "ThemeId2", "D1", "D2")
no_of_theme_pairs <- nrow(theme_pariwise_distance_data)
pairwise_similarity_matrix <- mat.or.vec(no_of_theme_labels, no_of_theme_labels)
rownames(pairwise_similarity_matrix) <- sorted_theme_labels
colnames(pairwise_similarity_matrix) <- sorted_theme_labels
diag(pairwise_similarity_matrix) <- 1
for(i in 1 : no_of_theme_pairs) {
  theme_1 <- theme_labels[theme_pariwise_distance_data[i, "ThemeId1"] + 1]
  theme_2 <- theme_labels[theme_pariwise_distance_data[i, "ThemeId2"] + 1]
  d1 <- theme_pariwise_distance_data[i, "D1"]
  d2 <- theme_pariwise_distance_data[i, "D2"]
  if(theme_1 < theme_2) {
    pairwise_similarity_matrix[theme_1, theme_2] <- 1 / (max(d1, d2) + 1)
  } else {
    pairwise_similarity_matrix[theme_2, theme_1] <- 1 / (max(d1, d2) + 1)
  }
}

## shiny ui
ui <- fluidPage(
  useShinyjs(),
  headerPanel('Story recommender'),
  sidebarPanel(
    div(
      id = "control-panel",
      selectInput(
        inputId = 'selected_story_id',
        label = 'Choose a story:',
        choices = story_menu_choices
      ),
      radioButtons(
        inputId = 'theme_level',
        label = 'Theme levels:',
        choices = list("Central and Peripheral" = "CentralAndPeripheralThemes", "Central" = "CentralThemes", "Peripheral" = "PeripheralThemes"), 
        selected = "CentralAndPeripheralThemes"
      ),
      selectInput(
        inputId = 'similarity_function',
        label = 'Similarity function:',
        choices = c("cosine", "cosine tf-idf", "soft cardinality", "soft cardinality tf-idf")
      ),
      numericInput(
        inputId = 'min_overlap',
        label = 'Minimum theme overlap:',
        value = 1,
        min = 1,
        max = 10
      ),
      fileInput(
        inputId = 'background_file',
        label = 'Background storyset (SMT file format):',
        accept = c("text/smt", ".smt")
      ),
      textAreaInput(
        inputId = "blacklisted_themes",
        label = "Blacklisted themes (input one theme per line):",
        value = "",
        height = "100px"
      )
    ),
    actionButton("resetAll", "Reset")
  ),
  conditionalPanel(condition = 'input.selected_story_id == "" ', mainPanel(h4("Choose a story to get a ranked list of similar stories"))),
  conditionalPanel(
    condition = 'input.selected_story_id != "" ',
    mainPanel(
      #h4("Ranked list of similar stories"),
      DT::dataTableOutput(outputId = 'result_table'),
      h4("Selected story summary"),
      verbatimTextOutput("summary")
    )
  )
)

# shiny server
server <- function(input, output, session) {
  data <- reactive({
    ## hard coded soft cardinality parameter
    p <- 1

    ## process background storyset
    if(is.null(input$background_file)) {
      background_story_ids <- story_ids
      no_of_background_stories <- no_of_stories
    } else {
      inlines <- readLines(input$background_file$datapath)
      no_of_storysets <- length(inlines)
      background_story_ids <- NULL
      for (i in 1 : no_of_storysets) {
        inline <- inlines[i]
        inline_tokens <- unlist(strsplit(inline, split = "\t"))
        background_story_ids <- c(background_story_ids, inline_tokens[3 : length(inline_tokens)])
      }
      background_story_ids <- intersect(story_ids, background_story_ids)
      no_of_background_stories <- length(background_story_ids)
      invalid_background_story_ids <- setdiff(background_story_ids, story_ids)
      no_of_invalid_background_story_ids <- length(invalid_background_story_ids)
      if (no_of_invalid_background_story_ids > 0) {
        alert(paste("Invalid story ids identified:", invalid_background_story_ids))
      }
    }

    ## process blacklisted themes
    blacklisted_themes <- ""
    if(!is.null(input$blacklisted_themes)) {
      raw_blacklisted_themes <- unlist(strsplit(input$blacklisted_themes, split = '\n'))
      blacklisted_themes <- intersect(themes, raw_blacklisted_themes)
      output$blacklisted_themes <- renderText(blacklisted_themes)
      no_of_blacklisted_themes <- length(blacklisted_themes)
      invalid_blacklisted_themes <- setdiff(raw_blacklisted_themes, themes)
      no_of_invalid_blacklisted_themes <- length(invalid_blacklisted_themes)
      output$inv_blacklisted_names = renderText("")
      # if (no_of_invalid_blacklisted_themes > 0) {
      #   alert(paste("Invalid blacklisted themes identified:", invalid_blacklisted_themes))
      # }
    }

    ## update possible story menu options
    has_colon_in_background_story_id <- grepl(":", background_story_ids)
    story_menu_choices <- character(no_of_background_stories)
    for(i in 1 : no_of_background_stories) {
      background_story_id <- background_story_ids[i]
      if(has_colon_in_background_story_id[i] == TRUE) {
        story_menu_choices[i] <- background_story_id
      } else {
        story_menu_choices[i] <- paste0(background_story_id, ": ", story_data[[background_story_id]]$Title)
      }
    }
    story_menu_choices <- c("", story_menu_choices)

    ## initialize output data frame
    result <- data.frame(
      StoryID = unname(subListExtract(story_data, name = "StoryID", simplify = TRUE, keep.names = TRUE)[background_story_ids]),
      Title = unname(subListExtract(story_data, name = "Title", simplify = TRUE, keep.names = TRUE)[background_story_ids]),
      SimilarityScore = numeric(no_of_background_stories),
      CommonThemes = character(no_of_background_stories),
      CommonThemeCount = numeric(no_of_background_stories),
      stringsAsFactors = FALSE
    )
    
    ## jury-rig inputed story id
    if(grepl('movie: |nonfiction: |novel: |tvseries: ', input$selected_story_id)) {
      selected_story_id <- input$selected_story_id
    } else {
      selected_story_id <- unlist(strsplit(input$selected_story_id, split = ": "))[1]
    }
    
    ## calculate background theme frequencies
    background_theme_frequency_data <- vector("list", length = 3)
    names(background_theme_frequency_data) <- c("CentralAndPeripheralThemes", "CentralThemes", "PeripheralThemes")
    bag_of_central_and_peripheral_themes <- unname(unlist(subListExtract(L = story_data, name = "CentralAndPeripheralThemes", keep.names = TRUE)[background_story_ids]))
    mytable <- table(bag_of_central_and_peripheral_themes)
    mythemes <- names(mytable)
    background_theme_frequency_data$CentralAndPeripheralThemes <- as.numeric(mytable)
    names(background_theme_frequency_data$CentralAndPeripheralThemes) <- mythemes
    bag_of_central_themes <- unname(unlist(subListExtract(L = story_data, name = "CentralThemes", keep.names = TRUE)[background_story_ids]))
    mytable <- table(bag_of_central_themes)
    mythemes <- names(mytable)
    background_theme_frequency_data$CentralThemes <- as.numeric(mytable)
    names(background_theme_frequency_data$CentralThemes) <- mythemes
    bag_of_peripheral_themes <- unname(unlist(subListExtract(L = story_data, name = "PeripheralThemes", keep.names = TRUE)[background_story_ids]))
    mytable <- table(bag_of_peripheral_themes)
    mythemes <- names(mytable)
    background_theme_frequency_data$PeripheralThemes <- as.numeric(mytable)
    names(background_theme_frequency_data$PeripheralThemes) <- mythemes

    ## get the selected story themes
    selected_story_themes <- setdiff(unlist(unname(story_data[[selected_story_id]][input$theme_level])), unlist(strsplit(input$blacklisted_themes, split = '\n')))
    no_of_selected_story_themes <- length(selected_story_themes)
    
    ## calculate similar stories so long as the selected story features themes
    if(no_of_selected_story_themes > 0) {
      for(i in 1 : no_of_background_stories) {
        refstory_id <- background_story_ids[i]
        refstory_themes <- setdiff(unlist(unname(story_data[[refstory_id]][input$theme_level])), unlist(strsplit(input$blacklisted_themes, split = '\n')))
        no_of_refstory_themes <- length(refstory_themes)
        common_themes <- intersect(selected_story_themes, refstory_themes)
        no_of_common_themes <- length(common_themes)
        if(no_of_common_themes > 0) {
          common_themes <- sort(common_themes)
        }
        union_themes <- sort(union(selected_story_themes, refstory_themes))
        no_of_union_themes <- length(union_themes)
        result[i, "CommonThemes"] <- paste0(common_themes, collapse = ", ")
        result[i, "CommonThemeCount"] <- no_of_common_themes

        if(no_of_refstory_themes > 0) {
          if(input$similarity_function == "cosine") {
            ## cosine similarity score
            result[i, "SimilarityScore"] <- no_of_common_themes / sqrt(no_of_selected_story_themes * no_of_refstory_themes)
          } else if(input$similarity_function == "cosine tf-idf") {
            refstory_themes <- unlist(unname(story_data[[i]][input$theme_level]))
            ## cosine similarity score with tf-idf weighting
            query_tfidf <- log(1 + no_of_background_stories / background_theme_frequency_data[[input$theme_level]][selected_story_themes])
            ref_tfidf <- log(1 + no_of_background_stories / background_theme_frequency_data[[input$theme_level]][refstory_themes])
            result[i, "SimilarityScore"] <- sum(query_tfidf[common_themes] * ref_tfidf[common_themes]) / sqrt(sum(query_tfidf * query_tfidf) * sum(ref_tfidf * ref_tfidf))
          } else if(input$similarity_function == "soft cardinality") {
            ## soft cardinality similarity score
            qsmat <- pairwise_similarity_matrix[selected_story_themes, selected_story_themes]
            if (no_of_selected_story_themes > 1) {
              query_card <- sum(1 / apply(qsmat ^ p, 1, sum))
            } else {
              query_card <- 1 / (qsmat ^ p)
            }
            rsmat <- pairwise_similarity_matrix[refstory_themes, refstory_themes]
            if (no_of_refstory_themes > 1) {
              ref_card <- sum(1 / apply(rsmat ^ p, 1, sum))
            } else {
              ref_card <- 1 / (rsmat ^ p)
            }
            
            ## intersection cardinality
            usmat <- pairwise_similarity_matrix[union_themes, union_themes]
            if (no_of_union_themes > 1) {
              union_card <- sum(1 / apply(usmat ^ p, 1, sum))
            } else {
              union_card <- 1 / (usmat ^ p)
            }
            intersection_card <- query_card + ref_card - union_card
            
            ## cosine similarity
            result[i, "SimilarityScore"] <- intersection_card / sqrt(query_card * ref_card)
          } else if(input$similarity_function == "soft cardinality tf-idf") {
            ## soft cardinality similarity score with tf-idf weighting
            qsmat <- pairwise_similarity_matrix[selected_story_themes, selected_story_themes]
            query_tfidf <- log(1 + no_of_background_stories / background_theme_frequency_data[[input$theme_level]][selected_story_themes])
            if(no_of_selected_story_themes > 1) {
              query_card <- sum(query_tfidf / apply(qsmat ^ p, 1, sum))
            } else {
              query_card <- query_tfidf / (qsmat ^ p)
            }
            rsmat <- pairwise_similarity_matrix[refstory_themes, refstory_themes]
            ref_tfidf <- log(1 + no_of_background_stories / background_theme_frequency_data[[input$theme_level]][refstory_themes])
            if(no_of_refstory_themes > 1) {
              ref_card <- sum(ref_tfidf / apply(rsmat ^ p, 1, sum))
            } else {
              ref_card <- ref_tfidf / (rsmat ^ p)
            }
            
            ## intersection tf-idf weighted soft cardinality
            usmat <- pairwise_similarity_matrix[union_themes, union_themes]
            union_tfidf <- log(1 + no_of_background_stories / background_theme_frequency_data[[input$theme_level]][union_themes])
            if(no_of_union_themes > 1) {
              union_card <- sum(union_tfidf / apply(usmat ^ p, 1, sum))
            } else {
              union_card <- union_tfidf / (usmat ^ p)
            }
            intersection_card <- query_card + ref_card - union_card
            
            ## cosine similarity with tf-idf weighting
            result[i, "SimilarityScore"] <- intersection_card / sqrt(query_card * ref_card)
          }
        } else {
          result[i, "SimilarityScore"] <- 0
        }
      }
    }
    
    result <- result[-which(result$StoryID == selected_story_id), ]
    result <- result[which(result$CommonThemeCount >= input$min_overlap), ]
    result <- result[with(result, order(-SimilarityScore)),]
    return(result)
  })
  
  # reactive update of the select input for story_ids
  outVar = reactive({
    ## update background story ids
    if(is.null(input$background_file)) {
      background_story_ids <- story_ids
      no_of_background_stories <- no_of_stories
    } else {
      inlines <- readLines(input$background_file$datapath)
      no_of_storysets <- length(inlines)
      background_story_ids <- NULL
      for (i in 1 : no_of_storysets) {
        inline <- inlines[i]
        inline_tokens <- unlist(strsplit(inline, split = "\t"))
        background_story_ids <- c(background_story_ids, inline_tokens[3 : length(inline_tokens)])
      }
      background_story_ids <- intersect(story_ids, background_story_ids)
      no_of_background_stories <- length(background_story_ids)
      invalid_background_story_ids <- setdiff(background_story_ids, story_ids)
      no_of_invalid_background_story_ids <- length(invalid_background_story_ids)
      if (no_of_invalid_background_story_ids > 0) {
        alert(paste("Invalid story ids identified:", invalid_background_story_ids))
      }
    }
    
    ## update possible story menu options
    has_colon_in_background_story_id <- grepl(":", background_story_ids)
    story_menu_choices <- character(no_of_background_stories)
    for(i in 1 : no_of_background_stories) {
      background_story_id <- background_story_ids[i]
      if(has_colon_in_background_story_id[i] == TRUE) {
        story_menu_choices[i] <- background_story_id
      } else {
        story_menu_choices[i] <- paste0(background_story_id, ": ", story_data[[background_story_id]]$Title)
      }
    }
    c("", story_menu_choices)
    
    })
    observe({
      updateSelectInput(session, "selected_story_id",
                        choices = outVar()
    )}
  )
  
  # generate table of similar stories
  output$result_table <- DT::renderDataTable(DT::datatable(
    data(),
    colnames = c('Story ID', 'Title', 'Score', 'Common Themes', 'Count'), 
    options = list(pageLength = 10),
    rownames = FALSE
    ) %>% formatRound(columns=c('SimilarityScore'), digits = 3)
  )
  
  ## selected story summary info
  output$summary <- renderText({
    if (grepl('movie: |nonfiction: |novel: |tvseries: ', input$selected_story_id)) {
      selected_story_id <- input$selected_story_id
    } else if (input$selected_story_id == "") {
      selected_story_id <- "movie: Serenity (2005)"
    } else {
      selected_story_id <- unlist(strsplit(input$selected_story_id, split = ": "))[1]
    }
    paste(
      paste0("Story ID: ", selected_story_id),
      paste0("Title: ", story_data[[selected_story_id]]$Title),
      paste0("Air Date: ", story_data[[selected_story_id]]$Date),
      paste0("Summary: ", story_data[[selected_story_id]]$Summary, "\n"),
      paste0("Central Themes:\n", paste0(story_data[[selected_story_id]]$CentralThemes, collapse = "\n"), "\n"),
      paste0("Peripheral Themes:\n", paste0(story_data[[selected_story_id]]$PeripheralThemes, collapse = "\n"), "\n"),
      sep = "\n"
    )
  })
  
  ## Reset all inputs to default values
  observeEvent(input$resetAll, {
    reset("control-panel")
  })
}

shinyApp(ui = ui, server = server)
