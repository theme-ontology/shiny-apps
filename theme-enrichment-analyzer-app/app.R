## theme-enrichment-analyzer-app

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
metathemes <- sort(names(story_themedata[[2]]))
no_of_metathemes <- length(metathemes)

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
  story_ids_of_stories_featuring_mytheme <- mytheme_data[, 1]
  no_of_story_ids_of_stories_featuring_mytheme <- length(story_ids_of_stories_featuring_mytheme)

  if(no_of_story_ids_of_stories_featuring_mytheme > 0) {
    for(j in 1 : no_of_occurrences_of_mytheme) {
      story_id_of_story_featuring_mytheme <- story_ids_of_stories_featuring_mytheme[j]
      level_of_mytheme_in_story_featuring_mytheme <- mytheme_data[j, 2]
      story_data[[story_id_of_story_featuring_mytheme]]$CentralAndPeripheralThemes <- append(story_data[[story_id_of_story_featuring_mytheme]]$CentralAndPeripheralThemes, mytheme)

      if(level_of_mytheme_in_story_featuring_mytheme == "choice" || level_of_mytheme_in_story_featuring_mytheme == "major") {
        story_data[[story_id_of_story_featuring_mytheme]]$CentralThemes <- append(story_data[[story_id_of_story_featuring_mytheme]]$CentralThemes, mytheme)
      } else if(level_of_mytheme_in_story_featuring_mytheme == "minor") {
        story_data[[story_id_of_story_featuring_mytheme]]$PeripheralThemes <- append(story_data[[story_id_of_story_featuring_mytheme]]$PeripheralThemes, mytheme)
      }
    }
  }
}
for(i in 1 : no_of_metathemes) {  
  mytheme <- metathemes[i]
  mytheme_data <- story_themedata[[2]][mytheme][[1]]
  story_ids_of_stories_featuring_mytheme <- mytheme_data[, 1]
  no_of_story_ids_of_stories_featuring_mytheme <- length(story_ids_of_stories_featuring_mytheme)

  if(no_of_story_ids_of_stories_featuring_mytheme > 0) {
    for(j in 1 : no_of_story_ids_of_stories_featuring_mytheme) {
      story_id_of_story_featuring_mytheme <- story_ids_of_stories_featuring_mytheme[j]
      level_of_mytheme_in_story_featuring_mytheme <- mytheme_data[j, 2]
      story_data[[story_id_of_story_featuring_mytheme]]$CentralAndPeripheralThemes <- append(story_data[[story_id_of_story_featuring_mytheme]]$CentralAndPeripheralThemes, mytheme)

      if(level_of_mytheme_in_story_featuring_mytheme == "choice" || level_of_mytheme_in_story_featuring_mytheme == "major") {
        story_data[[story_id_of_story_featuring_mytheme]]$CentralThemes <- append(story_data[[story_id_of_story_featuring_mytheme]]$CentralThemes, mytheme)
      } else if(level_of_mytheme_in_story_featuring_mytheme == "minor") {
        story_data[[story_id_of_story_featuring_mytheme]]$PeripheralThemes <- append(story_data[[story_id_of_story_featuring_mytheme]]$PeripheralThemes, mytheme)
      }
    }
  }
}
for(i in 1 : no_of_stories) {
  story_data[[i]]$CentralAndPeripheralThemes <- unique(story_data[[i]]$CentralAndPeripheralThemes)
  story_data[[i]]$CentralThemes <- unique(story_data[[i]]$CentralThemes)
  story_data[[i]]$PeripheralThemes <- unique(story_data[[i]]$PeripheralThemes)
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

## shiny ui
ui <- fluidPage(
  useShinyjs(),
  headerPanel('Theme enrichment analyzer'),
  sidebarPanel(
    div(
      id = "control-panel",
      fileInput(
        inputId = 'test_storyset_file',
        label = 'Test storyset (SMT file format):',
        accept = c("text/smt", ".smt")
      ),
      fileInput(
        inputId = 'background_storyset_file',
        label = 'Background storyset (SMT file format):',
        accept = c("text/smt", ".smt")
      ),
      radioButtons(
        inputId = 'theme_level',
        label = 'Theme levels:',
        choices = list("Central and Peripheral" = "CentralAndPeripheralThemes", "Central" = "CentralThemes", "Peripheral" = "PeripheralThemes"), 
        selected = "CentralAndPeripheralThemes"
      ),
      sliderInput(
        inputId = 'significance_level',
        label = 'Significance level:',
        min = 0,
        max = 1,
        value = 0.05,
        step = 0.05
      ),
      numericInput(
        inputId = 'min_theme_occurrence',
        label = 'Minimum theme occurrence:',
        value = 1,
        min = 1,
        max = 10
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
  conditionalPanel(condition = 'is.null(input.test_storyset_file) && is.null(input.background_storyset_file)', mainPanel(h4("Upload a test and background storysets to get a ranked list of enriched themes"))),
  conditionalPanel(
    condition = 'is.null(input.test_storyset_file) && is.null(input.background_storyset_file)',
    mainPanel(
      DT::dataTableOutput(outputId = 'result_table')
    )
  )
)

# shiny server
server <- function(input, output, session) {
  data <- reactive({
    ## process test storyset
    if(is.null(input$test_storyset_file)) {
      test_story_ids <- character()
      no_of_test_stories <- length(test_story_ids)
    } else {
      inlines <- readLines(input$test_storyset_file$datapath)
      no_of_storysets <- length(inlines)
      background_story_ids <- NULL
      for (i in 1 : no_of_storysets) {
        inline <- inlines[i]
        inline_tokens <- unlist(strsplit(inline, split = "\t"))
        test_story_ids <- c(test_story_ids, inline_tokens[3 : length(inline_tokens)])
      }
      test_story_ids <- intersect(test_story_ids, story_ids) 
      no_of_test_stories <- length(test_story_ids)
      invalid_test_story_ids <- setdiff(test_story_ids, story_ids)
      no_of_invalid_test_story_ids <- length(invalid_test_story_ids)
      if(no_of_invalid_test_story_ids > 0) {
        alert(paste("Invalid story IDs identified in test storyset:", invalid_test_story_ids))
      }
    }

    ## process background storyset
    if(is.null(input$background_storyset_file)) {
      background_story_ids <- story_ids
      no_of_background_stories <- no_of_stories
    } else {
      inlines <- readLines(input$background_storyset_file$datapath)
      no_of_storysets <- length(inlines)
      background_story_ids <- NULL
      for (i in 1 : no_of_storysets) {
        inline <- inlines[i]
        inline_tokens <- unlist(strsplit(inline, split = "\t"))
        background_story_ids <- c(background_story_ids, inline_tokens[3 : length(inline_tokens)])
      }
      background_story_ids <- intersect(background_story_ids, story_ids)
      no_of_background_stories <- length(background_story_ids)
      invalid_background_story_ids <- setdiff(background_story_ids, story_ids)
      no_of_invalid_background_story_ids <- length(invalid_background_story_ids)
      if(no_of_invalid_background_story_ids > 0) {
        alert(paste("Invalid background story IDs identified:", invalid_background_story_ids))
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
    }

    ## construct background theme frequency table
    if(input$theme_level == "CentralAndPeripheralThemes") {
      bag_of_central_and_peripheral_themes <- unname(unlist(subListExtract(L = story_data, name = "CentralAndPeripheralThemes", keep.names = TRUE)[background_story_ids]))
      mytable <- table(bag_of_central_and_peripheral_themes)
      mythemes <- names(mytable)
      background_df <- data.frame(
        Theme = mythemes,
        Frequency = as.numeric(mytable),
        stringsAsFactors = FALSE
      )
      rownames(background_df) <- mythemes
    } else if(input$theme_level == "CentralThemes") {
      bag_of_central_themes <- unname(unlist(subListExtract(L = story_data, name = "CentralThemes", keep.names = TRUE)[background_story_ids]))
      mytable <- table(bag_of_central_themes)
      mythemes <- names(mytable)
      background_df <- data.frame(
        Theme = mythemes,
        Frequency = as.numeric(mytable),
        stringsAsFactors = FALSE
      )
      rownames(background_df) <- mythemes
    } else if(input$theme_level == "PeripheralThemes") {
      bag_of_peripheral_themes <- unname(unlist(subListExtract(L = story_data, name = "PeripheralThemes", keep.names = TRUE)[background_story_ids]))
      mytable <- table(bag_of_peripheral_themes)
      mythemes <- names(mytable)
      background_df <- data.frame(
        Theme = mythemes,
        Frequency = as.numeric(mytable),
        stringsAsFactors = FALSE
      )
      rownames(background_df) <- mythemes
    }

    ## construct test theme frequency table
    if(input$theme_level == "CentralAndPeripheralThemes") {
      bag_of_central_and_peripheral_themes <- unname(unlist(subListExtract(L = story_data, name = "CentralAndPeripheralThemes", keep.names = TRUE)[test_story_ids]))
      mytable <- table(bag_of_central_and_peripheral_themes)
      mythemes <- names(mytable)
      test_df <- data.frame(
        Theme = mythemes,
        Frequency = as.numeric(mytable),
        stringsAsFactors = FALSE
      )
      rownames(test_df) <- mythemes
    } else if(input$theme_level == "CentralThemes") {
      bag_of_central_themes <- unname(unlist(subListExtract(L = story_data, name = "CentralThemes", keep.names = TRUE)[test_story_ids]))
      mytable <- table(bag_of_central_themes)
      mythemes <- names(mytable)
      test_df <- data.frame(
        Theme = mythemes,
        Frequency = as.numeric(mytable),
        stringsAsFactors = FALSE
      )
      rownames(test_df) <- mythemes
    } else if(input$theme_level == "PeripheralThemes") {
      bag_of_peripheral_themes <- unname(unlist(subListExtract(L = story_data, name = "PeripheralThemes", keep.names = TRUE)[test_story_ids]))
      mytable <- table(bag_of_peripheral_themes)
      mythemes <- names(mytable)
      test_df <- data.frame(
        Theme = mythemes,
        Frequency = as.numeric(mytable),
        stringsAsFactors = FALSE
      )
      rownames(test_df) <- mythemes
    }
    test_themes <- setdiff(rownames(test_df), unlist(strsplit(input$blacklisted_themes, split = '\n')))
    no_of_test_themes <- length(test_themes)

    ## initialize output data frame
    result <- data.frame(
      Theme = test_themes,
      k = numeric(no_of_test_themes),
      n = numeric(no_of_test_themes),
      K = numeric(no_of_test_themes),
      N = numeric(no_of_test_themes),
      P = numeric(no_of_test_themes),
      stringsAsFactors = FALSE
    )
    
    ## hypergeometric test for theme over-representation
    n = no_of_test_stories
    N = no_of_background_stories

    for(i in 1 : no_of_test_themes) {
      mytheme <- test_themes[i]
      k <- test_df[mytheme, "Frequency"]
      K <- background_df[mytheme, "Frequency"]
      P <- phyper(q = k - 1, m = K, n = (N - K), k = n, lower.tail = FALSE)
      result[i, "k"] <- k
      result[i, "n"] <- n
      result[i, "K"] <- K
      result[i, "N"] <- N
      result[i, "P"] <- P
    }

    keeper_rows <- union(which(result[, "k"] >= input$min_theme_occurrence), which(result[, "k"] <= input$significance_level))
    no_of_keeper_rows <- length(keeper_rows)
    if(no_of_keeper_rows >= 1) {
      result <- result[keeper_rows, ]
    }
    result <- result[with(result, order(P, decreasing = FALSE)), ]
    return(result)
  })
  
  # generate table of similar stories
  output$result_table <- DT::renderDataTable(DT::datatable(
    data(),
    colnames = c('Theme', 'k', 'n', 'K', 'N', 'P'), 
    options = list(pageLength = 20),
    rownames = FALSE
    ) %>% formatRound(columns=c('P'), digits = 5)
  )
  
  ## Reset all inputs to default values
  observeEvent(input$resetAll, {
    reset("control-panel")
  })
}

shinyApp(ui = ui, server = server)
