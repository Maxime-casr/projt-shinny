options(shiny.maxRequestSize = 100*1024^2)

library(shiny)
library(DT)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(tools)

data_clean <- function(df) {
  clean_text <- function(x) {
    x <- iconv(x, from = "latin1", to = "UTF-8", sub = "")
    x <- gsub("[ï¿½Ã¢Â Ã¢Â¡Â¡]+", "", x)
    x <- gsub("Ã.|Â.|â.|¢|œ|‚|„|‰|€|™", "", x)
    x <- gsub("[^\x20-\x7E]", "", x)
    return(x)
  }
  text_cols <- sapply(df, is.character)
  df[text_cols] <- lapply(df[text_cols], clean_text)
  df$year <- substr(df$year, 1, 4)
  df
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Shopper Sentiments Dashboard"),
  shinythemes::themeSelector(),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Importer un fichier CSV", accept = ".csv"),
      checkboxInput("na_rm", "Supprimer les lignes avec des valeurs manquantes", TRUE),
      sliderInput("note", "Filtrer par note :", min = 1, max = 5, value = c(1, 5)),
      uiOutput("country_ui"),
      checkboxInput("has_review", "Afficher uniquement les avis complets", TRUE),
      textInput("keyword", "Rechercher un mot dans les avis :", placeholder = "ex: refund"),
      selectInput("var_plot", "Variable à visualiser :", choices = c("review.label", "year", "store_location")),
      downloadButton("download_data", "Télécharger les données filtrées")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Données", DTOutput("table")),
        tabPanel("Graphique", plotOutput("barplot")),
        tabPanel("Carte", leafletOutput("map")),
        tabPanel("Résumé", verbatimTextOutput("summary")),
        tabPanel("Sentiment", plotOutput("sentiment_plot")),
        tabPanel("Exemples", DTOutput("examples"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data_user <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, fileEncoding = "latin1", stringsAsFactors = FALSE)
    df <- data_clean(df)
    if (input$na_rm) {
      df <- na.omit(df)
    }
    df
  })
  
  output$country_ui <- renderUI({
    req(data_user())
    selectInput("country", "Pays :", choices = c("Tous", unique(data_user()$store_location)))
  })
  
  data_filtered <- reactive({
    df <- data_user()
    
    df <- df %>%
      filter(review.label >= input$note[1], review.label <= input$note[2])
    
    if (input$country != "Tous") {
      df <- df %>% filter(store_location == input$country)
    }
    
    if (input$has_review) {
      df <- df %>% filter(!is.na(review) & review != "")
    }
    
    if (input$keyword != "") {
      df <- df %>% filter(grepl(input$keyword, review, ignore.case = TRUE))
    }
    
    df <- df %>%
      mutate(sentiment = case_when(
        review.label >= 4 ~ "Positif",
        review.label == 3 ~ "Neutre",
        review.label <= 2 ~ "Négatif",
        TRUE ~ NA_character_
      ))
    
    df
  })
  
  output$table <- renderDT({
    datatable(data_filtered(), options = list(pageLength = 10))
  })
  
  output$barplot <- renderPlot({
    ggplot(data_filtered(), aes_string(x = input$var_plot)) +
      geom_bar(fill = "steelblue") +
      labs(title = paste("Distribution de", input$var_plot), x = input$var_plot, y = "Fréquence")
  })
  
  output$map <- renderLeaflet({
    df <- data_filtered() %>% drop_na(latitude, longitude)
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0("<strong>", title, "</strong><br/>", review),
        radius = 4,
        fillOpacity = 0.6
      )
  })
  
  output$summary <- renderPrint({
    summary(data_filtered()$review.label)
  })
  
  output$sentiment_plot <- renderPlot({
    ggplot(data_filtered(), aes(x = sentiment, fill = sentiment)) +
      geom_bar() +
      labs(title = "Analyse de sentiment des avis",
           x = "Catégorie de sentiment",
           y = "Nombre d'avis") +
      scale_fill_manual(values = c("Positif" = "forestgreen", 
                                   "Neutre" = "gold", 
                                   "Négatif" = "firebrick")) +
      theme_minimal()
  })
  
  output$examples <- renderDT({
    data_filtered() %>%
      group_by(sentiment) %>%
      slice_head(n = 3) %>%
      ungroup() %>%
      select(sentiment, review.label, title, review)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("avis_filtrés_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_filtered(), file, row.names = FALSE, fileEncoding = "UTF-8-BOM")
    }
  )
}

shinyApp(ui, server)
