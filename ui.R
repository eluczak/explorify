library(shiny)

shinyUI(fluidPage(
    includeCSS("styles.css"),
    
    uiOutput("ui_header"),
    
    fluidRow(
        column(8, offset = 4, class = "dummy",
               fileInput(
                   "input_file",
                   "",
                   multiple = TRUE,
                   accept = ".json",
                   buttonLabel = "Browse or drag file(s) here",
                   placeholder = "No file selected",
                   width = "50%"),
               br())),
    
    #uiOutput("ui_date_range"),
    uiOutput("ui_report_ready"),
    uiOutput("ui_top_artists"),
    uiOutput("ui_top_tracks"),
    uiOutput("ui_summary"),
    uiOutput("ui_hourly"),
    uiOutput("ui_monthly"),
    #uiOutput("ui_genres"),
    #uiOutput("ui_audio_features"),
    uiOutput("ui_quick_facts"),
    uiOutput("ui_info"),
    uiOutput("ui_author")
    
))
