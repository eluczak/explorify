library(shiny)

shinyUI(fluidPage(
    includeCSS("styles.css"),
    
    uiOutput("ui_header"),
    uiOutput("ui_file_upload"),
    uiOutput("ui_date_range"),
    uiOutput("ui_warning_no_data"),
    uiOutput("ui_top_artists"),
    uiOutput("ui_top_tracks"),
    uiOutput("ui_summary"),
    uiOutput("ui_hourly"),
    uiOutput("ui_monthly"),
    uiOutput("ui_genres"),
    # uiOutput("ui_audio_features"),
    uiOutput("ui_quick_facts"),
    uiOutput("ui_info"),
    uiOutput("ui_author")
    
))
