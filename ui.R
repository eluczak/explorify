library(shiny)

shinyUI(fluidPage(
    fluidRow(
        column(6, offset = 3,
               h1(tags$b("my Spotify stats")),
               h3("A visualization of your listening history on Spotify"),
               br(),
               p(tags$b("Instructions:")),
               tags$ol(
                   tags$li("You'll need to request your last year's listening history from Spotify website (you find this option in the privacy settings). Usually after 2 or 3 days the data is available to download and you will receive a set of files containing various information that has been collected by Spotify. The file (or files) that we need contains only listening records and is named",tags$i("StreamingHistory*.json"), ", for example StreamingHistory0.json."),
                   tags$li("Upload the file(s) below and enjoy!")
               ),
               p(tags$b("Note:"),"After upload please wait a while. It may take up to one minute to generate the report."),
               pre(br(), "upload or drag file(s) here", br()),
               br(),
               br())
    ),
    fluidRow(
        column(3, offset = 3,   
               selectInput("scope", "Scope of report:", c("all", "weekends", "weekdays"))
        ),
        column(3,
               dateRangeInput('dateRange',
                              label = 'Date range:',
                              start = Sys.Date() - 2, end = Sys.Date() + 2)),
    ),
    
    fluidRow(
        column(6, offset = 3,
               br(),
               p(tags$b("28 Dec 2019 - 05 May 2020"), align = "center"))
    ),
    
    fluidRow(
        column(6, offset = 3,
               p("Total listening time:",
                 tags$b("3459"),"hours"))
    ),
    
    fluidRow(
        column(6, offset = 3,
               p(tags$b("765"),"listenings"))
    ),
    
    fluidRow(
        column(6, offset = 3,
               h3("Top artists"),
               hr())
    ),
    
    fluidRow(
        column(2, offset = 3,
               p(img(src = "images/artist1.png", width = "80%")),
               p(tags$b("Lana Del Rey")),
               p("345 listenings")),
        column(2,
               p(img(src = "images/artist2.png", width = "80%")),
               p(tags$b("ABBA")),
               p("101 listenings")),
        column(2,
               p(img(src = "images/artist3.png", width = "80%")),
               p(tags$b("Radiohead")),
               p("89 listenings"))
    ),
    
    fluidRow(
        column(6, offset = 3,
               h3("Top tracks"),
               hr())
    ),
    
    fluidRow(
        column(2, offset = 3,
               p(img(src = "images/album1.png", width = "80%")),
               p(tags$b("Piouiyutyr"),br(),"Liquido"),
               p("59 listenings")),
        column(2,
               p(img(src = "images/album2.png", width = "80%")),
               p(tags$b("Ipsum Lorem"),br(),"Pixies"),
               p("10 listenings")),
        column(2,
               p(img(src = "images/album3.png", width = "80%")),
               p(tags$b("Rfasde af oiuyg"),br(),"Liquido"),
               p("8 listenings"))
    ),
    
    fluidRow(
        column(6, offset = 3,
               h3("Total listening time by day of the week"),
               hr())
    ),
    
    fluidRow(
        column(6, offset = 3,
               p(img(src = "images/total_listening_time_by_weekday.png", width = "100%")))
    ),
    
    fluidRow(
        column(6, offset = 3,
               h3("Total listening time by hour"),
               hr())
    ),
    
    fluidRow(
        column(6, offset = 3,
               p(img(src = "images/total_listening_time_by_hour.png", width = "100%")))
    ),
    
    fluidRow(
        column(6, offset = 3,
               h3("Total listening time by month"),
               hr())
    ),
    
    fluidRow(
        column(6, offset = 3,
               p(img(src = "images/total_listening_time_by_hour.png", width = "100%")))
    ),
    
    fluidRow(
        column(6, offset = 3,
               h3("Genres of most played songs"),
               hr())
    ),
    
    fluidRow(
        column(6, offset = 3,
               p(img(src = "images/genres_plot.png", width = "100%")))
    ),
    
    fluidRow(
        column(6, offset = 3,
               h3("Audio features of most played songs"),
               hr())
    ),
    
    fluidRow(
        column(6, offset = 3,
               p(img(src = "images/audio_features_spider_plot.png", width = "100%")))
    ),
    
    fluidRow(
        column(6, offset = 3,
               hr())
    ),
    
    fluidRow(
        column(5, offset = 3,
               br(),
               p("On 27 Feb 2020 you had been listening for a longest time -",
                 tags$b("8.7"),"hours.",
                 br(),
                 "Mostly of", tags$b("Arcade Fire"), "songs."))
    ),
    
    fluidRow(
        column(5, offset = 5,
               br(),
               p("The longest track you listened to, was about",
                 tags$b("15 minutes"),"long.",
                 br(),
                 "It was", tags$b("Lucid Dreams"), "by Franz Ferdinand."))
    ),
    
    fluidRow(
        column(6, offset = 3,
               br(),
               br(),
               br(),
               p("Note: Tracks that were played less than 0.5 min have been excluded from this report to avoid taking into account possibly accidentally played songs."))
    ),
    
    fluidRow(
        column(6, offset = 3,
               hr(),
               p("Ewelina Luczak 2020"))
    )
    
    
    
    
))
