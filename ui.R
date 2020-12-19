library(shiny)

shinyUI(fluidPage(
includeCSS("styles.css"),
 
fluidRow(
    column(12,
           column(10, offset = 1,
                  br(),
                  p(class="title","musiclife"),
                  p(class="subtitle","A visualization of your listening history in Spotify",br(),"without the need to log in."),
                  br()))),


   
fluidRow(
column(10, offset = 1, class = "main",
       
    fluidRow(
        column(12,
               fluidRow(
                   column(10, offset = 0,
                          h3("Instructions:"),
                          p("1. You'll need to request your last year's listening history from",tags$a(href="https://www.spotify.com/us/account/privacy/","Spotify website."),"Usually after 2 or 3 days the data is available to download and you will receive a set of files containing various information that has been collected by Spotify. The file (or files) that we need contains only listening records and is named",tags$i("StreamingHistory*.json"), ", for example StreamingHistory0.json."),
                          p("2. Upload the file(s) below and enjoy!"),
                          br(),
                          br())))
        ),

    fluidRow(
        column(12, class = "",
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
                       br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())))),

    fluidRow(
        column(12, class = "",
               fluidRow(
                   column(10, offset = 1, class = "dummy",
                          selectInput("date_range", "Date range",
                                      c("All time" = "all_time",
                                        "Last week" = "last_week",
                                        "Last month" = "last_month",
                                        "Last year" = "last_year")))))),

    fluidRow(
        column(12, class = "dashboard_item",
               fluidRow(

                   column(8, offset = 2,
                          h3("Top artists"),
                          hr()),

                   column(2, offset = 2, class = "dummy",
                          p(img(src = "images/artist1.png", width = "80%")),
                          p(tags$b(textOutput("top_artist_1", inline = TRUE))),
                          p(textOutput("num_of_listenings_top_artist_1", inline = TRUE))),

                   column(2, offset = 1, class = "dummy",
                          p(img(src = "images/artist2.png", width = "80%")),
                          p(tags$b(textOutput("top_artist_2", inline = TRUE))),
                          p(textOutput("num_of_listenings_top_artist_2", inline = TRUE))),

                   column(2, offset = 1, class = "dummy",
                          p(img(src = "images/artist3.png", width = "80%")),
                          p(tags$b(textOutput("top_artist_3", inline = TRUE))),
                          p(textOutput("num_of_listenings_top_artist_3", inline = TRUE)))

                   ))),

    fluidRow(
        column(12, class = "dashboard_item",
               fluidRow(

                   column(8, offset = 2,
                          h3("Top tracks"),
                          hr()),

                   column(2, offset = 2, class = "dummy",
                          p(img(src = "images/album1.png", width = "80%")),
                          p(tags$b(textOutput("top_track_title_1", inline = TRUE)),
                            br(),
                            textOutput("top_track_artist_1", inline = TRUE)),
                          p(textOutput("num_of_listenings_top_track_1", inline = TRUE))),

                   column(2, offset = 1, class = "dummy",
                          p(img(src = "images/album2.png", width = "80%")),
                          p(tags$b(textOutput("top_track_title_2", inline = TRUE)),
                            br(),
                            textOutput("top_track_artist_2", inline = TRUE)),
                          p(textOutput("num_of_listenings_top_track_2", inline = TRUE))),

                   column(2, offset = 1, class = "dummy",
                          p(img(src = "images/album3.png", width = "80%")),
                          p(tags$b(textOutput("top_track_title_3", inline = TRUE)),
                            br(),
                            textOutput("top_track_artist_3", inline = TRUE)),
                          p(textOutput("num_of_listenings_top_track_3", inline = TRUE)))

                   ))),

    fluidRow(
        column(12, class = "dashboard_item dummy",
               column(2, offset = 1,
                      br(),br(),
                      p(tags$span(class = "big",textOutput("total_hours_played", inline = TRUE)),"hours"),
                      p(tags$span(class = "big",textOutput("num_of_listenings", inline = TRUE)),"listenings"),
                      p(tags$span(class = "big",textOutput("num_of_artists", inline = TRUE)),"artists"),
                      p(tags$span(class = "big",textOutput("num_of_tracks", inline = TRUE)),"tracks")),
               column(7, offset = 1,
                      plotOutput(
                          "plot_total_tracks_per_weekday",
                          width = "100%")))
    ),

    fluidRow(
        column(12, class = "dashboard_item dummy",
               column(10, offset = 1,
                      h3("How much you listened at each hour"),
                      hr(),
                      plotOutput(
                          "plot_total_tracks_per_hour",
                          width = "100%"),
                      br(),
                      selectInput("hour_plot_weekday", "day of the week", c("all", "weekdays", "weekends", "---", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))))
    ),

    fluidRow(
        column(12, class = "dashboard_item dummy",
               column(10, offset = 1,
                      h3("How much you listened in each month"),
                      hr(),
                      p("y - average listening time in hours"),
                      p(img(src = "images/total_listening_time_by_hour.png", width = "100%")),
                      br(),
                      selectInput("month_plot_year", "year", c("all", "2020", "2019", "2018"))))
    ),

    fluidRow(
        column(12, class = "dashboard_item dummy",
               column(10, offset = 1,
                      h3("Genres of your most played songs"),
                      hr(),
                      p(img(src = "images/genres_plot.png", width = "100%")),
                      sliderInput("genres_plot_num_of_songs",
                                  "Number of most played songs:",
                                  value = 5,
                                  min = 1,
                                  max = 10)))
    ),


    fluidRow(
        column(12, class = "dashboard_item dummy",
               column(10, offset = 1,
                      br(),
                      h3("Audio features of your most played songs"),
                      hr(),
                      #p(img(src = "images/audio_features_spider_plot.png", width = "100%")),
                      plotOutput(
                          "plot_audio_features",
                          width = "100%"),
                      sliderInput("audio_features_plot_num_of_songs",
                                  "Number of most played songs:",
                                  value = 5,
                                  min = 1,
                                  max = 10)))
    ),

    fluidRow(
        column(12, class = "dashboard_item dummy",
        column(8, offset = 2, class = "dummy",
               h3("Quick facts"),
               hr(),
               p(textOutput("day_max_mins_played", inline=TRUE),
                 tags$b(textOutput("max_hours_played_per_day", inline=TRUE)),
                 "Mostly of", tags$b(textOutput("top_artist_day_max_mins_played", inline=TRUE)),"songs."),
               br(),
               p("The longest track you listened to, was about",
                 tags$b(textOutput("longest_track_min_played", inline=TRUE)),"long.",
                 "It was", tags$b(textOutput("longest_track_name", inline=TRUE)), "by",textOutput("longest_track_artist", inline=TRUE))))
    ),


    fluidRow(
        column(8, offset = 2, class = "dummy",
               br(),
               br(),
               br(),
               p("Note: Tracks that were played less than 0.5 min have been excluded from this report to avoid taking into account possibly accidentally played songs."))
    ),
    
    fluidRow(
        column(3, offset = 2,
               hr(),
               p("Ewelina Luczak 2020"))
    )
    
))
    
))
