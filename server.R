library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinyjs)
library(spotifyr)

shinyServer(function(input, output) {
    
    # uncomment and populate following 2 variables with your Spotify API credentials
    # Sys.setenv(SPOTIFY_CLIENT_ID = '')
    # Sys.setenv(SPOTIFY_CLIENT_SECRET = '')
    
    clientID = Sys.getenv("SPOTIFY_CLIENT_ID")
    secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")
    
    response = POST(
        'https://accounts.spotify.com/api/token',
        accept_json(),
        authenticate(clientID, secret),
        body = list(grant_type = 'client_credentials'),
        encode = 'form',
        verbose()
    )
    mytoken = content(response)$access_token
    HeaderValue = paste0('Bearer ', mytoken)
    
    useShinyjs(html = TRUE)
    
    # getting English weekdays' names
    Sys.setlocale("LC_TIME", "C")
    
    #---------------------------------------------------------------------------
    # Data loading and transformation
    #---------------------------------------------------------------------------
    
    original_data <- reactive({
        req(input$input_file)
        path <- input$input_file
        path <- path$datapath
        original_data <- fromJSON(path, simplifyVector = TRUE)
        return(original_data)
    })
    
    data <- reactive({
        
        data <- original_data()
        
        # including only tracks which were played longer than a specific time
        min_allowed_duration = 0.5; # in minutes
        data <- data[ which(data$msPlayed/60000>min_allowed_duration), ]
        
        # preparing columns
        data$endTime <- as.POSIXct(data$endTime, format = "%Y-%m-%d %H:%M")
        data$minPlayed <- round(data$msPlayed/60000, 2)
        data$year <- substr(data$endTime, 1, 4)
        data$month <- substr(data$endTime, 6, 7)
        data$day <- substr(data$endTime, 9, 10)
        data$hour <- substr(data$endTime, 12, 13)
        data$min <- substr(data$endTime, 15, 16)
        data$weekday <- weekdays(as.POSIXct(data$endTime, format = "%Y-%m-%d %H:%M"))
        data$artistName <- data$artistName
        data$trackName <- data$trackName
        
        # limiting date range of the report
        data <- data[which(as.POSIXct(substr(data$endTime,1,10), format = "%Y-%m-%d") >=
                                     as.POSIXct(substr(input$report_start_date,1,10), format = "%Y-%m-%d")), ]
        data <- data[which(as.POSIXct(substr(data$endTime,1,10), format = "%Y-%m-%d") <=
                                     as.POSIXct(substr(input$report_end_date,1,10), format = "%Y-%m-%d")), ] 

        return(data)
    })
    
    #---------------------------------------------------------------------------
    # Functions
    #---------------------------------------------------------------------------
    
    get_dataset_start_date <- function()
    {
        return(substr(min(original_data()$endTime), 1,10))
    }
    
    get_dataset_end_date <- function()
    {
        return(substr(max(original_data()$endTime), 1,10))
    }
    
    get_top_genres <- function(num_of_top_artists, num_of_genres)
    {
        top_artists_names <- head(names(sort(table(data()$artistName), decreasing=TRUE)), num_of_top_artists)
        
        top_artists_ids <- character()
        top_genres <- character()
        for (i in 1:length(top_artists_names))
        {
            artist_name <- top_artists_names[i]
            artist_data <- search_spotify(artist_name, type="artist")
            artist_data <- head(artist_data[artist_data["name"]==artist_name,], 1)
            top_genres <- c(top_genres,artist_data$genres)
        }
        top_genres <- as.data.frame(table(unlist(top_genres)))
        top_genres <- head(top_genres[order(-top_genres$Freq),], num_of_genres)
        return(top_genres)
    }
    
    total_listenings <- function()
    {
        return(length(data()$endTime))
    }
    
    total_artists <- function()
    {
        return( length(table(data()$artistName)) )
    }
    
    total_tracks <- function()
    {
        return( length(table(data()$trackName)) )
    }
    
    total_hours <- function()
    {
        return( round((sum(data()$minPlayed)/60),0) )
    } 
    
    get_top_artist_name <- function(place)
    {
        return( names(sort(table(data()$artistName), decreasing=TRUE)[place]) )
    }
    
    get_top_artist_image_url <- function(place)
    {
        # getting artist name
        artist_name <- names(sort(table(data()$artistName), decreasing=TRUE)[place])
        
        # getting artist_id
        artist_id <- search_spotify(artist_name, type="artist")
        artist_id <- head(artist_id[artist_id["name"]==artist_name,], 1)
        
        # getting an image url
        image_url <- artist_id$images[1]
        image_url <- as.data.frame(image_url)
        return(image_url$url[1])
    }
    
    get_top_artist_num_of_listenings <- function(place)
    {
        return(paste(sort(table(data()$artistName), decreasing=TRUE)[place], "listenings"))
    }
    
    get_top_track_num_of_listenings <- function(place)
    {
        return(paste(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE)[place],
                     "listenings"))
    }
    
    get_top_track_name <- function(place)
    {
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        return( sub(";.*", "", top_tracks[place]) )
    }
    
    get_top_track_artist_name <- function(place)
    {
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        return( sub(".*;", "", top_tracks[place]) )
    }
    
    longest_track_min_played <- function()
    {
        return( round(max(data()$minPlayed),0) )
    }
    
    longest_track_name <- function()
    {
        return(data()[data()$minPlayed == max(data()$minPlayed),"trackName"])
    }
    
    longest_track_artist <- function()
    {
        return(data()[data()$minPlayed == max(data()$minPlayed),"artistName"])
    }
    
    day_max_mins_played <- function()
    {
        temp <- data.frame( min_played = data()$minPlayed, day = as.factor(substr(data()$endTime,1,10)) )
        temp <- aggregate(temp$min_played, list(temp$day), sum)
        date <- as.Date(temp[temp$x == max(temp$x),1])
        date <- format(date,"%d %b %Y")
        return(date)
    }
    
    max_hours_played_per_day <- function()
    {
        temp <- data.frame( min_played = data()$minPlayed, day = as.factor(substr(data()$endTime,1,10)) )
        temp <- aggregate(temp$min_played, list(temp$day), sum)
        return( round( (temp[temp$x == max(temp$x),2]/60),1) )
    }
    
    top_artist_day_max_mins_played <- function()
    {
        temp <- data.frame( min_played = data()$minPlayed, day = as.factor(substr(data()$endTime,1,10)) )
        temp <- aggregate(temp$min_played, list(temp$day), sum)
        temp1 <- data()[ which( as.POSIXct(substr(data()$endTime,1,10), format = "%Y-%m-%d") == as.character( temp[temp$x == max(temp$x),1] ) ), ]
        return( names(sort(table(temp1$artistName), decreasing=TRUE)[1]) )
    }
    
    #---------------------------------------------------------------------------
    # Plots
    #---------------------------------------------------------------------------
    
    draw_plot_audio_features <- function(songs_and_artists)
    {
        audio_features <- data.frame("danceability"=double(), "energy"=double(),
                                     "loudness"=double(), "speechiness"=double(),
                                     "acousticness"=double(), "instrumentalness"=double(),
                                     "valence"=double(), "tempo"=double())
        for (i in 1:6)
        {
            artist_name <- songs_and_artists$artistName[i]
            song_name <- songs_and_artists$trackName[i]
            
            artist_data <- search_spotify(artist_name, type="artist")
            artist_data <- head(artist_data[artist_data$name==artist_name,], 1)
            # maybe i should use pipe here, to do: check it
            artist_audio_features <- get_artist_audio_features(artist_data$id)
            artist_audio_features <- artist_audio_features[artist_audio_features$track_name==song_name,]
            artist_audio_features <- head(artist_audio_features[ c("danceability", "energy", "acousticness",
                                                                   "instrumentalness", "valence", "tempo") ],1)
            audio_features <- rbind(audio_features, artist_audio_features)
        }
        
        audio_features <- as.data.frame(as.list(apply(audio_features, 2, mean)))
        audio_features <- audio_features[,c(1:5)]
        audio_features <- data.frame(features=colnames(audio_features), 
                                     values=unlist(audio_features[1,], use.names=FALSE))
        audio_features <- audio_features[order(-audio_features$values),]
        
        # ordering values on a plot
        audio_features$features <- factor(audio_features$features, levels=audio_features$features)
        
        audio_features_plot <- ggplot(data=audio_features, aes(x=features, y=values)) +
            geom_segment( aes(x=features ,xend=features, y=0, yend=values), color="grey") +
            geom_point(size=5, color="#7add00") +
            coord_flip() +
            theme_minimal() +
            theme(
                panel.grid.minor.y = element_blank(),
                panel.grid.major.y = element_blank(),
                legend.position="none"
            ) +
            ylab("average value") +
            xlab("")
        
        return(audio_features_plot)
    }
    
    draw_plot_top_genres <- function(plot_data)
    {
        sum_frequencies <- sum(plot_data$Freq)
        plot <- ggplot(plot_data, aes(x=Freq/sum_frequencies,
                                      y=reorder(Var1, Freq/sum_frequencies),
                                      label=Freq/sum_frequencies)) +
            geom_bar(aes(x=1,
                         y=reorder(Var1, Freq/sum_frequencies)),
                     stat="identity", fill="#f3f6f3", color="#f3f6f3", width = 0.7) +
            geom_bar(stat="identity", fill="#c0ff02", color="#5b7200", width = 0.7) +
            theme_minimal() +
            xlab("% of tracks labelled with a specific genre") +
            ylab("") +
            xlim(0,1) +
            geom_text(
                aes(label = paste0(round(Freq/sum_frequencies*100,0),"%"), x = 0.03),
                position = position_dodge(0.9),
                vjust = 0.3
            ) +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  legend.position="none") 
        
        return(plot)
    }
    
    output$plot_total_tracks_per_month <- renderPlot({
        dataframe <- data.frame(endTime = as.POSIXct(data()$endTime, format = "%Y-%m-%d %H:%M"), 
                                month = as.integer(data()$month))
        
        ggplot(dataframe, aes(x = month)) +
            geom_bar(fill="#aaaaaa", color="#555555") +
            theme_minimal() +
            theme(panel.grid.minor.x = element_blank()) +
            theme(panel.grid.major.x = element_blank()) +
            ylab("Total number of tracks") +
            xlab("") +
            scale_x_continuous(breaks = seq(1,12), labels = month.name) +
            theme(legend.position='none')
    }, bg="transparent")
    
    output$plot_total_tracks_per_weekday <- renderPlot({
        dataframe <- data.frame(endTime = data()$endTime, weekday = data()$weekday)
        
        max_value <- max(table(dataframe$weekday))
        
        dataframe$weekday <- factor(dataframe$weekday, 
                                    levels= rev(c("Monday", "Tuesday", "Wednesday",
                                                  "Thursday", "Friday", "Saturday", "Sunday")))
        ggplot(dataframe, aes(y=weekday)) +
            theme_minimal() +
            coord_cartesian(xlim = c(0, 1.1*max_value)) +
            theme(panel.grid.major.x = element_blank()) +
            theme(panel.grid.major.y = element_blank()) +
            theme(panel.grid.minor.x = element_blank()) +
            geom_bar(width = 0.4, fill="#CB3E85") +
            theme(axis.title.y = element_blank()) +
            xlab("Total number of tracks")
    }, bg="transparent")
    
    output$plot_audio_features <- renderPlot({
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        top_artists <- sub(".*;", "", top_tracks)
        top_tracks <- sub(";.*", "", top_tracks)
        songs_and_artists <- data.frame(trackName=top_tracks, artistName=top_artists, stringsAsFactors = FALSE)
        draw_plot_audio_features(songs_and_artists)
    }, bg="transparent")
    
    output$plot_top_genres <- renderPlot({
        num_of_input_tracks = 10
        num_of_output_genres = 10
        draw_plot_top_genres(plot_data=get_top_genres(num_of_input_tracks,num_of_output_genres))
    }, bg="transparent")
    
    output$draw_plot_hourly <- renderPlot({
        dataframe <- data.frame(endTime = as.POSIXct(data()$endTime, format = "%Y-%m-%d %H:%M"),
                                hour = substr(data()$endTime, 12,13))
        
        dataframe<- as.data.frame(table(dataframe$hour))
        dataframe$Var1 <- as.POSIXct(dataframe$Var1, format = "%H")
        
        smooth_line <- as.data.frame(spline(dataframe$Var1, dataframe$Freq))
        
        smooth_line$x <- as.POSIXct(smooth_line$x, origin = "1970-01-01 00:00:00")
        smooth_line <- as.data.frame(spline(smooth_line$x, smooth_line$y))
        smooth_line$x <- as.POSIXct(smooth_line$x, origin = "1970-01-01 00:00:00")
        head(smooth_line)
        
        hours <- as.POSIXct(paste0(c(0:23),":00:00"), format = "%H:%M")
        
        label_hours <- c(as.character(paste0(seq(0:22),":00")),"00:00")
        
        smooth_line$x <- as.integer(smooth_line$x)
        plot <- ggplot(smooth_line, aes(x = x, y = y)) +
            geom_line(colour="#dc9a1b", size = 2) +
            theme_minimal() +
            ylab("Total number of tracks") +
            xlab("") +
            theme(panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank()) + 
            scale_x_continuous(breaks = hours,labels =label_hours) +
            theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=1))
        return(plot)
    }, bg="transparent")
    
    #---------------------------------------------------------------------------
    # UI elements
    #---------------------------------------------------------------------------
    
    output$ui_header <- renderUI({
        fluidRow(class = "main_area",
            column(8, offset = 2,
                   br(),
                   p(class="title","explorify"),
                   h3("view your Spotify listening statistics without the need to login"),
                   br(),
                   p(
                     "You just need to upload a file (or files) named StreamingHistory*.json, which you can obtain from",tags$a(href="https://www.spotify.com/us/account/privacy/","Spotify website.")),
                   p("After upload, please wait a while. It may take up to one minute to load everything.")
                   ))
    })
    
    is_data_available <- reactive({
        if ( length(data()$endTime) > 0 ) TRUE else FALSE
    })
    
    output$ui_warning_no_data <- renderUI({
        req(input$input_file)
        if(is_data_available() == FALSE)
        {
            fluidRow(class = "main_area",
                     column(8, offset = 2,
                            tags$div(class="alert alert-warning", "There is no (enough) data within selected period.")
                     ))
        }
        
    })
    
    output$ui_date_range <- renderUI({
        req(input$input_file)
        fluidRow(class = "main_area",
                 column(8, offset = 2,
                        p("Date range in the uploaded file:",
                          paste(get_dataset_start_date(),"-",get_dataset_end_date())),
                        p("Select reporting range:"),
                        dateInput('report_start_date',
                                  label = 'From',
                                  value = get_dataset_start_date()
                        ),
                        dateInput('report_end_date',
                                  label = 'To',
                                  value = get_dataset_end_date()
                        )))
    })
    
    output$ui_top_artists <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "dashboard_area",
                     column(10, offset = 1,
                            h3("Top artists"),br(),br()),
                     column(2, offset = 2,
                            tags$img(src = get_top_artist_image_url(1), width = "100%", height = "100%"),
                            p(span(class = "marked",tags$span(class = "big","1 "),tags$b(get_top_artist_name(1))),br(),
                              get_top_artist_num_of_listenings(1))),
                     column(2, offset = 1,
                            tags$img(src = get_top_artist_image_url(2), width = "100%", height = "100%"),
                            p(span(class = "marked",tags$span(class = "big","2 "),tags$b(get_top_artist_name(2))),br(),
                              get_top_artist_num_of_listenings(2))),
                     column(2, offset = 1,
                            tags$img(src = get_top_artist_image_url(3), width = "100%", height = "100%"),
                            p(span(class = "marked",tags$span(class = "big","3 "),tags$b(get_top_artist_name(3))),br(),
                              get_top_artist_num_of_listenings(3),br())))
        }
    })
    
    output$ui_top_tracks <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "dashboard_area",
                     column(10, offset = 1,
                            h3("Top tracks"),br(),br()),
                     column(2, offset = 2, 
                            p(span(class = "marked",tags$span(class = "big","1 "),tags$b(get_top_track_name(1))),br(),
                              get_top_track_artist_name(1),br(),
                              get_top_track_num_of_listenings(1))),
                     column(2, offset = 1,
                            p(span(class = "marked",tags$span(class = "big","2 "),tags$b(get_top_track_name(2))),br(),
                              get_top_track_artist_name(2),br(),
                              get_top_track_num_of_listenings(2))),
                     column(2, offset = 1,
                            p(span(class = "marked",tags$span(class = "big","3 "),tags$b(get_top_track_name(3))),br(),
                              get_top_track_artist_name(3),br(),
                              get_top_track_num_of_listenings(3))))
        }
    })
    
    output$ui_summary <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "dashboard_area",
                     column(2, offset = 1,
                            br(),br(),
                            p(tags$span(class = "big",total_hours()),br(),"hours",br()),
                            p(tags$span(class = "big",total_listenings()),br(),"listenings",br()),
                            p(tags$span(class = "big",total_artists()),br(),"artists",br()),
                            p(tags$span(class = "big",total_tracks()),br(),"unique tracks")),
                     column(7, offset = 1,
                            plotOutput(
                                "plot_total_tracks_per_weekday",
                                width = "100%")))
        }
    })
    
    output$ui_hourly <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "dashboard_area",
                     column(10, offset = 1,
                            h3("How much you listened at each hour"),
                            plotOutput(
                                "draw_plot_hourly",
                                width = "100%"),
                            br()))
        }
    })
    
    output$ui_monthly <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "dashboard_area",
                     column(10, offset = 1,
                            h3("How much you listened in each month"),
                            plotOutput(
                                "plot_total_tracks_per_month",
                                width = "100%"),
                            br()))
        }
        
    })
    
    output$ui_genres <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "dashboard_area",
                     column(10, offset = 1,
                            h3("Genres of your most played songs"),
                            plotOutput(
                                "plot_top_genres",
                                width = "90%"),
                            br()))
        }
    })
    
    output$ui_audio_features <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "dashboard_area",
                     column(10, offset = 1,
                            br(),
                            h3("Audio features of your most played songs"),
                            #p(img(src = "images/audio_features_spider_plot.png", width = "100%")),
                            plotOutput(
                                "plot_audio_features",
                                width = "100%"),
                            br(),
                            p("A description of each feature you can find",
                              tags$a(href="https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/",
                                     "here"))))
        }
    })
    
    output$ui_quick_facts <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "dashboard_area",
                     column(10, offset = 1,
                            h3("Quick facts"),
                            p(
                                "Most active day:",
                                span(class="marked",tags$b(day_max_mins_played())),
                                paste0("(",max_hours_played_per_day()," hours)")),
                            p(
                                "Longest track:",
                                span(class="marked",tags$b(longest_track_min_played(), "minutes")),
                                paste0("(",longest_track_name()," - ",longest_track_artist(),")"))))
        }
    })
    
    output$ui_info <- renderUI({
        if(is_data_available())
        {
            fluidRow(class = "main_area",
                     column(12,
                            hr()),
                     column(10, offset = 1,
                            p(
                                "Note: Tracks that were played less than 0.5 min have been excluded from this report to avoid taking into account possibly accidentally played songs."))
            )
        }
    })
    
    output$ui_author <- renderUI({
        fluidRow(class = "main_area",
                 column(10, offset = 1,
                        p(class = "note", "Ewelina Luczak 2020")))
    })
    
})
