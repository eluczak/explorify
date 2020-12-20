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
    
    # line below is used for getting English weekdays' names
    Sys.setlocale("LC_TIME", "C")

    
    # reading the data
    original_data <- reactive({
        req(input$input_file)
        path <- input$input_file
        path <- path$datapath
        original_data <- fromJSON(path, simplifyVector = TRUE)
        return(original_data)
    })
    
    # data transformation
    data <- reactive({
        
        data <- original_data()
        
        # including only tracks which were played longer than a specific time
        min_allowed_duration = 0.5; # in minutes
        data <- data[ which(data$msPlayed/60000>min_allowed_duration), ]
        
        # preparing columns
        data$minPlayed <- round(data$msPlayed/60000, 2)
        data$year <- substr(data$endTime, 1, 4)
        data$month <- substr(data$endTime, 6, 7)
        data$day <- substr(data$endTime, 9, 10)
        data$hour <- substr(data$endTime, 12, 13)
        data$min <- substr(data$endTime, 15, 16)
        data$weekday <- weekdays(as.POSIXct(data$endTime, format = "%Y-%m-%d %H:%M"))
        data$artistName <- data$artistName
        data$trackName <- data$trackName
        
        return(data)
    })
        
    get_top_genres <- function(num_of_top_artists, num_of_genres)
    {
        # getting top artists' names
        top_artists_names <- character()
        for(i in 1:num_of_top_artists) {
            top_artists_names <- c(top_artists_names, names(sort(table(data()$artistName), decreasing=TRUE)[i]))
        }
        
        # getting top artists' IDs
        top_artists_ids <- character()
        for (i in 1:length(top_artists_names))
        {
            artist_name <- top_artists_names[i]
            artist_data <- search_spotify(artist_name, type="artist")
            artist_data <- head(artist_data[artist_data["name"]==artist_name,], 1)
            top_artists_ids <- c(top_artists_ids, artist_data$id)
        }
        
        # getting top artists' genres
        top_genres <- character()
        for (i in 1:length(top_artists_ids))
        {
            URI <- paste0('https://api.spotify.com/v1/artists/', top_artists_ids[i])
            response2 <- GET(url=URI, add_headers(Authorization = HeaderValue))
            response2 <- content(response2)
            top_genres <- c(top_genres, data.frame(genres=unlist(response2["genres"])))
        }
        # vector with top genres sorted from most common ones
        top_genres <- table(as.data.frame(unlist(top_genres)))
        top_genres <- as.data.frame(top_genres)
        top_genres <- head(top_genres[order(-top_genres$Freq),], num_of_genres)
        return(top_genres)
    }
    
    output$top_genres <- renderTable({
        print(get_top_genres(10, 6))
    })
    
    
    get_artist_image <- function(artist_id) 
    {
        response <- GET(url=paste0("https://api.spotify.com/v1/artists/","12Chz98pHFMPJEknJQMWvI"), add_headers(Authorization = HeaderValue))
        response <- content(response)
        image_url <- data.frame(response["images"])[1,2]
        return(image_url)
    }

    draw_plot_audio_features <- function(songs_and_artists)
    {
        audio_features <- data.frame("danceability"=double(), "energy"=double(),"loudness"=double(),
                                     "speechiness"=double(), "acousticness"=double(),
                                     "instrumentalness"=double(), "valence"=double(), "tempo"=double())
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
        audio_features <- data.frame(features=colnames(audio_features), values=unlist(audio_features[1,], use.names=FALSE))
        audio_features <- audio_features[order(-audio_features$values),]
        
        # ordering values on a plot
        audio_features$features <- factor(audio_features$features, levels=audio_features$features)
        
        # audio_features_plot <- ggplot(data=audio_features, aes(x=features, y=values) ) +
        #     geom_line() +
        #     ylim(0,1) +
        #     coord_polar() +
        #     xlab("average value") +
        #     ylab("") +
        #     theme_minimal() +
        #     theme(panel.grid.minor.x = element_blank()) +
        #     theme(panel.grid.major.x = element_blank()) +
        #     theme(plot.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7"))
        
        audio_features_plot <- ggplot(data=audio_features, aes(x=features, y=values)) +
            geom_segment( aes(x=features ,xend=features, y=0, yend=values), color="grey") +
            geom_point(size=5, color="#842bd7") +
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
        plot <- ggplot(plot_data, aes(x=Freq, y=reorder(Var1, Freq), fill=Freq, label=Freq)) +
            geom_bar(stat="identity") +
            theme_minimal() +
            xlab("number of tracks labelled with a specific genre") +
            ylab("") +
            #geom_text(aes(x = 0), angle = 0, hjust = 2, size = 3) +
            geom_text(
                aes(label = Freq, x = 0.5),
                position = position_dodge(0.9),
                vjust = 0.3
            ) +
            scale_fill_gradient(low = "pink", high = "orangered")+
            theme(axis.text.x = element_blank(), 
                  axis.ticks.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  legend.position="none")
        
        return(plot)
    }
    
    output$plot_top_genres <- renderPlot({
        draw_plot_top_genres(plot_data=get_top_genres(30,15))
    })
    
    output$num_of_listenings <- renderPrint({
        cat(length(data()$endTime))
    })
    
    output$num_of_artists <- renderPrint({
        cat( length(table(data()$artistName)) )
    })
    
    output$num_of_tracks <- renderPrint({
        cat( length(table(data()$trackName)) )
    })
    
    output$start_date <- renderPrint({
        cat( substr(min(data()$endTime),1,10) )
    })
    
    output$end_date <- renderPrint({
        cat( substr(max(data()$endTime),1,10) )
    })
    
    output$total_hours_played <- renderPrint({
        cat( round((sum(data()$minPlayed)/60),0) )
    }) 
    
    output$top_artist_1 <- renderPrint({
        cat( names(sort(table(data()$artistName), decreasing=TRUE)[1]) )
    })
    
    output$top_artist_2 <- renderPrint({
        cat( names(sort(table(data()$artistName), decreasing=TRUE)[2]) )
    })
    
    output$top_artist_3 <- renderPrint({
        cat( names(sort(table(data()$artistName), decreasing=TRUE)[3]) )
    })
    
    top_artist_image_url <- function(place)
        # argument place means the place in ranking of most listened artists
        # 1 is the most listened, 2 is the second etc.
    {
        # getting artist name
        artist_name <- names(sort(table(data()$artistName), decreasing=TRUE)[place])
        
        # getting artist_id
        artist_id <- search_spotify(artist_name, type="artist")
        artist_id <- head(artist_id[artist_id["name"]==artist_name,], 1)
        
        # getting an image
        image_url <- artist_id$images[1]
        image_url <- as.data.frame(image_url)
        return(image_url$url[1])
    }
        
    
    output$top_artist_1_image <- renderUI({
        tags$img(src = top_artist_image_url(place = 1), width = "80%", height = "80%")
    })
    
    output$top_artist_2_image <- renderUI({
        tags$img(src = top_artist_image_url(place = 2), width = "80%", height = "80%")
    })
    
    output$top_artist_3_image <- renderUI({
        tags$img(src = top_artist_image_url(place = 3), width = "80%", height = "80%")
    })
    
    
    output$num_of_listenings_top_artist_1 <- renderPrint({
        cat(paste(sort(table(data()$artistName), decreasing=TRUE)[1] ), "listenings")
    })
    
    output$num_of_listenings_top_artist_2 <- renderPrint({
        cat(paste(sort(table(data()$artistName), decreasing=TRUE)[2] ), "listenings")
    })
    
    output$num_of_listenings_top_artist_3 <- renderPrint({
        cat(paste(sort(table(data()$artistName), decreasing=TRUE)[3] ), "listenings")
    })
    
    output$top_track_title_1 <- renderPrint({
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        cat( sub(";.*", "", top_tracks[1]) )
    })
    output$top_track_title_2 <- renderPrint({
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        cat( sub(";.*", "", top_tracks[2]) )
    })
    output$top_track_title_3 <- renderPrint({
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        cat( sub(";.*", "", top_tracks[3]) )
    })

    # artist of most played song
    output$top_track_artist_1 <- renderPrint({
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        cat( sub(".*;", "", top_tracks[1]) )
    })
    output$top_track_artist_2 <- renderPrint({
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        cat( sub(".*;", "", top_tracks[2]) )
    })
    output$top_track_artist_3 <- renderPrint({
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        cat( sub(".*;", "", top_tracks[3]) )
    })
    
    output$num_of_listenings_top_track_1 <- renderPrint({
        cat(paste(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE)[1]), "listenings")
    })
    
    output$num_of_listenings_top_track_2 <- renderPrint({
        cat(paste(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE)[2]), "listenings")
    })
    
    output$num_of_listenings_top_track_3 <- renderPrint({
        cat(paste(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE)[3]), "listenings")
    })
    
    output$longest_track_min_played <- renderPrint({
        cat( round(max(data()$minPlayed),0) )
    })
    
    output$longest_track_name <- renderPrint({
        cat(data()[data()$minPlayed == max(data()$minPlayed),"trackName"])
    })
    
    output$longest_track_artist <- renderPrint({
        cat(data()[data()$minPlayed == max(data()$minPlayed),"artistName"])
    })
    
    # average minutes played in hour
    draw_plot_avg_mins_played <- function(data, title) {
        
        temp <- data
        
        temp$day_hour = substr(temp$endTime, 1, 13)
        temp$day_hour
        total_mins_per_hour_by_day = aggregate(temp$minPlayed, by=temp["day_hour"], FUN=sum)
        total_mins_per_hour_by_day$hour = substr(total_mins_per_hour_by_day$day_hour, 11, 13)
        total_mins_per_hour_by_day
        
        total_mins_per_hour = aggregate(total_mins_per_hour_by_day$x, by=total_mins_per_hour_by_day["hour"], FUN=sum)
        total_mins_per_hour
        summary(total_mins_per_hour)
        
        # number of how many times I have listened music at a specific hour 
        # (when I have listened a couple of tracks in one hour but one day, it still counts as one.
        # it counts just how many times i played at a specific hour  but on separate days)
        temp <- as.data.frame(table(total_mins_per_hour_by_day$hour))
        
        # total number of minutes played during a specific hour
        temp$total_mins <- total_mins_per_hour$x
        
        # mean number of minutes of music played at a specific hour
        temp$mean = temp$total_mins / temp$Freq
        
        temp$Var1 <- as.character(temp$Var1)
        
        #plot(temp$Var1, temp$mean, type="o", xlab="hour", ylab="mean minutes played", main=title, ylim=c(1,60))
        
        dataframe <- data.frame(avg_mins_played = temp$mean, hour = as.integer(temp$Var1))
        ggplot(dataframe, aes(x = hour, y = avg_mins_played)) +
            geom_point(colour="navy") +
            ggtitle("Your average listening time per hour") +
            geom_line(colour="steelblue") +
            theme_minimal() +
            ylim(c(0,60))+
            theme(panel.grid.minor.x = element_blank()) +
            ylab("average minutes played") +
            xlab("hour") +
            theme(plot.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
            theme(panel.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
            scale_x_continuous(breaks = seq(0,23), labels = seq(0,23)) +
            theme(aspect.ratio = 2/5)
    }
    
    output$plot_avg_mins_played <- renderPlot({
        draw_plot_avg_mins_played(data(), title="every weekday")
    })
    
    output$plot_total_tracks_per_hour <- renderPlot({
        dataframe <- data.frame(endTime = as.POSIXct(data()$endTime, format = "%Y-%m-%d %H:%M"), hour = as.integer(data()$hour))
        
        ggplot(dataframe, aes(x = hour)) + 
            geom_histogram(bins = 24, colour = "#ffffff", fill = "#a7eb66") + 
            #coord_polar(start = 0) + 
            theme_minimal() + 
            theme(panel.grid.minor.x = element_blank()) +
            # scale_fill_continuous(low = "#ffff00", high = "#84d31c") +
            ylab("Total number of tracks") +
            #theme(plot.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
            #theme(panel.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
            scale_x_continuous(breaks = seq(-0.5,22.5), labels = seq(0,23))
    })
    
    output$plot_total_tracks_per_month <- renderPlot({
        dataframe <- data.frame(endTime = as.POSIXct(data()$endTime, format = "%Y-%m-%d %H:%M"), month = as.integer(data()$month))

        ggplot(dataframe, aes(x = month, fill=factor(month))) +
            geom_bar() +
            theme_minimal() +
            theme(panel.grid.minor.x = element_blank()) +
            theme(panel.grid.major.x = element_blank()) +
            ylab("Total number of tracks") +
            xlab("Month") +
            scale_x_continuous(breaks = seq(1,12), labels = seq(1,12)) +
            theme(legend.position='none')
    })


    output$hour_max_tracks_played <- renderPrint({
        cat( names(sort(table(data()$hour), decreasing=TRUE)[1]) )
    })
    
    output$plot_total_tracks_per_weekday <- renderPlot({
        dataframe <- data.frame(endTime = data()$endTime, weekday = data()$weekday)
        
        max_value <- max(table(dataframe$weekday))
        
        dataframe$weekday <- factor(dataframe$weekday, 
                                    levels= rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
        ggplot(dataframe, aes(y=weekday)) +
            theme_minimal() +
            coord_cartesian(xlim = c(0, 1.1*max_value)) +
            theme(panel.grid.major.x = element_blank()) +
            theme(panel.grid.major.y = element_blank()) +
            theme(panel.grid.minor.x = element_blank()) +
            geom_bar(width = 0.4, fill = "#cbd0d6") +
            theme(axis.title.y = element_blank()) +
            xlab("Total number of tracks")
    })    
    
    output$weekday_max_tracks_played <- renderPrint({
        cat( names(sort(table(data()$weekday), decreasing=TRUE)[1]) )
    })
    
    output$day_max_mins_played <- renderPrint({
        temp <- data.frame( min_played = data()$minPlayed, day = as.factor(substr(data()$endTime,1,10)) )
        temp <- aggregate(temp$min_played, list(temp$day), sum)
        cat( paste("On", as.character( temp[temp$x == max(temp$x),1] ),"you had been listening for a longest time," ) )
    })
    
    output$max_hours_played_per_day <- renderPrint({
        temp <- data.frame( min_played = data()$minPlayed, day = as.factor(substr(data()$endTime,1,10)) )
        temp <- aggregate(temp$min_played, list(temp$day), sum)
        cat( paste0(round( (temp[temp$x == max(temp$x),2]/60),1), " hours.") )
    })
    
    output$top_artist_day_max_mins_played <- renderPrint({
        temp <- data.frame( min_played = data()$minPlayed, day = as.factor(substr(data()$endTime,1,10)) )
        temp <- aggregate(temp$min_played, list(temp$day), sum)
        temp1 <- data()[ which( as.POSIXct(substr(data()$endTime,1,10), format = "%Y-%m-%d") == as.character( temp[temp$x == max(temp$x),1] ) ), ]
        cat( names(sort(table(temp1$artistName), decreasing=TRUE)[1]) )
    })
    
    # top x % most frequently listened artists
    output$count_of_top_artists <- renderPrint({
        proportion_of_artists <- 0.87
        temp2 <- data()
        top_tracks <- names(sort(table( paste(temp2$trackName,temp2$artistName, sep=";;;") ), decreasing=TRUE))
        temp2 <- as.data.frame(head(top_tracks, (length(top_tracks) * proportion_of_artists)))
        temp2 <- temp2 %>% 
            separate('head(top_tracks, (length(top_tracks) * proportion_of_artists))', into = c("track", "artist"), sep = ";;;")
        
        cat(length(table(temp2$artist )))
    })
    
    output$percent_of_top_artists <- renderPrint({
        proportion_of_artists <- 0.87
        cat(proportion_of_artists*100)
    })
    
    output$plot_audio_features <- renderPlot({
        top_tracks <- names(sort(table( paste(data()$trackName,data()$artistName, sep=";") ), decreasing=TRUE))
        top_artists <- sub(".*;", "", top_tracks)
        top_tracks <- sub(";.*", "", top_tracks)
        songs_and_artists <- data.frame(trackName=top_tracks, artistName=top_artists, stringsAsFactors = FALSE)
        draw_plot_audio_features(songs_and_artists)
    })

})
