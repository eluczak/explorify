server <- function(input, output) {
  # imports and some general settings
  library(jsonlite)
  library(ggplot2)
  library(dplyr)
  # line below is used for getting English weekdays' names
  Sys.setlocale("LC_TIME", "C")
  
  # to do: sanitizing the input

  # reading the data
  # to do: read multiple files and join them
  original_data <- fromJSON("www/data/StreamingHistory0.json", simplifyVector = TRUE)
  dataset <- original_data
  
  # to do: check if the data is correct
  
  # including only tracks which were played longer than 0.5 min
  dataset <- dataset[ which(dataset$msPlayed/60000>0.5), ]
  
  # subsetting to a specific date range
  date_start <- "YYYY-MM-DD"
  date_end <- "YYYY-MM-DD"
  #dataset <- dataset[ which( as.POSIXct(substr(dataset$endTime,1,10), format = "%Y-%m-%d") <= "date_end" ), ]
  
  # preparing fields
  dataset$minPlayed <- round(dataset$msPlayed/60000, 2)
  dataset$year <- substr(dataset$endTime, 1, 4)
  dataset$month <- substr(dataset$endTime, 6, 7)
  dataset$day <- substr(dataset$endTime, 9, 10)
  dataset$hour <- substr(dataset$endTime, 12, 13)
  dataset$min <- substr(dataset$endTime, 15, 16)
  dataset$weekday <- weekdays(as.POSIXct(dataset$endTime, format = "%Y-%m-%d %H:%M"))
  dataset$artistName <- dataset$artistName
  dataset$trackName <- dataset$trackName
  # to do: get genre and other tracks' features via Spotify API
  
  # to do: correctness check after transformation (I am not sure if it is necessary though)
  
  # quick summary 
  
  output$num_of_tracks <- renderPrint({
    cat(length(dataset$endTime))
  })
  
  output$start_date <- renderPrint({
    cat( substr(min(dataset$endTime),1,10) )
  })

  output$end_date <- renderPrint({
    cat( substr(max(dataset$endTime),1,10) )
  })
  
  output$total_min_played <- renderPrint({
    cat( round(sum(dataset$minPlayed),0) )
  })
  
  # to do: make following lines non-repeatable 
  output$top_artist_1 <- renderPrint({
    cat( names(sort(table(dataset$artistName), decreasing=TRUE)[1]) )
  })
  
  output$top_artist_2 <- renderPrint({
    cat( names(sort(table(dataset$artistName), decreasing=TRUE)[2]) )
  })
  
  output$top_artist_3 <- renderPrint({
    cat( names(sort(table(dataset$artistName), decreasing=TRUE)[3]) )
  })
  
  output$top_artist_4 <- renderPrint({
    cat( names(sort(table(dataset$artistName), decreasing=TRUE)[4]) )
  })
  
  output$top_artist_5 <- renderPrint({
    cat( names(sort(table(dataset$artistName), decreasing=TRUE)[5]) )
  })
  
  output$top_artist_6 <- renderPrint({
    cat( names(sort(table(dataset$artistName), decreasing=TRUE)[6]) )
  })
  
  # to do: make following lines non-repeatable
  top_tracks <- names(sort(table( paste(dataset$trackName,dataset$artistName, sep=";") ), decreasing=TRUE))
  # title of most played song
  output$top_track_title_1 <- renderPrint({
    cat( sub(";.*", "", top_tracks[1]) )
  })
  output$top_track_title_2 <- renderPrint({
    cat( sub(";.*", "", top_tracks[2]) )
  })
  output$top_track_title_3 <- renderPrint({
    cat( sub(";.*", "", top_tracks[3]) )
  })
  output$top_track_title_4 <- renderPrint({
    cat( sub(";.*", "", top_tracks[4]) )
  })
  output$top_track_title_5 <- renderPrint({
    cat( sub(";.*", "", top_tracks[5]) )
  })
  output$top_track_title_6 <- renderPrint({
    cat( sub(";.*", "", top_tracks[6]) )
  })
  # artist of most played song
  output$top_track_artist_1 <- renderPrint({
    cat( sub(".*;", "", top_tracks[1]) )
  })
  output$top_track_artist_2 <- renderPrint({
    cat( sub(".*;", "", top_tracks[2]) )
  })
  output$top_track_artist_3 <- renderPrint({
    cat( sub(".*;", "", top_tracks[3]) )
  })
  output$top_track_artist_4 <- renderPrint({
    cat( sub(".*;", "", top_tracks[4]) )
  })
  output$top_track_artist_5 <- renderPrint({
    cat( sub(".*;", "", top_tracks[5]) )
  })
  output$top_track_artist_6 <- renderPrint({
    cat( sub(".*;", "", top_tracks[6]) )
  })
  
  output$longest_track_duration <- renderPrint({
    cat( round(max(dataset$minPlayed),0) )
  })
  
  output$longest_track_name <- renderPrint({
    cat(dataset[dataset$minPlayed == max(dataset$minPlayed),"trackName"])
  })
  
  output$longest_track_artist <- renderPrint({
    cat(dataset[dataset$minPlayed == max(dataset$minPlayed),"artistName"])
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
      geom_point() +
      geom_line(colour="#67eb33") +
      #geom_point(colour = "#f7f7f7", fill="#67eb33") +
      #coord_polar(start = 0) + 
      theme_minimal() +
      ylim(c(0,60))+
      theme(panel.grid.minor.x = element_blank()) +
      ylab("average minutes played") +
      xlab("hour") +
      theme(plot.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
      theme(panel.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
      scale_x_continuous(breaks = seq(0,23), labels = seq(0,23))
  }
  
  output$plot_avg_mins_played <- renderPlot({
    draw_plot_avg_mins_played(dataset, title="every weekday")
  })
  
  output$plot_total_tracks_per_hour <- renderPlot({
    dataframe <- data.frame(endTime = as.POSIXct(dataset$endTime, format = "%Y-%m-%d %H:%M"), hour = as.integer(dataset$hour))
    
    ggplot(dataframe, aes(x = hour)) + 
      geom_histogram(bins = 24, colour = "#f7f7f7", fill="#67eb33") + 
      coord_polar(start = 0) + 
      theme_minimal() + 
      theme(panel.grid.minor.x = element_blank()) +
      ylab("Number of tracks") +
      theme(plot.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
      theme(panel.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
      scale_x_continuous(breaks = seq(-0.5,22.5), labels = seq(0,23))
  })

  output$hour_max_tracks_played <- renderPrint({
    cat( names(sort(table(dataset$hour), decreasing=TRUE)[1]) )
  })
  
  output$plot_total_tracks_per_weekday <- renderPlot({
    dataframe <- data.frame(endTime = dataset$endTime, weekday = dataset$weekday)
    dataframe$weekday <- factor(dataframe$weekday, 
                                levels= rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
    ggplot(dataframe, aes(x = weekday)) + 
      geom_histogram(bins = 7, colour = "#67eb33", fill="#67eb33", stat="count") + 
      theme_minimal() + 
      coord_flip() +
      theme(panel.grid.major.x = element_blank()) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(panel.grid.major.y = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(plot.background = element_rect(fill = "#f7f7f7", colour = "#f7f7f7")) +
      ylab("Number of tracks")
  })
  
  output$weekday_max_tracks_played <- renderPrint({
    cat( names(sort(table(dataset$weekday), decreasing=TRUE)[1]) )
  })
  
  temp <- data.frame( min_played = dataset$minPlayed, day = as.factor(substr(dataset$endTime,1,10)) )
  temp <- aggregate(temp$min_played, list(temp$day), sum)
  
  output$day_max_mins_played <- renderPrint({
    cat(as.character( temp[temp$x == max(temp$x),1] ))
  })
  
  output$max_hours_played_per_day <- renderPrint({
    cat(round( (temp[temp$x == max(temp$x),2]/60),1))
  })
  
  temp1 <- dataset[ which( as.POSIXct(substr(dataset$endTime,1,10), format = "%Y-%m-%d") == as.character( temp[temp$x == max(temp$x),1] ) ), ]
  output$top_artist_day_max_mins_played <- renderPrint({
    cat( names(sort(table(temp1$artistName), decreasing=TRUE)[1]) )
  })

}

shinyApp(ui = htmlTemplate("www/index.html"), server)