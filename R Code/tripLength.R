tripLength <- function(startStationID,endStationID){
  
  ## Load relavent CSV files & packages into workspace
  trips <- read.csv("HackBikeShareTO-Trips.csv")
  
  trips <- trips[,c("Trip.ID", "Duration", "Start.Date", "Start.Station.ID", "End.Station.ID")]
  colnames(trips)[3] <- "Date"
  trips <- splitDateTime(trips)
  
  trips <- trips[,c("Trip.ID", "Duration", "Start.Station.ID", "End.Station.ID", "Date2", "Time")]
  colnames(trips)[5] <- "Date"
  trips <- trips[trips$Start.Station.ID == startStationID | trips$Start.Station.ID == endStationID,]
  trips <- trips[trips$End.Station.ID == startStationID | trips$End.Station.ID == endStationID,]
  trips <- trips[trips$Start.Station.ID != trips$End.Station.ID,]
  trips <- trips[complete.cases(trips),]
  trips$Duration <- paste0("0",trips$Duration)
  trips$Duration <- strptime(trips$Duration,format = "%H:%M:%S")
  
  duration <- mean(trips$Duration)
  duration <- strsplit( as.character( duration ) , " " )
  
  duration <- duration[[1]]
  duration <- duration[2]
  duration <- strsplit( as.character( duration ) , ":" )
  duration <- duration[[1]]
  duration <- as.numeric(duration[2:3])
  
  ifelse(duration[2] > 30,(FinalDuration = duration[1] + 1),(FinalDuration = duration[1]))
  
}