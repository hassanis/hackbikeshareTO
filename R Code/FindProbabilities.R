FindProbabilities <- function(rebal,StationID,Day,Hour){
  ## Filter by provided Station ID
  rebal$Station.ID <- as.factor(as.character(rebal$Station.ID))
  rebal <- rebal[rebal$Station.ID == StationID,]
  
  ## Format Date and filter by weekday and hour
  rebal$Date <- strptime(rebal$Date, format = "%m-%d-%Y %H:%M:%S")
  rebal <- cbind(rebal,weekdays(rebal[,4]))
  colnames(rebal)[5] <- "Weekday"
  rebal <- rebal[rebal[,4] == Day,]
  rebal <- splitDateTime(rebal)
  rebal <- rebal[c("Station.ID","Bikes.Available", "Docks.Available", "Weekday", "Date2","Time")]
  rebal$Time <- chron(times = as.character(rebal$Time),format = "h:m:s")
  rebal$Date2 <- chron(dates = as.character(rebal$Date2),format = "Y-m-d")
  colnames(rebal)[5] <- "Date"
  rebal <- rebal[hours(rebal[,6]) == Hour,]
  
  ## Track changes in dock availability and set appropriate flags
  rebal <- setFlags(rebal)
  
  ## Find probabilities based on dock availability changes
  counts <- as.data.frame(table(rebal$flag))
  numWeeks <- length(table(rebal$Date))
  countsPerWeek <- counts/numWeeks
  probs <- countsPerWeek[2]/60
  probs <- rbind(probs,countsPerWeek[3]/60)
  probs <- rbind(probs,1-sum(probs[,1]))
  
  cbind(counts,sort(probs,TRUE))

}