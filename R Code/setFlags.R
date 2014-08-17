setFlags <- function(data){
  
  change <- numeric()
  change[1] <- 0
  flag <- character()
  flag[1] <- "idle"
  
  for(i in 2 : nrow(data)) {
    flux <- data[i,3] - data[i-1,3]
    change <- rbind(change,flux)
    
    if (change[i] == 0) {
      flag[i] <- "idle"
    }
    else if (change[i] < 0) {
      flag[i] <- "in"
    }
    else if (change[i] > 0) {
      flag[i] <- "out"
    }
  }
  
  flag <- as.factor(flag)
  
  cbind(data,change,flag)
}