splitDateTime <- function(data){
  new <- do.call( rbind , strsplit( as.character( data$Date ) , " " ) )
  cbind( data , Date2 = new[,1] , Time = new[,2] )
}