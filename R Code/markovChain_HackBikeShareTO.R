markovChain_HackBikeShareTO <- function(stationID,weekday,hour,tripLength){

## Load relavent CSV files & packages into workspace
rebal <- read.csv("HackBikeShareTO-Rebalancing.csv")  ###Rebalancing
stations <- read.csv("HackBikeShareTO-Stations.csv")   ### Stations
source("splitDateTime.R")
source("setFlags.R")
source("FindProbability.R")
source("createProbabilityMatrix.R")
source("PowerMatrix.R")
library(chron)
library(expm)

probabilities <- FindProbabilities(rebal,stationID,weekday,hour)

probMatrix <- createProbabilityMatrix(probabilities,stations[stations$Station.ID == stationID,"Total.Docks"])

powerMatrixFinal <- PowerMatrix(probMatrix,tripLength)

powerMatrixFinal
}
