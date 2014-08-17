createProbabilityMatrix <- function(probs,totalDocks){
  
  probabilityMatrix <- matrix(numeric(0),totalDocks+1,totalDocks+1)
  colnames(probabilityMatrix) <- (0:totalDocks)
  rownames(probabilityMatrix) <- (0:totalDocks)
  
  for (i in 1:nrow(probabilityMatrix)){
    if(i == 1){
      probabilityMatrix[i,i] = 1- counts[2,3]
      probabilityMatrix[i,i+1] = counts[2,3]
    }
    else if (i == nrow(probabilityMatrix)){
      probabilityMatrix[i,i] = 1 - counts[3,3]
      probabilityMatrix[i,i-1] = counts[3,3]
    }
    else{
      probabilityMatrix[i,i] = counts[1,3]
      probabilityMatrix[i,i+1] = counts[2,3]
      probabilityMatrix[i,i-1] = counts[3,3]
    }
  }
  
  for (i in 1:nrow(probabilityMatrix)){
    for (j in 1:nrow(probabilityMatrix)){
      if (is.na(probabilityMatrix[i,j])){
        probabilityMatrix[i,j] <- 0
      }
    }
  }
  
  probabilityMatrix
}