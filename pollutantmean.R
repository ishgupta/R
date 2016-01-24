pollutantmean <- function(directory, pollutant, id=1:332){
  pollutantmean <- 0
  meanPerMonitor <-c()
  tmpMean <- 0
  
  for(filename in id){
    tmpId <- filename
    filename <- str_pad(filename, 3, "left", pad="0")
    filename <- paste(filename, ".csv", sep="")
    
    filename <- file.path(directory, filename)
    
    df <- read.csv(filename, header=TRUE)
    
    if(!all(is.na(df[[pollutant]]))){
        tmpMean <- mean(df[[pollutant]], na.rm = TRUE)
    }
    
    meanPerMonitor <- append(meanPerMonitor, tmpMean)
  }
  mean(meanPerMonitor)
}
