## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332) {
  all <- vector("numeric")
  
  for (fileId in id) {
    fileNum <-
      if (fileId < 10)
        paste("00", fileId, sep="")
      else if (fileId < 100)
        paste("0", fileId, sep="")
      else
        paste(fileId)
    
    fileName <- paste(directory,"/",fileNum,".csv", sep="")
    data <- read.csv(fileName)
    measures <- na.omit(data[[pollutant]])
    all <- c(all, measures)
  }
  
  mean(all)
}