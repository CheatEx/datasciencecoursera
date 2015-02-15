## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) {
  res <- data.frame(id=integer(), nobs=integer())
  
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
    complete <- complete.cases(data)
    
    res <- rbind(res, list(id=fileId, nobs=nrow(data[complete,])))
  }
  
  res
}