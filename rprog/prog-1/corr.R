## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) {
  res <- numeric(0)
  
  for (file in dir(directory, full.names=TRUE)) {
    data <- read.csv(file)
    complete <- complete.cases(data)
    completeData <- data[complete,]
    if (nrow(completeData) > threshold) {
      v <- cor(completeData$sulfate, completeData$nitrate)
      res <- c(res, v)
    }
  }
  
  res
}