rankall <- function(outcome, rank = "best") {
  if (outcome=="heart attack") {
    dataColName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome=="heart failure") {
    dataColName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome=="pneumonia") {
    dataColName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  if (is.na(as.numeric(rank)) && rank!="best" && rank!="worst")
    stop("invalid rank")
  
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
  data$State <- factor(data$State)
  split <- split(data, data$State)
  
  r <- lapply(split, function(stateFrame) {
    suppressWarnings(stateFrame[, dataColName] <- as.numeric(stateFrame[, dataColName]))
    stateFrame <- stateFrame[!is.na(stateFrame[, dataColName]),]
    stateFrame <- stateFrame[
      order(stateFrame[dataColName], stateFrame$Hospital.Name),] 
    total <- nrow(stateFrame)
    if (rank == "best") {
      hospital <- stateFrame$Hospital.Name[1]
    } else if (rank == "worst") {
      hospital <- stateFrame$Hospital.Name[total]
    } else if (rank > total) {
      hospital <- NA;
    } else {
      hospital <- stateFrame$Hospital.Name[rank]
    }
  })
  
  data.frame(hospital=unlist(r), state=names(r))
}
