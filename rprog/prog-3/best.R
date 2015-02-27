best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
  
  states <- levels(factor(data$State))
  
  if (length(states[states==state]) == 0)
    stop("invalid state")
  
  localHospitals <- data[data$State==state,]
  
  if (outcome=="heart attack") {
    col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome=="heart failure") {
    col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome=="pneumonia") {
    col <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  localHospitals[, col] <- as.numeric(localHospitals[, col])
  dataCol <- localHospitals[, col]
  m <- min(dataCol, na.rm=TRUE)
  
  tie <- localHospitals[dataCol==m,]
  win <- sort(tie$Hospital.Name)[1]
  win
}