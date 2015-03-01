rankhospital <- function(state, outcome, rank) {
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
  
  states <- levels(factor(data$State))
  
  if (length(states[states==state]) == 0)
    stop("invalid state")
  
  if (outcome=="heart attack") {
    col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome=="heart failure") {
    col <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome=="pneumonia") {
    col <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  lh <- data[data$State==state,]
  lh[, col] <- as.numeric(lh[, col])
  lh <- lh[!is.na(lh[, col]),]
  
  dataCol <- lh[, col]
  lh <- lh[
    order(dataCol, lh$Hospital.Name),]
  
  total = nrow(lh)
  
  if (rank == "best") {
    lh$Hospital.Name[1]
  } else if (rank == "worst") {
    lh$Hospital.Name[total]
  } else if (rank > total) {
    NA;
  } else {
    lh$Hospital.Name[rank]
  }
}
