best <- function(state, cond) {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = F, na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  if (state %in% outcome[,7]) {
      #nothing
  } else {
    stop("invalid state")
  }
  
  conditions <- c("heart attack", "heart failure", "pneumonia")
  if (cond %in% conditions) {
    #nothing
  } else {
    stop("invalid condition")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if (cond == "heart attack") {
    poz <- 11
  } else if (cond == "heart failure") {
    poz <- 17
  } else {
    poz <- 23
  }
  
  stateoutcome <- subset(outcome, outcome$State == state)
  list <- which(stateoutcome[,poz] == min(stateoutcome[,poz], na.rm = T))
  print(min(stateoutcome[list,2]))
  
}