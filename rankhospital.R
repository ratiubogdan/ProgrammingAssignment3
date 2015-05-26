rankhospital <- function(state, cond, num = "best") {
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
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with num 30-day death
  ## rate
  if (cond == "heart attack") {
    poz <- 11
  } else if (cond == "heart failure") {
    poz <- 17
  } else {
    poz <- 23
  }
  
  stateoutcome <- subset(outcome, outcome$State == state)
  list <- order(stateoutcome[,poz], stateoutcome[,2], na.last = NA)
  if (num == "best") {
    print(stateoutcome[list[1],2])
  } else if (num == "worst") {
    print(stateoutcome[list[length(list)],2])
  } else if (num > length(list)) {
    print("NA")
  } else {
    print(stateoutcome[list[num],2])
  }
  

}