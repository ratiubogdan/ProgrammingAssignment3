rankall <- function(cond, num = "best") {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = F, na.strings = "Not Available")
  #need <- c(2,7,11,17,23)
  #outcome <- outcome[,need]
  
  
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
  
  factor <- factor(outcome$State)
  
  #stateoutcome <- subset(outcome, outcome$State == state)
  i <- 1
  df <- data.frame(hospital=character(54), state=character(54), stringsAsFactors = FALSE)
  for (st in levels(factor)) {
    #print(paste("new state", st))
    stateoutcome <- subset(outcome, outcome$State == st)
    list <- order(stateoutcome[,poz], stateoutcome[,2], na.last = NA)
    if (num == "best") {
      df[i,1] <- stateoutcome[list[1],2]
      
      #print(stateoutcome[list[1],2])
    } else if (num == "worst") {
      df[i,1] <- stateoutcome[list[length(list)],2]
      #print(stateoutcome[list[length(list)],2])
    } else if (num > length(list)) {
      df[i,1] <- "NA"
      #print("NA")
    } else {
      df[i,1] <- stateoutcome[list[num],2]
      #print(stateoutcome[list[num],2])
    }
    df[i,2] <- st
    i <- i + 1
  }
  df
}