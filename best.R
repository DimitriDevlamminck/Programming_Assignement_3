# create a function that will returnn the best hospital given two urguments: the state and the outcome
# "state" is a two letters state abreviation
# "outcome" is : heart attack, heart failure, pneumonia
# by besg hospital we mean the one with the lowest death rate


best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  options(warn=-1) # sets R warnings off for NA data
  
  # Check that the "state" entered in the fucntion is valid
  if(!(state %in% data$State)) { 
    stop("invalid state entered")
  }
  
  # for each "outcome" perform the right selection
  
  if(outcome == "heart attack") {
    findBest(11, state, data)
  }
  else if (outcome == "heart failure") {
    findBest(17, state, data)
  }
  else if(outcome == "pneumonia") {
    findBest(23, state, data)
  }
  else {
    stop("invalid outcome entered")
  }  
  
}

# Sub function that finds the best hospiral for the given outcome number
findBest <- function(outcomeNumber, state, data) {
  data[, outcomeNumber] <- as.numeric(data[, outcomeNumber])
  data<-subset(data,data$State==state) # only need info for given state
  valMin<-min(data[[outcomeNumber]],na.rm=TRUE) # best in this case = minimum
  data<-subset(data,data[[outcomeNumber]]==valMin)
  data<-data[order(data[["Hospital.Name"]]),]
  return(data[1,"Hospital.Name"]) # return best hospital name
}
