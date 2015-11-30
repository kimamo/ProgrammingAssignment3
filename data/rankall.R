
rankall <- function(outcome, num="best"){
  #read outcome csv file
  d <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character" )
  
  # gets all unique states abbrevs in the file
  state <- unique(d$State, incomparables = FALSE)
  #print(length(state))
    
  #vector holding outcomes to check against
  outcomes <- c("heart attack","heart failure", "pneumonia")
  
  #if outcome given not in my outcomes vector, spit out invalid outcome message
  if(!is.element(outcome,outcomes)){stop("Invalid Outcome")}
   
  hospital <- vector(mode = "character")
  
  for (st in 1:length(state)) {
    hospital[st] <- rankhospital(state[st],outcome,num)
  }
  
  data.frame(hospital, state)
}