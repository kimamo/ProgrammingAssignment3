best <- function(state, outcome) {
  #read outcome csv file
  d <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character" )
  #boolean flag to set if state abbrev invalid
  isState <- FALSE
  
  # gets all unique states abbrevs in the file
  states <- unique(d$State, incomparables = FALSE)
  
  
  for (st in 1:length(states)) {
    if (state == states[st]) { isState <- TRUE }
  }
  
  #check if IsState is valid and spit invalid state message if otherwise
  # stopifnot(isState,states, op="Invalid State" )
  if (!isState) {stop("Invalid State")}
  
  #vector holding outcomes to check against
  outcomes <- c("heart attack","heart failure", "pneumonia")
  
  #if outcome given not in my outcomes vector, spit out invalid outcome message
  if(!is.element(outcome,outcomes)){stop("Invalid Outcome")}
  
  #get hospital name in that state with lowest 30-day death rate
col <- 23
  if(outcome=="heart attack"){ col <- 11}
  else if (outcome=="heart failure"){col <- 17}
  
  # get subset of the given state data
  stdata  <- subset(d,d$State==state)

  #clean up data by removing NA values from state subset
  wantedCols <- suppressWarnings(as.numeric(stdata[, col]))
  mal <- is.na(wantedCols)
  cleanData <- stdata[!mal, ] 
  
  #get hospital with min outcome value
  myCols <- suppressWarnings(as.numeric(cleanData[,col]))
  myRows <- which(myCols == min(myCols))
  theHospitals <- cleanData[myRows,2]
  
  #sort if more than one returned
  if (length(theHospitals) > 1) {
    sortedRanks <- sort(theHospitals)
    sortedRanks[1]
  }else
  {
    theHospitals
  }
}






