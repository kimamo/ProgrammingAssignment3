



rankhospital <- function(state, outcome, num){
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
  
  
  #get Outcome columns from file
  col <- 23
  if(outcome=="heart attack"){ col <- 11}
  else if (outcome=="heart failure"){col <- 17}
  
  if(is.numeric(num) ==TRUE){
    if(length(d[,2]) < num){
      return(NA)
    }
  }
    
  
  # get subset of the given state data
  stdata  <- subset(d,d$State==state)
  
  #clean up data by removing NA values from state subset
  stdata[,col] <- suppressWarnings(as.numeric(stdata[, col]))
  mal <- is.na(stdata[,col])
  cleanData <- stdata[!mal, ] 
  
  myColName <- names(cleanData)[col]
  theHospitals <- names(cleanData)[2]
  
  #print(head(c(myColName, theHospital)))
  
  position <- with(cleanData,order(cleanData[myColName], cleanData[theHospitals])) 
  #print(position)
  orderedCleanData <-cleanData[position, ]
 # print(head(orderCleanData))
  
  if(is.character(num)==TRUE){
    if(num == "best"){
        num=1
      }
    else if(num == "worst"){
      num = length(orderedCleanData[,col])
     # print(num)
    }
   
  } 

  orderedCleanData[num, 2]
   
}



