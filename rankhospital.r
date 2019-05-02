outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)


#---------------Part 3---------------#
# create a rankhospital function
rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  best.data   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(best.data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% best.data[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    SortState <- which(best.data[, "state"] == state)
    ExtactState <- best.data[SortState, ]                     # extracting dataframe for the called state
    rankhosp <- ExtactState[order(as.numeric(ExtactState[, outcome]), ExtactState[, "hospital"]), ] # ordered elements, ranks
    output <- rankhosp[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      SortState <- which(best.data[, "state"] == state)
      ExtactState <- best.data[SortState, ]    
      rankhosp <- ExtactState[order(as.numeric(ExtactState[, outcome]), ExtactState[, "hospital"], decreasing = TRUE),] # sorting by decreasing order
      output <- rankhosp[, "hospital"][1]
    } else {
      stop('invalid rank') 
    }
  }
  return(output)
}

rankhospital("TX", "heart failure",4)

rankhospital("MD", "heart attack","worst")

rankhospital("MN","heart attack", 5000)




