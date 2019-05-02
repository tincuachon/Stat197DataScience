#---------------Part 1---------------#

setwd("C:/Users/user/Desktop")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)


#ncol(outcome)-to know how many columns in the data
#names(outcome) - the name of each column in the data

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11], main = "30-day death rates from heart attack")

#---------------Part 2---------------#

best<-function(state, outcome){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
    best.data   <- as.data.frame(cbind(data[, 2],   # hospital name
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE) #convert all characters into factor
    colnames(best.data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if(!state %in% best.data[, "state"]){ #is the current element of !state equal to any value in best.data[, "state"]
        stop('invalid state')
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {
        SortState <- which(best.data[, "state"] == state) # sorts cases; give the TRUE indices of a logical object
        ExtractState <- best.data[SortState, ]    # extracting data for the called state
        getValues <- as.numeric(ExtractState [, outcome])# all the values od the outcome for that state
        minval <- min(getValues, na.rm = TRUE) # minimum number of deaths of the outcome for that state
        result  <- ExtractState[, "hospital"][which(getValues  == minval)] # finds the hospital that contains that minimum value for that state
        output  <- result[order(result)] # breaking ties, arranging by ascending order
    }
    return(output)
    
}

best("TX", "heart attack")
best("TX", "heart failure")

best("MD", "heart attack")
best("MD", "pneumonia")


best("BB","heart attack")
best("NY","hert attack")