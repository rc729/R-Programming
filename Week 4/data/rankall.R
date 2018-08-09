## Helper function to improve readability in the main function (rankall)
## This function removes NA values from a state subset and returns the desired 
## hospital name by num given the specified outcome and state.
##
##    state_subset: the dataset of only a single state
##    outcome_column: the column (integer) of the dataset for 30-day mortality 
##                    rates of the 3 outcomes
##    num: the ranking of a hospital the specified outcome/state

rankallHelper <- function(state_subset, outcome_column, num) {
    
    # Remove NA's from data
    valid_set <- state_subset[!is.na(state_subset[outcome_column]),]
    # Sort by ascending order of rates then alphabetically by hospital name
    sorted <- valid_set[order(valid_set[outcome_column], valid_set[2]),]
    # Vector of ordered hospital names
    hospitals <- sorted[,2]
    if(num == "best") {
        result <- hospitals[1]
    }
    else if(num == "worst") {
        result <- hospitals[length(hospitals)]
    }
    else if(num > length(hospitals)) {
        result <- NA
    }
    else {
        result <- hospitals[num]
    }
    return(result)
}

## This function takes two arguments:
##
##    outcome: character vector of the outcome name
##    num: the ranking of a hospital of the specified outcome/state
##
## It returns a two-column data frame: 1) names of the hospitals determined by
## the specified rank and outcome. 2) the state associated with the hospital. 
##
## It should return a value for every state, but some states may return NA 
## if there are no hospitals that satisfy the rank condition. 

rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Change data types of rates from character to numeric
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
    
    ## Vector of valid outcomes
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check for valid outcome
    if(!outcome %in% valid_outcomes) {
        stop("invalid outcomes")
    }
    
    ## Create a list of states to iterate
    state_list <- sort(unique(data$State))
    
    ## Create empty resulting hospital names vector
    hospitals <- c()
    
    ## Loop through each state to get the corresponding hospital
    for(i in 1:length(state_list)) {
        state_subset <- data[data[,7] == state_list[i],]
        if(outcome == "heart attack") {
            name <- rankallHelper(state_subset, 11, num)
        }
        else if(outcome == "heart failure") {
            name <- rankallHelper(state_subset, 17 ,num)
        }
        else {
            name <- rankallHelper(state_subset, 23, num)
        }
        hospitals <- c(hospitals, name)
    }
    # Create two-column data frame
    result <- data.frame(hospital=hospitals, state=state_list)
    return(result)
}
## Test
# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)

## Quiz
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
