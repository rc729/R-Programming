source("best.R")

## Helper function to improve readability in the main function (rankhospital)
## This function removes NA values and returns the desired hospital name by num
## given the specified outcome and state.
##
##    data: the dataset
##    outcome_column: the column (integer) of the dataset for 30-day mortality 
##                    rates of the 3 outcomes
##    state: the 2-character abbreviated name of state
##    num: the ranking of a hospital of the specified outcome/state
##

rankHelper <- function(data, outcome_column, state, num) {
    # Get the valid subset of hospitals given the outcome/state
    state_subset <- data[data[,7] == state,]
    valid_subset <- state_subset[!is.na(state_subset[outcome_column]),]
    
    # Order the subset by outcome in ascending order
    sorted_subset <- valid_subset[order(valid_subset[outcome_column], valid_subset[2]),]

    # Get the hospital names in rank order
    ordered_hospitals <- sorted_subset[,2]

    # worst rank is last index of sorted hospital names
    if (num == "worst") {
        result <- ordered_hospitals[length(ordered_hospitals)]
    }
    # Check if num is greater than length of valid hospitals
    else if(num > dim(sorted_subset)[1]) {
        result <- NA
    }
    else {
        result <- ordered_hospitals[num]
    }
    return(result)
}

## This function takes three arguments:
##
##    state: 2-character abreviated name of state
##    outcome: character vector of the outcome name
##    num: the ranking of a hospital of the specified outcome/state
##
## It returns a character vector with the name of the hospital that has the 
## ranking specified by the *num* argument. The argument *num* can take integer
## values indicating the ranking (the smaller the better the ranking). It can 
## also take the values "best" and "worst" which represents the lowest or highest
## ranked hospital. If the number given by num is larger than the number of 
## hospitals in that state, then the function will return NA. Hospitals that 
## do not have data on a particular outcome are excluded from the set when
## deciding ranking.
rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Change data types of rates from character to numeric
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
    
    ## Vector of valid outcomes
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check for valid state and outcome
    if(!state %in% data$State) {
        stop("invalid state")
    }
    if(!outcome %in% valid_outcomes) {
        stop("invalid outcomes")
    }
    
    ## Cases
    if(num == "best") {
        result <- best(state, outcome)
    }
    
    if(outcome == "heart attack") {
        result <- rankHelper(data, 11, state, num)
    }
    
    if(outcome == "heart failure") {
        result <- rankHelper(data, 17, state, num)
    }
    
    if(outcome == "pneumonia") {
        result <- rankHelper(data, 23, state, num)
    }
    
    return(result)
}

## Test
# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)

## Quiz
# rankhospital("NC", "heart attack", "worst")
# rankhospital("WA", "heart attack", 7)
# rankhospital("TX", "pneumonia", 10)
# rankhospital("NY", "heart attack", 7)
