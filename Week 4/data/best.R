## This function takes two arguments:
##
##    state: 2-character abreviated name of state
##    outcome: character vector of the outcome name  
##
## The outcomes are "heart attack", "heart failure", or "pneumonia". 
## The function returns a character vector of the hospital name that
## has the lowest 30-day mortality rate of the specified state/outcome.
## Hospitals that do not have data on the  specified outcome are excluded when 
## deciding the rankings.

best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Change data types of rates from character to numeric
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
    
    ## Vector of valid outcomes
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    state_subset <- data[data[,7] == state,]
    hospital_names <- c()
    
    ## Check for valid state and outcome
    if(!state %in% data$State) {
        stop("invalid state")
    }
    if(!outcome %in% valid_outcomes) {
        stop("invalid outcomes")
    }
    
    ## Find the set of hospitals with lowest rate of the specified outcome 
    ## and state
    if(outcome == "heart attack") {
        rates <- state_subset[, 11]
        lowest_rate <- min(rates, na.rm = T)
        hospital_names <- c(hospital_names, sort(state_subset[which(rates == lowest_rate), 2]))
    }
    if(outcome == "heart failure") {
        rates <- state_subset[, 17]
        lowest_rate <- min(rates, na.rm = T)
        hospital_names <- c(hospital_names, sort(state_subset[which(rates == lowest_rate), 2]))
    }
    if(outcome == "pneumonia") {
        rates <- state_subset[, 23]
        lowest_rate <- min(rates, na.rm = T)
        hospital_names <- c(hospital_names, sort(state_subset[which(rates == lowest_rate), 2]))
    }
    result <- hospital_names[1]
    print(hospital_names)
    return(result)
}

## Test Cases
# best("TX", "heart attack") == "CYPRESS FAIRBANKS MEDICAL CENTER"
# best("TX", "heart failure") == "FORT DUNCAN MEDICAL CENTER"
# best("MD", "heart attack") == "JOHNS HOPKINS HOSPITAL, THE"
# best("MD", "pneumonia") == "GREATER BALTIMORE MEDICAL CENTER"
# best("BB", "heart attack") #invalid state
# best("NY", "hert attack")  #invalid outcome

## Quiz
# best("SC", "heart attack")
# best("NY", "pneumonia")
# best("AK", "pneumonia")
