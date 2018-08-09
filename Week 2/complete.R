complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    if(directory == "specdata") {
        directory <- ("./specdata/")
    }
    
    all_files <- as.character( list.files(directory) )
    file_paths <- paste(directory, all_files, sep="")
    
    id_vector <- vector("numeric", length = length(id)) ## empty vector for id 
    nobs_vector <- c()                                  ## empty vector to nobs
    
    for (i in id) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        nobs_vector <- c(nobs_vector,sum(complete.cases(current_file)))
    }
    
    result <- data.frame(id = id, nobs = nobs_vector)
    return(result)
}

## Results
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
