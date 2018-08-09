pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    # set working directory
    if(directory == "specdata") {
        directory <- ("./specdata/")
    }
    
    # find all files in the specdata folder
    all_files <- as.character( list.files(directory) )
    file_paths <- paste(directory, all_files, sep="")
    
    pollutant_data <- c() ## empty vector to add pollutant values into
    
    for(i in id) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        na_removed <- current_file[!is.na(current_file[pollutant]), pollutant]
        pollutant_data <- c(pollutant_data, na_removed)
    }
    result <- mean(pollutant_data)
    return(result) 
}

## Results
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

