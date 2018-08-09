corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    if(directory == "specdata") {
        directory <- ("./specdata/")
    }
    
    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep="")
    
    table <- complete("specdata")
    ids <- table$id[table$nobs > threshold]

    corr_vector <- c()
    
    for(i in ids) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        corr_vector <- c(corr_vector, cor(current_file$sulfate, current_file$nitrate, use="complete.obs"))
        
    }
    return(corr_vector)
}

## Results
cr <- corr("specdata")    
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
