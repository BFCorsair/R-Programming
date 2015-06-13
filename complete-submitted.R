complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        if (!file.exists(directory)) {
        # directory argument is incorrect
        stop(paste("Error: Directory -", directory, "-name is incorrect"))                                
        }

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        filename_list = c()
        id_list = c()
        wrong_filename = c()
        file_error <- FALSE
        # Create the list of files - In the process, make sure they are all present
        for (i in id) {
                id_list <- c(id_list, i)  # create a list of indexes
                if (i <10) {
                        prefix = "00"
                } else if (i <100) {
                        prefix = "0"
                } else if (i>332) {
                        stop("Error: file index cannot be greater than 332")                        
                } else {
                        prefix = ""
                }
                filename <- paste(directory,"/",prefix,i,".csv",sep="")
                if (!file.exists(filename)) {
                        # file is missing
                        file_error <- TRUE
                        wrong_filename <- append(wrong_filename, filename)
                }
                filename_list <- append(filename_list,filename)
        }
        if (file_error) {
                # there are missing files
                print("Could not find the following files:")
                for (f in wrong_filename) print(f)
                stop("Error: some files are missing")                      
        }
        nb_file <- length(filename_list)
        filename_list

        # Make a 2-col matrix
        # Col1: index
        # Col2: filename
        file_mx <- cbind(id_list, filename_list)
       
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

        # Create the results matrix
        resultDF <- data.frame("id"=numeric(0), "nobs"=numeric(0))
        for (i in 1:nb_file) {
                resultDF[i,1] <- file_mx[i,1]
                tmpDF <- read.csv(file_mx[i,2], header = TRUE)
                good_rows <- complete.cases(tmpDF)  # Vector of rows with good data
                tmpDF2 <- tmpDF[good_rows,] # DF w/ only good data
                resultDF[i,2] <- nrow(tmpDF2)
                # resultDF[i,2] <- nrow(complete.cases(tmpDF))
        }
        # print.data.frame(resultDF)
        resultDF
}