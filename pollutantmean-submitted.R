pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        if (!file.exists(directory)) {
                # directory argument is incorrect
                stop(paste("Error: Directory -", directory, "-name is incorrect"))                                
        }

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        if (pollutant != "sulfate" && pollutant != "nitrate") {
                stop("Error: the 2nd argument must be equal to sulfate or nitrate")                
        }

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        filename_list = c()
        wrong_filename = c()
        file_error <- FALSE
        # Create the list of files - In the process, make sure they are all present
        for (i in id) {
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
        # print(paste("Read a total of",length(filename_list), "files"))

        # Read the files and extract the desired metric into resultVct
        row_cnt <- 0
        resultVct = c()
        for (filename in filename_list) {                
                # print(filename)
                tmpDF <- read.csv(filename, header = TRUE)
                tmp_row_cnt <- nrow(tmpDF)
                resultVct <- c(resultVct, tmpDF[,pollutant]) # append the desired column to resultVct
                row_cnt <- row_cnt + tmp_row_cnt
                # print(paste("file: ", filename, "has", tmp_row_cnt, "rows - total count is: ", row_cnt))
                # print(paste("length resultVct:", length(resultVct), sep=" ")) 
        }
        # print(paste("Read a total of",length(resultVct), "raw measurements"))


        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!


        # resultVct now contains all the data fron all the files
        mean_result <- mean(resultVct, na.rm=TRUE)
        # print(paste("Mean:", mean_result, sep=" ")) 
        mean_result       
}
