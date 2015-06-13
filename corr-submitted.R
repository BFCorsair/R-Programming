corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        if (!file.exists(directory)) {
                # directory argument is incorrect
                stop(paste("Error: Directory -", directory, "-name is incorrect"))                                
        }
        filenameVct <- list.files(directory)
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        resultVct <- c()  # result vector
        for (filename in filenameVct) {
            fullfilename <- paste0(directory,"/",filename)
            tmpDF <- read.csv(fullfilename, header = TRUE)
            good_rows <- complete.cases(tmpDF)  # Vector of rows with good data
            goodDF <- tmpDF[good_rows,]
            # if (anyNA(goodDF[, "sulfate"])) print(paste(filename, "sulfate"))
            # if (anyNA(goodDF[, "nitrate"])) print(paste(filename, "nitrate"))
            if (nrow(goodDF) <= threshold) {  # have to use '<=' for case when threshold =0
            	# print(filename)
            	next # skip this file
            }
            result_cor <- cor(goodDF[, "sulfate"],goodDF[, "nitrate"])
            resultVct <- c(resultVct, result_cor)
        }


        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        resultVct
}
