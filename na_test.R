na_test <- function(filenameVct, directory="specdata") {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        if (!file.exists(directory)) {
                # directory argument is incorrect
                stop(paste("Error: Directory -", directory, "-name is incorrect"))                                
        }
        
        # 056, 085, 106

        for (filename in filenameVct) {
            fullfilename <- paste0(directory,"/",filename)
            tmpDF <- read.csv(fullfilename, header = TRUE)
            good_rows <- complete.cases(tmpDF)  # Vector of rows with good data
            goodDF <- tmpDF[good_rows,]
            if (is.na(goodDF[, "sulfate"]) != good_rows) print(paste(filename, "sulfate"))
            if (is.na(goodDF[, "nitrate"]) != good_rows) print(paste(filename, "nitrate"))
            result_cor <- cor(goodDF[, "sulfate"],goodDF[, "nitrate"])

        }


        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        goodDF
}
