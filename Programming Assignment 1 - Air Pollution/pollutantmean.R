## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used
pollutantmean <- function(directory, pollutant, id = 1:332) {
  valuesAccrosFiles <- c()
  for (i in id) {
    directory <- "specdata"
    fileName <- paste(formatC(as.numeric(i), format='f',width=3, digits=0, flag="0"), ".csv", sep = "")
    path <- paste(directory, fileName, sep = "/")
    
    csvFile <- read.csv(path, header=TRUE) 
    x <- subset(csvFile[pollutant])
    x <- x[!is.na(x)]
    valuesAccrosFiles <- c(valuesAccrosFiles, x)
  }
  meanAccrosFiles <- mean(valuesAccrosFiles)
  meanAccrosFiles
}
