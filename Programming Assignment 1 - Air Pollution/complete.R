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
complete <- function(directory, id = 1:332) {
  valuesAccrosFiles <- data.frame(rep(0, length(id)), rep(0, length(id)))
  idxValuesAcrossFiles <- 1
  
  for (i in id) {
    fileName <- paste(formatC(as.numeric(i), format='f',width=3, digits=0, flag="0"), ".csv", sep = "")
    path <- paste(directory, fileName, sep = "/")
    
    csvFile <- read.csv(path, header=TRUE)
    nobs <- sum(apply(csvFile, 1, function(x) !anyNA(x)))
    
    valuesAccrosFiles[idxValuesAcrossFiles, 2] <- nobs
    idxValuesAcrossFiles <- idxValuesAcrossFiles + 1
  }
  valuesAccrosFiles[, 1] <- id
  colnames(valuesAccrosFiles) <- c("id", "nobs")
  valuesAccrosFiles
}