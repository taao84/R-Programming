## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) {
  result <- c()
  resultIdx <- 0;
  
  filesInDir <- list.files(directory, pattern="*.csv", full.names=TRUE)
  for (path in filesInDir) {
    csvFile <- read.csv(path, header=TRUE)
    
    nobs <- sum(apply(csvFile, 1, function(x) !anyNA(x)))
    if (nobs >= threshold) {
      resultIdx <- resultIdx + 1
      result[resultIdx] <- cor(csvFile["sulfate"], csvFile["nitrate"], use="pairwise.complete.obs")
    }
  }
  result
}