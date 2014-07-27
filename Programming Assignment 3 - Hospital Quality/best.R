## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate
best <- function(state, outcome) {
  if (is.na(state) | is.null(state) | is.infinite(state) | !is.character(state)){
    stop("invalid state")
  }
  
  if (is.na(outcome) | is.null(outcome) | is.infinite(outcome) | !is.character(outcome)){
    stop("invalid outcome")
  }
  
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!any(validOutcomes == outcome)) {
    stop("invalid outcome")
  }
  
  csvFile <- read.csv("outcome-of-care-measures.csv", header=TRUE)
  ## csvFile[7]  State
  ## csvFile[15] Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  ## csvFile[21] Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  ## csvFile[27] Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  
  hospitalWithLowerMortality <- NA
  idxOutcome <- NA
  if (outcome == "heart attack") {
    idxOutcome <- 15
  } else if (outcome == "heart failure") {
    idxOutcome <- 21
  } else if (outcome == "pneumonia") {
    idxOutcome <- 27
  }
  
  if (is.na(idxOutcome)) {
    stop("invalid outcome")
  }
  
  idx <- which.min(csvFile[, idxOutcome])
  ## csvFile[2] "Hospital.Name"  
  csvFile[idx, 2]
}