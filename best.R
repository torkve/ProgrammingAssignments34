if (length(find.package("dplyr", quiet=TRUE)) == 0) {
  install.packages("dplyr")
}
library(dplyr)

readcsv <- function() {
  csv <- read.csv("outcome-of-care-measures.csv",
                  colClasses="character",
                  na.strings="Not Available")
  for (num in c(1, 11, 13:15, 17, 19:21, 23, 25:27, 29, 31:33, 35, 37:39, 41, 43:45)) {
    csv[, num] <- as.numeric(csv[, num])
  }
  tbl_df(csv)
}

coltail <- function(outcome) {
  if(outcome == "heart attack") {
    "Heart.Attack"
  } else if (outcome == "heart failure") {
    "Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Pneumonia"
  } else {
    stop("invalid outcome")
  }
}

best <- function(state, outcome) {
  csv <- readcsv()
  columnName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", coltail(outcome), sep="")
  csv <- filter(csv, State == state)
  if (nrow(csv) == 0) {
    stop("invalid state")
  }
  csv <- arrange_(csv, .dots=c(columnName, "Hospital.Name"))[1, "Hospital.Name"]
  as.character(csv)
}
