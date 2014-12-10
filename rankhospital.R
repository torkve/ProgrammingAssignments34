source("best.R")
library(lazyeval)

rankhospital <- function(state, outcome, num = "best") {
  csv <- readcsv()
  columnName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", coltail(outcome), sep="")
  csv <- csv %>% filter_(interp(~ State == state & !is.na(col), col=as.name(columnName))) %>%
    select_(.dots=c("Hospital.Name", columnName))
  if (nrow(csv) == 0) {
    stop("invalid state")
  }
  csv <- arrange_(csv, .dots=c(columnName, "Hospital.Name"))
  rowIndex <- if(num == "best") {
    1
  } else if (num == "worst") {
    nrow(csv)
  } else if (num > nrow(csv)) {
    return(NA)
  } else {
    num
  }
  as.character(csv[rowIndex, "Hospital.Name"])
}