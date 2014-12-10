source("best.R")

nth.outcome <- function(hospitals, num) {
  n <- nrow(hospitals)
  rowIndex <- if(num == "best") {
    1
  } else if (num == "worst") {
    n
  } else if (num > n) {
    return(data.frame(State=hospitals[1, "State"]))
  } else {
    num
  }
  data.frame(Hospital.Name=hospitals[rowIndex, "Hospital.Name"], State=hospitals[1, "State"])
}

rankall <- function(outcome, num = "best") {
  csv <- readcsv()
  columnName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", coltail(outcome), sep="")
  csv <- csv %>% select_(.dots=c("State", "Hospital.Name", columnName)) %>%
    arrange_(.dots=c("State", columnName, "Hospital.Name")) %>%
    group_by(State) %>%
    do(nth.outcome(., num)) %>%
    ungroup %>%
    transmute(hospital=Hospital.Name, state=State) %>%
    as.data.frame
  rownames(csv) <- csv$state
  csv
}