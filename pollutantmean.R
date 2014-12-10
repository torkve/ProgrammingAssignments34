pollutantmean <- function(directory, pollutant, id = 1:332) {
  filenames <- paste(directory, "/", formatC(id, width=3, flag="0"), ".csv", sep="")
  data <- lapply(filenames, read.csv)
  data <- do.call(rbind, data)
  data <- data[, pollutant, drop=TRUE]
  round(mean(data, na.rm=TRUE), 3)
}