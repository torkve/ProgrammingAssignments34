corr <- function(directory, threshold = 0) {
  filenames <- paste(directory, "/", list.files(directory), sep="")
  data <- lapply(filenames, read.csv)
  data <- lapply(data, na.omit)
  result <- numeric(0)
  for (case in data) {
    if (nrow(case) >= threshold) {
      result <- append(result, cor(case$sulfate, case$nitrate))
    }
  }
  na.omit(result)
}
