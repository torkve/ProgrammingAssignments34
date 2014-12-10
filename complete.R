complete <- function(directory, id = 1:332) {
  filenames <- paste(directory, "/", formatC(id, width=3, flag="0"), ".csv", sep="")
  data <- lapply(filenames, read.csv)
  data <- lapply(data, na.omit)
  data <- do.call(rbind, data)
  
  res <- tapply(rep(1, nrow(data)), data$ID, sum, simplify=FALSE)
  res <- data.frame(id=id, nobs=res[as.character(id)], row.names=NULL, check.names=FALSE)
  names(res) <- c("id", "nobs")
  res
}