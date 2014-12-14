pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  data <- numeric()
  for (i in id) {
    pathi <- paste0(directory,"/",substr(i+1000,2,4),".csv")
    datai <- read.csv(pathi, colClasses=c("character", rep("numeric",3)))
    if (pollutant=="sulfate") data <- c(data, datai[!is.na(datai[,2]),2])
    if (pollutant=="nitrate") data <- c(data, datai[!is.na(datai[,3]),3])
  }
  data.mean <- mean(data)
  data.mean
}