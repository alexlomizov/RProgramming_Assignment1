corr <- function(directory, threshold = 0, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  corrs <- numeric()
  for (i in id) {
    path.i <- paste0(directory,"/",substr(i+1000,2,4),".csv")
    data.i <- read.csv(path.i, colClasses=c("character", rep("numeric",3)))
    data.i.compl <- complete.cases(data.i)
    if (sum(data.i.compl)>=threshold) {
      corr.i <- cor(data.i[data.i.compl,2], data.i[data.i.compl,3])
      corrs <- c(corrs, corr.i)
    }
  }
  corrs
}