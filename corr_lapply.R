corr <- function(directory, threshold = 0, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  path <- paste0(directory,"/",substr(id+1000,2,4),".csv")
  data <- lapply(path, read.csv, stringsAsFactors=FALSE)
  n.complete <- sapply(data, function(elt) sum(complete.cases(elt)))
  sapply(data[n.complete>=threshold], 
         function(elt) cor(elt$sulfate, elt$nitrate, use="complete.obs"))   

}