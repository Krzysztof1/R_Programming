corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  df = complete(directory)
  ids = df[df["nobs"] > threshold, ]$id
  corrr = numeric()
  for (i in ids) {
    
    path = read.csv(paste("C:/Chris/R/specdata/", formatC(i, width = 3,flag = "0"), 
                             ".csv", sep = ""))
    
    dff = path[complete.cases(path), ]
    corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
  
}
