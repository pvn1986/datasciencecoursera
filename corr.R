corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files<-list.files(directory)
  cr<-c()
  nobs<-c()
  
  for(i in files){
    csv <- read.csv(paste(directory, "/", i, sep =""))
    
    nobs<-sum(as.numeric(complete.cases(csv)))
    
    sul<-csv[,2][!is.na(csv[2]) & !is.na(csv[3])]
    nit<-csv[,3][!is.na(csv[2]) & !is.na(csv[3])]
    
    if (nobs>threshold) cr<-c(cr,cor(sul,nit))
    else cr<-c(cr,numeric())
  }
  cr
}