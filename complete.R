complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  iden<-id
  id<-c()
  nobs<-c()
  res<-rbind(id,nobs)
  
  for (i in iden){
    
    if (nchar(i)==1) fname<-paste(directory,"\\00",i,".csv",sep="")
    else if (nchar(i)==2) fname<-paste(directory,"\\0",i,".csv",sep="")
    else fname<-paste(directory,"\\",i,".csv",sep="")
    
    csv<-read.csv(fname)
    
    ##nobs<-length(csv[1][!is.na(csv["nitrate"]) & !is.na(csv["sulfate"])])
    nobs<-sum(as.numeric(complete.cases(csv)))
    
    res<-rbind(res,data.frame(id=i,nobs))  
    
  }
  
  res
  
}