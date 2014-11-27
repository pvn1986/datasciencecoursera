pollutantmean <- function(directory, pollutant, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  value<-c()
  
  for (i in id){
    
    if (nchar(i)==1) fname<-paste(directory,"\\00",i,".csv",sep="")
    else if (nchar(i)==2) fname<-paste(directory,"\\0",i,".csv",sep="")
    else fname<-paste(directory,"\\",i,".csv",sep="")
    
    csv<-read.csv(fname)
    
    value<-append(value,csv[pollutant][!is.na(csv[pollutant])])
    
  }
  mean(value)
  ##nit<-csv_1$nitrate[!is.na(csv_1$nitrate)]
}