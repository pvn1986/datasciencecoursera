rankall <- function(outcome,num="best") {
  ## Read outcome data
  hosp<-read.csv("outcome-of-care-measures.csv")
  s1 <- strsplit(outcome, " ")[[1]]
  s2 <- paste(toupper(substring(s1, 1,1)), substring(s1, 2),sep="", collapse=" ")
  
  
  outc<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",gsub(" ",".",s2),sep="")
  
  ## Check that state and outcome are valid
  chk_out<-sum(names(hosp)==outc)
  if(chk_out==0) stop("invalid outcome")
  
  ## Return hospital name in that state with the given rank
  subset <- split(hosp,hosp["State"])
  
  hosp3<-lapply(subset,function(subset,oc=outc,nm=num)
    {
    hosp1<-subset[which(subset[oc]!="Not Available"),]  
    hosp2<-hosp1[order(as.numeric(as.character(unlist(hosp1[outc]))),hosp1["Hospital.Name"]),c("Hospital.Name")]
    
    if (nm=="best") rnk=1
    else if (nm=="worst") rnk=nrow(as.matrix(hosp2))
    else rnk=nm

    as.vector(as.matrix(hosp2[rnk],names(hosp[2])))
  }
  )
  hosp4<-cbind(hosp3,names(hosp3))
  colnames(hosp4)<-c("hospital","state")
  as.data.frame(hosp4)  
  ## rate
}