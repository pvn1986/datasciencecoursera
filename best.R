best <- function(state, outcome) {
  ## Read outcome data
  hosp<-read.csv("outcome-of-care-measures.csv")
  s1 <- strsplit(outcome, " ")[[1]]
  s2 <- paste(toupper(substring(s1, 1,1)), substring(s1, 2),sep="", collapse=" ")
  
  outc<-paste("Hospital.30.Day.Death..Mortality..Rates.from.",gsub(" ",".",s2),sep="")

  ## Check that state and outcome are valid
  chk_st<-sum(hosp["State"]==state)
  chk_out<-sum(names(hosp)==outc)
  if(chk_st==0) stop("invalid state")
  if(chk_out==0) stop("invalid outcome")

  ## Return hospital name in that state with lowest 30-day death
  hosp1<-hosp[which(hosp["State"]==state & hosp[outc]!="Not Available"),]
  ##attach(hosp1)
  ##hosp2<-hosp1[order(hosp1[outc],hosp1["Hospital.Name"]),]
  ##hosp1a
  min_val<-min(as.numeric(as.character(unlist(hosp1[outc]))),na.rm=TRUE)
  hosp2<-subset(hosp1,as.numeric(as.character(unlist(hosp1[outc])))==min_val,2)
  as.vector(as.matrix(hosp2[1,1]))
  ## rate
}