
rankhospital <- function(x,y,t)
{
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- read.csv("hospital-data.csv", colClasses = "character")
  outcome.hospital <- merge(outcome, hospital, by = c("Provider.Number","State","Hospital.Name"))
  outcome.hospital[,11] <- as.numeric(outcome.hospital[,11])
  outcome.hospital[,17] <- as.numeric(outcome.hospital[,17])
  outcome.hospital[,23] <- as.numeric(outcome.hospital[,23])
  
  
  if (nrow(outcome.hospital[outcome.hospital$State==x,]))
  {
  if (y=="heart attack"&nrow(outcome.hospital[outcome.hospital$State==x,])) 
  { hospital.rate<-outcome.hospital[,c(3,11)][which(outcome.hospital$State==x),]
    hospital.rate <- hospital.rate[order(hospital.rate[,2],na.last=TRUE),]
    hospital.rate <-hospital.rate[order(hospital.rate[,2],hospital.rate[,1],na.last=TRUE),]
    hospital.rate <-hospital.rate[!is.na(hospital.rate[,2]),]
    hospital.rate$Rank <- 1:nrow(hospital.rate)
    
    if(t<=nrow(hospital.rate)|t=="worst"|t=="best"){
      if(t=="worst")
      {
        rank <- hospital.rate[which(hospital.rate[,3]==max(hospital.rate[,3],na.rm=TRUE)),]
        t <- rank[,1][1]
        return(t)
      }
      else if(t=="best")
      {
        rank <- hospital.rate[which(hospital.rate[,3]==min(hospital.rate[,3],na.rm=TRUE)),]
        t <- rank[,1][1]
        return(t)
      }
      else {
      rank <- hospital.rate[which(hospital.rate$Rank==t),]
      t <- rank[,1][1]
      return(t)
    }}
    else return("NA")
  }
  else if (y=="heart failure"&nrow(outcome.hospital[outcome.hospital$State==x,])) 
  {hospital.rate<-outcome.hospital[,c(3,17)][which(outcome.hospital$State==x),]
   hospital.rate <- hospital.rate[order(hospital.rate[,2],na.last=TRUE),]
   hospital.rate <-hospital.rate[order(hospital.rate[,2],hospital.rate[,1],na.last=TRUE),]
   hospital.rate <-hospital.rate[!is.na(hospital.rate[,2]),]
   hospital.rate$Rank <- 1:nrow(hospital.rate)
   
   
   if(t<=nrow(hospital.rate)|t=="worst"|t=="best"){
     if(t=="worst")
     {
       rank <- hospital.rate[which(hospital.rate[,3]==max(hospital.rate[,3],na.rm=TRUE)),]
       t <- rank[,1][1]
       return(t)
     }
     else if(t=="best")
     {
       rank <- hospital.rate[which(hospital.rate[,3]==min(hospital.rate[,3],na.rm=TRUE)),]
       t <- rank[,1][1]
       return(t)
     }
     else if(t){
     rank <- hospital.rate[which(hospital.rate$Rank==t),]
     t <- rank[,1][1]
     return(t)
   }}
   else return("NA")
  
  }
  else if (y=="pneumonia"&nrow(outcome.hospital[outcome.hospital$State==x,])) 
   
  {hospital.rate<-outcome.hospital[,c(3,23)][which(outcome.hospital$State==x),]
   hospital.rate <- hospital.rate[order(hospital.rate[,2],na.last=TRUE),]
   
   
   hospital.rate <-hospital.rate[order(hospital.rate[,2],hospital.rate[,1],na.last=TRUE),]
   hospital.rate <-hospital.rate[!is.na(hospital.rate[,2]),]
   hospital.rate$Rank <- 1:nrow(hospital.rate)
   
   if(t<=nrow(hospital.rate)|t=="worst"|t=="best"){
   if(t=="worst")
   { 
     rank <- hospital.rate[which(hospital.rate[,3]==max(hospital.rate[,3],na.rm=TRUE)),]
     t <- rank[,1][1]
     return(t)
   }
   else if(t=="best")
   {
     rank <- hospital.rate[which(hospital.rate[,3]==min(hospital.rate[,3],na.rm=TRUE)),]
     t <- rank[,1][1]
     return(t)
   }
   else {
   rank <- hospital.rate[which(hospital.rate$Rank==t),]
   t <- rank[,1][1]
   return(t)
  }}
   else return("NA")
  }
   else stop("invalid outcome")
  
  }
  else stop("invalid state")
}
