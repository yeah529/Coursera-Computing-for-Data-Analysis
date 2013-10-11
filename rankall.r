rankall <- function(y,t="best") {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- read.csv("hospital-data.csv", colClasses = "character")
  outcome.hospital <- merge(outcome, hospital, by = c("Provider.Number","State","Hospital.Name"))
  outcome.hospital[,11] <- as.numeric(outcome.hospital[,11])
  outcome.hospital[,17] <- as.numeric(outcome.hospital[,17])
  outcome.hospital[,23] <- as.numeric(outcome.hospital[,23])
  hospital <- rep(NA,54)
  state <- rep(NA,54)
  statenames <- unique(outcome.hospital$State)
  statenames <- statenames[order(statenames)]
   
  if(y=="heart attack"){
    for(i in 1:54){
      if(t!="worst"&t!="best"&t>nrow(outcome.hospital[outcome.hospital$State==statenames[i],])) {
        state[i]<-statenames[i] 
        hospital[i]<-NA}
      else if(t=="worst"|t=="best"|t<=nrow(outcome.hospital[outcome.hospital$State==statenames[i],])){
    hospital.rate<-outcome.hospital[,c(3,2,11)][which(statenames[i]==outcome.hospital$State),]
    hospital.rate <- hospital.rate[order(hospital.rate[,3],na.last=TRUE),]
    hospital.rate <-hospital.rate[order(hospital.rate[,3],hospital.rate[,1],na.last=TRUE),]
    hospital.rate <-hospital.rate[!is.na(hospital.rate[,3]),]
    hospital.rate$Rank <- 1:nrow(hospital.rate)
    
    if(t=="worst"){
      
      rank <- hospital.rate[which(hospital.rate[,4]==max(hospital.rate[,4])),]
      hospital[i]<-rank[1,1]
      state[i] <- rank[1,2]
    }
    else if(t=="best"){
      
      rank <- hospital.rate[which(hospital.rate[,4]==min(hospital.rate[,4])),]
      hospital[i]<-rank[1,1]
      state[i] <- rank[1,2]
      
    }
    else if(t!="worst"&t!="best"){
      rank <- hospital.rate[which(hospital.rate[,4]==t),]
      hospital[i]<-rank[1,1]
      state[i]<-rank[1,2]
    }
    
    }
  }
  return(data.frame(cbind(hospital,state)))
  }
  
  else if(y=="heart failure"){
    for(i in 1:54){
      if(t!="worst"&t!="best"&t>nrow(outcome.hospital[outcome.hospital$State==statenames[i],])) {
        state[i]<-statenames[i] 
        hospital[i]<-NA}
      else if(t=="worst"|t=="best"|t<=nrow(outcome.hospital[outcome.hospital$State==statenames[i],])){
      hospital.rate<-outcome.hospital[,c(3,2,17)][which(statenames[i]==outcome.hospital$State),]
      hospital.rate <- hospital.rate[order(hospital.rate[,3],na.last=TRUE),]
      hospital.rate <-hospital.rate[order(hospital.rate[,3],hospital.rate[,1],na.last=TRUE),]
      hospital.rate <-hospital.rate[!is.na(hospital.rate[,3]),]
      hospital.rate$Rank <- 1:nrow(hospital.rate)
      
      if(t=="worst"){
        
        rank <- hospital.rate[which(hospital.rate[,4]==max(hospital.rate[,4])),]
        hospital[i]<-rank[1,1]
        state[i] <- rank[1,2]
      }
      else if(t=="best"){
        
        rank <- hospital.rate[which(hospital.rate[,4]==min(hospital.rate[,4])),]
        hospital[i]<-rank[1,1]
        state[i] <- rank[1,2]
        
      }
      else if(t!="worst"&t!="best"){
        rank <- hospital.rate[which(hospital.rate[,4]==t),]
        hospital[i]<-rank[1,1]
        state[i]<-rank[1,2]
      }
      
    }
  }
    return(data.frame(cbind(hospital,state)))
  }
  else if(y=="pneumonia"){
    for(i in 1:54){
      if(t!="worst"&t!="best"&t>nrow(outcome.hospital[outcome.hospital$State==statenames[i],])) {
        state[i]<-statenames[i] 
        hospital[i]<-NA}
      else if(t=="worst"|t=="best"|t<=nrow(outcome.hospital[outcome.hospital$State==statenames[i],])){
      hospital.rate<-outcome.hospital[,c(3,2,23)][which(statenames[i]==outcome.hospital$State),]
      hospital.rate <- hospital.rate[order(hospital.rate[,3],na.last=TRUE),]
      hospital.rate <-hospital.rate[order(hospital.rate[,3],hospital.rate[,1],na.last=TRUE),]
      hospital.rate <-hospital.rate[!is.na(hospital.rate[,3]),]
      hospital.rate$Rank <- 1:nrow(hospital.rate)
      
      if(t=="worst"){
        
        rank <- hospital.rate[which(hospital.rate[,4]==max(hospital.rate[,4])),]
        hospital[i]<-rank[1,1]
        state[i] <- rank[1,2]
      }
      else if(t=="best"){
        
        rank <- hospital.rate[which(hospital.rate[,4]==min(hospital.rate[,4])),]
        hospital[i]<-rank[1,1]
        state[i] <- rank[1,2]
        
      }
      else if(t!="worst"&t!="best"){
        rank <- hospital.rate[which(hospital.rate[,4]==t),]
        hospital[i]<-rank[1,1]
        state[i]<-rank[1,2]
      }
      
    }
  }
    return(data.frame(cbind(hospital,state)))
  }
  else stop("invalid outcome")
  
  
}
