count <- function(cause = NULL) {
  
  if(is.null(cause)) stop("Error")
  else if(cause!="asphyxiation"&cause!="blunt force"&cause!="other"&cause!="shooting"&cause!="stabbing"
          &cause!="unknown")
    stop("Error")
  else {
    homicides <- readLines("homicides.txt")
    r <- regexec("<dd>[Cc]ause: (.*?)</dd>",homicides)
    m <- regmatches(homicides,r)
    reasons <- sapply(m,function(x) x[2])
    test <- data.frame(table(reasons))
    
    if(cause=="asphyxiation")
      return(test$Freq[test[,1]=="asphyxiation"]+test$Freq[test[,1]=="Asphyxiation"])
    else if(cause=="blunt force")
      return(test$Freq[test[,1]=="blunt force"]+test$Freq[test[,1]=="Blunt Force"])
    else if(cause=="shooting")
      return(test$Freq[test[,1]=="shooting"]+test$Freq[test[,1]=="Shooting"])
    else if(cause=="stabbing")
      return(test$Freq[test[,1]=="stabbing"]+test$Freq[test[,1]=="Stabbing"])
    else if(cause=="unknown")
      return(test$Freq[test[,1]=="unknown"]+test$Freq[test[,1]=="Unknown"])
    else if(cause=="other")
      return(test$Freq[test[,1]=="Other"])
    }
  }
  




