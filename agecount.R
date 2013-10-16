agecount <- function(age = NULL) {
  if(is.null(age)) stop("Error")
  else {
    homicides <- readLines("homicides.txt")
    r <- regexec("<dd>.*? ([0-9][0-9]) years old",homicides)
    m <- regmatches(homicides,r)
    get.age <- sapply(m,function(x) x[2])
    test <- data.frame(table(get.age))
    num <- 0
    for(i in 1:nrow(test))
    {
      if(age==test$get.age[i])
        num <- test$Freq[i]
    }
    return(num)
  }
}
