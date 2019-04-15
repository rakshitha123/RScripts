categories <- c("C", "CE", "CEG", "CEGM", "CEM" ,"CG", "CGM", "CM", "E", "EG", "EGM", "EM", "G", "GM", "M")

results <- read.csv('C:/Projects/AODE/backups/tc2000_804414_iswd_b-AODE_B_T.csv')

#results <- read.csv('C:/Projects/Trevor/Article Classification/ty_thesis_support/javasrc/tc2000_804414_none_b-NaiveBayes.csv')


#findMax <- function(rowNo){
#  cols <- 16
#  max <- results[rowNo,2]
#  index <- 2
#  for(i in 3:16){
#   if(results[rowNo,i]>max){
#      max <- results[rowNo,i]
#      index <- i
#    }
#  }
#  index
#}

getAccuracyPercentage <- function(){
  count <- 0
  for(i in 1:nrow(results)){
    if(as.character(results[i,"Actual"])==as.character(results[i,"Predicted"])){
      count <- count+1
    }
  }
  count/nrow(results)
}

getHammingLoss <- function(){
  L <- 15
  diff <- 0
  for(doc in 1:nrow(results)){
    #if(as.character(results[doc,"Actual"])!=as.character(results[doc,"Predicted"])){
      actual <- c()
      predicted <- c()
      for(i in 1:nchar(as.character(results[doc,"Actual"]))){
        actual[i] <- substr(as.character(results[doc,"Actual"]), i, i)
      }
      for(i in 1:nchar(as.character(results[doc,"Predicted"]))){
        predicted[i] <- substr(as.character(results[doc,"Predicted"]), i, i)
      }
     
      if(!(identical(setdiff(actual,predicted),character(0)))){
        diff <- diff + length(setdiff(actual,predicted))
       
      }
      if(!(identical(setdiff(predicted,actual),character(0)))){
        diff <- diff + length(setdiff(predicted,actual))
      }
      
      diff<- diff/L
      
    #}
  }
  diff/nrow(results)
}



getAccuracy <- function(){
  val <- 0
  for(doc in 1:nrow(results)){
    
      actual <- c()
      predicted <- c()
      for(i in 1:nchar(as.character(results[doc,"Actual"]))){
        actual[i] <- substr(as.character(results[doc,"Actual"]), i, i)
      }
      for(i in 1:nchar(as.character(results[doc,"Predicted"]))){
        predicted[i] <- substr(as.character(results[doc,"Predicted"]), i, i)
      }
      
      anp <- intersect(actual,predicted)
      aop <- length(union(actual,predicted))
      
      if(identical(anp,character(0))){
        anp <- 0
      }
      else{
        anp <- length(anp)
      }
      
      val <- val+(anp/aop)
     
  }
  val/nrow(results)
}


getFScore <- function(){
  val <- 0
  for(doc in 1:nrow(results)){
   
    actual <- c()
    predicted <- c()
    for(i in 1:nchar(as.character(results[doc,"Actual"]))){
      actual[i] <- substr(as.character(results[doc,"Actual"]), i, i)
    }
    for(i in 1:nchar(as.character(results[doc,"Predicted"]))){
      predicted[i] <- substr(as.character(results[doc,"Predicted"]), i, i)
    }
    
    anp <- intersect(actual,predicted)
    
    if(identical(anp,character(0))){
      anp <- 0
    }
    else{
      anp <- length(anp)*2
    }
    
    val <- val+(anp/(length(actual) + length(predicted)))
  }
  val/nrow(results)
}


getLabels <- function(label){
  labelSet <- c()
  count <- 0
  for(i in 1:length(categories)){
    current <- categories[i]
    for(j in 1:nchar(current)){
      if(substr(current,j,j)==label){
        count <- count + 1
        labelSet[count] <- current
      }
    }
  }
  labelSet
}


getMse <- function(){
  labels <- c("C","E","G","M")
    error <- 0
  
  for(doc in 1:nrow(results)){
    actual <- c()
    for(i in 1:nchar(as.character(results[doc,"Actual"]))){
      actual[i] <- substr(as.character(results[doc,"Actual"]), i, i)
    }
    for(label in 1:length(labels)){
      requiredLabels <- getLabels(labels[label])
      prob <- 0
      for(j in 1:length(requiredLabels)){
        prob <- prob + results[doc,paste0("Pr",requiredLabels[j])]
      }
      
      belong <- 0
     
      if(labels[label] %in% actual){
        belong <- 1
      }
      
      error <- error + ((prob - belong)^2)
    }
  }
  
  error/(nrow(results)*length(labels))
}


print(paste0("MSE is ",getMse()))
print(paste0("RMSE is ",sqrt(getMse())))
print(paste0("F-1 Score is ",getFScore()))
print(paste0("Accuracy is ",getAccuracy()))
print(paste0("Hamming Loss is ",getHammingLoss()))
print(paste0("Accuracy Percentage is ",getAccuracyPercentage()*100,"%"))

