consensus_xgboost <- function(range,ss) {
  
  # This function iterates through a range of cut-offs
  # and collects the XGBoost classification performance
  # range :: the range of ages to probe (e.g. range = 60, means 1-60)
  # ss : "serum" or "sputum"
  
  min_error<- asthma_clustering(ss,1)
  min_i <- 1
  error <- c()
  error  <- c(error,min_error)
  age <- c()
  age <- c(age,1)
  
  for (i in 2: range) {
    
    print(paste0("Trying Age = ", i))
    new_error <- asthma_clustering(ss,i)
    
   
    if (new_error < min_error) {
        
        min_i <- i
        min_error <- new_error
        
    }
    error <- c(error,new_error)
    age <- c(age,i)
    
    
  }
  g <- plot(age,error,type = "b", pch = 19, 
            col = "red", xlab = "Onset of Asthma (Age in years)", ylab = "XGBoost Classification error(%)")
  print(g)
  print("======================")
  print(error)
  print("======================")
  print(age)
  print("======================")
  return(min_i)
}