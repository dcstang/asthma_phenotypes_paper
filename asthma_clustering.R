asthma_clustering <- function(ss,cutoff)  {

  # This function performs XGBoost classification based on 
  # the labels allocated to the samples due to a cut-off in age
  # ss : "serum" or "sputum"
  # cutoff : the number of age to cut-off for child/adult
  
  wd <- getwd()
  library(stringr)
  library(dplyr)
  library(dplyr)
  library(ggplot2)
  library(randomForest)
  library(varImp)
  library(ranger)
  library(janitor)
  library(Boruta)
  library(datasets)
  library(caret)
  library(glmnet)
  library(factoextra)
  library(stats)
  library(useful)
  library(clValid)
  library(ggfortify)
  library(kableExtra)
  library(data.table)
  library(mlr)
  set.seed(41)
  print(wd)
  
  pdf("RESULTS_UBIOPRED.pdf", 
      width = 12, height = 10, 
      bg = "white",    
      colormodel = "cmyk") 

  inputs_dir <- paste0(wd,"/Asthma_data/inputs")
  load(paste0(inputs_dir,"/Asthma_dataset.RData"))
  
  EM_serum_bkp <- EM_serum1
  EM_sputum_bkp <- EM_sputum1

  library(tibble)
  
  EM_severity_sputum  <- tibble::rownames_to_column(EM_sputum , "Patient")
  colnames(EM_severity_sputum) <- NULL
  colnames(EM_severity_sputum) <- EM_severity_sputum[1,]
  colnames(EM_severity_sputum)[1] <- "Patient"
  EM_severity_sputum <- EM_severity_sputum[-1,]
  
  EM_severity_serum  <- tibble::rownames_to_column(EM_serum , "Patient")
  colnames(EM_severity_serum) <- NULL
  colnames(EM_severity_serum) <- EM_severity_serum[1,]
  colnames(EM_severity_serum)[1] <- "Patient"
  EM_severity_serum <- EM_severity_serum[-1,]
  
  
  
  Clinical_unity2 <- Clinical %>% select(Patient, cohort)
  colnames(Clinical_unity2) <- c("Patient","cluster")
  
  Clinical_unity <- as.data.frame(read.csv(paste0(inputs_dir,"/UBIOPRED_AGE.csv")),header= TRUE, rownames =  FALSE)
  Clinical_unity <- Clinical_unity %>% dplyr::select(c("ID", "Onset"))
  colnames(Clinical_unity) <- c("Patient", "cluster")
  Clinical_unity <- Clinical_unity %>% dplyr::mutate(cluster = case_when( cluster <= cutoff ~ 0,
                                                                          cluster > cutoff  ~ 1))
  
  centers_user <- length(unique(Clinical_unity$cluster))
  
  EM_severity_sputum$Patient <- rownames(EM_sputum)[2:nrow(EM_sputum)]
  EM_severity_sputum <- merge(Clinical_unity,EM_severity_sputum,by = "Patient" ,all = FALSE)
  EM_severity_sputum$cluster <-  as.factor(EM_severity_sputum$cluster)
  saveRDS(EM_severity_sputum, file = "EM_severity_sputum.rds")
  
  EM_severity_serum$Patient <- rownames(EM_serum)[2:nrow(EM_serum)]
  EM_severity_serum <- merge(Clinical_unity,EM_severity_serum,by = "Patient" ,all = FALSE)
  EM_severity_serum$cluster <-  as.factor(EM_severity_serum$cluster)
  saveRDS(EM_severity_serum, file = "EM_severity_serum.rds")
  
  
  fc <- tapply(EM_severity_sputum$Patient, EM_severity_sputum$cluster, c)
  fc2 <- tapply(EM_severity_serum$Patient, EM_severity_serum$cluster, c)
  
  if (ss == "serum") {
    clustering_data <- EM_severity_serum 
    
  }
  else {
    clustering_data <- EM_severity_sputum
    
  }
  
  
  clustering_data <- clustering_data %>% dplyr::select(-Patient)
  saveRDS(clustering_data, file = "to_cluster.rds")
  
  rownames(EM_severity_serum) <- EM_severity_serum$Patient
  EM_severity_serum <- EM_severity_serum %>% dplyr::select(-Patient)
  EM_severity_serum <- EM_severity_serum %>% dplyr::select(-cluster)
  EM_severity_serum <- scale(data.matrix(EM_severity_serum))
  
  rownames(EM_severity_sputum) <- EM_severity_sputum$Patient
  EM_severity_sputum <- EM_severity_sputum %>% dplyr::select(-Patient)
  EM_severity_sputum <- EM_severity_sputum %>% dplyr::select(-cluster)
  EM_severity_sputum <- scale(data.matrix(EM_severity_sputum))
  
  if (ss == "serum") {
    dataX <- EM_severity_serum 
    fcA <- fc2
  }
  else {
    dataX <- EM_severity_sputum
    fcA <- fc
  }
  print(fcA)
  
  library(xgboost)
  print("XGboost training...")
  labels <- clustering_data$cluster
  training.samples <- clustering_data$cluster %>%
    createDataPartition(p = 0.7, list = FALSE)
  
  train.data  <- clustering_data[training.samples, ]
  test.data <- clustering_data[-training.samples, ]
  
  print(table(train.data$cluster))
  print(table(test.data$cluster))
  Sys.sleep(4)
  
  train.data <- train.data %>% select(-cluster)
  test.data <- test.data %>% select(-cluster)
  
  labels <- labels[training.samples]
  
  labels <- as.numeric(labels)
  labels <- labels - 1;
  
  
  bstSparse <- xgboost(data = data.matrix(train.data), label=labels, booster = "gbtree",
                       nthread = 8, max_depth = 15,nrounds  = 100,  num_class = centers_user, objective = "multi:softmax")
  
  print(bstSparse)
  
  importance_matrix <- xgb.importance(model = bstSparse)
  print(importance_matrix)
  xgb.plot.importance(importance_matrix = importance_matrix)
  pred <- predict(bstSparse, data.matrix(test.data))
  
  print(pred)
  print("-----------------------------")
  labels <- as.numeric(clustering_data$cluster)
  labels <- labels[-training.samples]
  labels <- as.numeric(labels)
  labels <- labels - 1;
  print(labels)
  err <- mean(pred != labels)
  print(paste("test-error=", err))
  
  e <- data.frame(bstSparse$evaluation_log)
  plot(e$iter, e$train_mlogloss, col = 'blue')
  lines(e$iter, e$test_mlogloss, col = "red")
  
  
  dev.off()
  return(err)
}