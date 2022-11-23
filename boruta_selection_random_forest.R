testdata <- readRDS("ih_debugged_df.RDS")
# testdata <- testdata %>% mutate (asthma_outcome = if_else (testdata$Age.asthma.diagnosed.by.doctor1.0.0 <= 18, 'child', 'adult'))

testdata$Body.mass.index..BMI.1.0.0  <-as.numeric(testdata$Body.mass.index..BMI.1.0.0)
testdata$Nitrogen.dioxide.air.pollution..20101.0.0  <-as.numeric(testdata$Nitrogen.dioxide.air.pollution..20101.0.0)
testdata$Nitrogen.oxides.air.pollution..20101.0.0  <-as.numeric(testdata$Nitrogen.oxides.air.pollution..20101.0.0)
testdata$Particulate.matter.air.pollution..pm10...20101.0.0  <-as.numeric(testdata$Particulate.matter.air.pollution..pm10...20101.0.0)
testdata$Particulate.matter.air.pollution..pm2.5...20101.0.0  <-as.numeric(testdata$Particulate.matter.air.pollution..pm2.5...20101.0.0)
testdata$Particulate.matter.air.pollution..pm2.5..absorbance..20101.0.0  <-as.numeric(testdata$Particulate.matter.air.pollution..pm2.5..absorbance..20101.0.0)
testdata$Particulate.matter.air.pollution.2.5.10um..20101.0.0  <-as.numeric(testdata$Particulate.matter.air.pollution.2.5.10um..20101.0.0)
# testdata$Traffic.intensity.on.the.nearest.road1.0.0  <-as.numeric(testdata$Traffic.intensity.on.the.nearest.road1.0.0)
testdata$Inverse.distance.to.the.nearest.road1.0.0  <-as.numeric(testdata$Inverse.distance.to.the.nearest.road1.0.0)
testdata$Traffic.intensity.on.the.nearest.major.road1.0.0  <-as.numeric(testdata$Traffic.intensity.on.the.nearest.major.road1.0.0)
testdata$Inverse.distance.to.the.nearest.major.road1.0.0  <-as.numeric(testdata$Inverse.distance.to.the.nearest.major.road1.0.0)
# testdata$Total.traffic.load.on.major.roads1.0.0  <-as.numeric(testdata$Total.traffic.load.on.major.roads1.0.0)
# testdata$Sum.of.road.length.of.major.roads.within.100m1.0.0  <-as.numeric(testdata$Sum.of.road.length.of.major.roads.within.100m1.0.0)
testdata$Nitrogen.dioxide.air.pollution..20051.0.0  <-as.numeric(testdata$Nitrogen.dioxide.air.pollution..20051.0.0)
testdata$Nitrogen.dioxide.air.pollution..20061.0.0  <-as.numeric(testdata$Nitrogen.dioxide.air.pollution..20061.0.0)
testdata$Nitrogen.dioxide.air.pollution..20071.0.0  <-as.numeric(testdata$Nitrogen.dioxide.air.pollution..20071.0.0)
testdata$Particulate.matter.air.pollution..pm10...20071.0.0  <-as.numeric(testdata$Particulate.matter.air.pollution..pm10...20071.0.0)
testdata$Natural.environment.percentage..buffer.1000m1.0.0  <-as.numeric(testdata$Natural.environment.percentage..buffer.1000m1.0.0)
testdata$Natural.environment.percentage..buffer.300m1.0.0  <-as.numeric(testdata$Natural.environment.percentage..buffer.300m1.0.0)
testdata$Forced.expiratory.volume.in.1.second..FEV1...Best.measure1.0.0  <-as.numeric(testdata$Forced.expiratory.volume.in.1.second..FEV1...Best.measure1.0.0)
testdata$Forced.expiratory.volume.in.1.second..FEV1..Z.score1.0.0  <-as.numeric(testdata$Forced.expiratory.volume.in.1.second..FEV1..Z.score1.0.0)
testdata$FEV1..FVC.ratio.Z.score1.0.0  <-as.numeric(testdata$FEV1..FVC.ratio.Z.score1.0.0)
testdata$Underlying..primary..cause.of.death..ICD101.0.0  <-as.numeric(testdata$Underlying..primary..cause.of.death..ICD101.0.0)
testdata$Contributory..secondary..causes.of.death..ICD101.0.1  <-as.numeric(testdata$Contributory..secondary..causes.of.death..ICD101.0.1)
testdata$Neutrophill.count1.0.0  <-as.numeric(testdata$Neutrophill.count1.0.0)
testdata$Eosinophill.count1.0.0  <-as.numeric(testdata$Eosinophill.count1.0.0)
testdata$Townsend.deprivation.index.at.recruitment1.0.0  <-as.numeric(testdata$Townsend.deprivation.index.at.recruitment1.0.0)
testdata$Length.of.time.at.current.address1.0.0  <-as.numeric(testdata$Length.of.time.at.current.address1.0.0)

testdata[sapply(testdata, is.character)] <- lapply(testdata[sapply(testdata, is.character)], 
                                                   as.factor)
testdata <- testdata %>% select(-X)
testdata <- testdata %>% select(-Age.asthma.diagnosed.by.doctor1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.asbestosis1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.bronchiectasis1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.chronic.bronchitis1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.COPD..chronic.obstructive.pulmonary.disease.1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.emphysema1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.fibrosing.alveolitis.unspecified.alveolitis1.0.0)
# testdata <- testdata %>% select(-Doctor.diagnosed.hayfever.or.allergic.rhinitis1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.idiopathic.pulmonary.fibrosis1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.lung.cancer..not.mesothelioma.1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.sarcoidosis1.0.0)
testdata <- testdata %>% select(-Doctor.diagnosed.tuberculosis1.0.0)
testdata <- testdata %>% select(-Illnesses.of.father1.0.1)
testdata <- testdata %>% select(-Illnesses.of.mother1.0.1)
testdata <- testdata %>% select(-Current.tobacco.smoking1.0.0)

testdata1 <- subset(testdata, select = c(1:10, 107))
testdata0 <- subset(testdata, select = c("Past.tobacco.smoking1.0.0","Smoking.status1.0.0","Sex1.0.0","Genetic.sex1.0.0","Body.mass.index..BMI.1.0.0", 
                                         "Nitrogen.dioxide.air.pollution..20101.0.0","Nitrogen.oxides.air.pollution..20101.0.0","Particulate.matter.air.pollution..pm10...20101.0.0", 
                                         "Particulate.matter.air.pollution..pm2.5...20101.0.0","Particulate.matter.air.pollution..pm2.5..absorbance..20101.0.0", 
                                         "Particulate.matter.air.pollution.2.5.10um..20101.0.0","Inverse.distance.to.the.nearest.major.road1.0.0",
                                         "Nitrogen.dioxide.air.pollution..20051.0.0","Nitrogen.dioxide.air.pollution..20061.0.0", 
                                         "Nitrogen.dioxide.air.pollution..20071.0.0","Particulate.matter.air.pollution..pm10...20071.0.0", 
                                         "Natural.environment.percentage..buffer.1000m1.0.0","Natural.environment.percentage..buffer.300m1.0.0", 
                                         "Forced.expiratory.volume.in.1.second..FEV1...Best.measure1.0.0", "Forced.expiratory.volume.in.1.second..FEV1..Z.score1.0.0",
                                         "FEV1..FVC.ratio.Z.score1.0.0","Recent.medication.for.asthma1.0.0","Doctor.diagnosed.hayfever.or.allergic.rhinitis1.0.0", "Current.employment.status1.0.0",
                                         "Number.in.household1.0.0","onset"))

## Feature selection using the Boruta package
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

set.seed(111)
boruta <- Boruta(onset ~ ., data = testdata, doTrace = 2, maxRuns = 1000)
print(boruta)

plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)
bor <- TentativeRoughFix(boruta)
print(bor)

set.seed(222)
ind <- sample(2, nrow(testdata), replace = T, prob = c(0.6, 0.4))
train <- testdata[ind==1,]
test <- testdata[ind==2,]

set.seed(333) 
rf60 <- randomForest(Cluster~., data = train) 
p <- predict(rf60, test, type="prob")
plot.roc(test$Cluster, p[,2], percent=TRUE, lwd=4, col="blue", print.auc=TRUE)
confusionMatrix(p, test$Cluster)

getNonRejectedFormula(boruta)
p <- predict(rf60, test)
confusionMatrix(p, test$Cluster)
getConfirmedFormula(boruta)
attStats(boruta)
attStats(bor)

## Random forest
library(ggplot2)
library(cowplot)
library(randomForest)
library(caret)

# Split the data into training and testing
set.seed(100)
train <- sample(nrow(testdata), 0.7*nrow(testdata), replace = FALSE)
TrainSet <- testdata[train,]
ValidSet <- testdata[-train,]
summary(TrainSet)
summary(ValidSet)

# Create a Random Forest model with default parameters
model1 <- randomForest(onset ~ ., data = TrainSet, importance=TRUE)
model1
confusionMatrix(table(model1$predicted, TrainSet$onset))

# Fine tuning parameters of Random Forest model
model2 <- randomForest(onset ~ ., data = TrainSet, ntree = 1000, mtry = 3, importance = TRUE)
model2
confusionMatrix(table(model2$predicted, TrainSet$onset))

# Plot ROC for training data
plot.roc(TrainSet$onset, model1$votes[,2], percent=TRUE, lwd=4, col="blue", print.auc=TRUE)

# Predicting on Validation set and Checking classification accuracy
predValid <- predict(model1, ValidSet, type = "class")
mean(predValid == ValidSet$onset)                    
confusionMatrix(table(predValid,ValidSet$onset))

# Plot ROC for testing data
predValid <- predict(model1, ValidSet, type = "prob")
plot.roc(ValidSet$onset, predValid[,2], percent=TRUE, lwd=4, col="blue", print.auc=TRUE)

# To check important variables
importance(model1)        
varImpPlot(model1,sort=TRUE,n.var=5)        

# Using For loop to identify the right ntree and mtry for model
set.seed(666)
a=c()
i=5
for (i in 3:8) {
  model <- randomForest(onset ~ ., data = TrainSet, ntree = 1000, mtry = i, importance = TRUE)
  predValid <- predict(model, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$onset)
}
a
plot(3:8, a)

set.seed(666) 
model3 <- randomForest(onset ~ ., data = TrainSet, ntree = 1000, mtry = 4, importance = TRUE)
model3
predTrain3 <- predict(model3, TrainSet, type = "class")
table(predTrain3, TrainSet$onset)  
predValid3 <- predict(model3, ValidSet, type = "prob")
plot.roc(ValidSet$onset, predValid3[,2], percent=TRUE, lwd=4, col="pink", print.auc=TRUE)


