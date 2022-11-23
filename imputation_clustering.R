#this is the k-prototype clustering to select asthma subtypes

library(tidyverse)
library(ggfortify)
library(ggplot2)
library("FactoMineR")
library("factoextra")
library("flexclust")
library(clustMixType)
library(Hmisc)
library (cluster)
library (vegan)
library(cclust)

#first define the data as the set of early or late onset asthma patients
data<-data3_1

data$Treatment.medication.code.0.0 <-as.factor(data$Treatment.medication.code.0.0)
data$Ethnic.background.0.0 <-as.factor(data$Ethnic.background.0.0)
data$Job.Type.0.0 <- as.factor(data$Job.Type.0.0)
data$Job.Type.0.1 <- as.factor(data$Job.Type.0.1)

#Do imputaion for missing values if not previouly imputed
data$Job.Type.0.0<-with(data,impute(data$Job.Type.0.0,"random"))
data$Job.Type.0.1<-with(data,impute(data$Job.Type.0.1,"random"))
data$Treatment.medication.code.0.0<-with(data,impute(data$Treatment.medication.code.0.0,"random"))

#data <- data[complete.cases(data),]

#use numerical features to test best k according to k-means
data_decide<- data %>% select_if(is.numeric)

# Elbow method
fviz_nbclust(data_decide, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(data_decide, kmeans, method = "silhouette") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Silhouette method")


# Silhouette method
silhouette_score <- function(k){
  km <- kmeans(data_decide, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(data_decide))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


#data<-data[,-c(31,56)]
#data <- data[complete.cases(data),]

#do k-prototype
kpres <- kproto(data,3)
table(kpres$cluster)
data$cluster <-kpres$cluster

#write cluster membership
write.csv(data,"3_1_with_cluster.csv")

#visulisation
clprofiles(kpres, data)


#k-means
data <- data %>% select_if(is.numeric)
fit<-kmeans(data_decide,3)

#plot
autoplot(kmeans(data_decide, 3),data=data_decide,label=TRUE, label.size=3, frame=TRUE)

#plot the barplot
clk2 <- cclust(data_decide,k=3)
barchart(clk2,legend=TRUE)

