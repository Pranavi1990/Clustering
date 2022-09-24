rm(list = ls())
library(tidyverse)# Data Processing
library(cluster)# Clustering
library(factoextra)# Cluster Visualization
library(gridExtra)# Cluster Visualization
#SGN


##############################################################################################
###### Build an analytical model to create clusters of airline travellers ####################
##############################################################################################

#---------------------Step1: Loading the Data in R-----------------------------
Path<-setwd("E:/Ivyproschool/R/Clustering/Mall Case Study")
mall_customers<-read.csv("Mall_Customers.csv",header = TRUE,as.is = TRUE,na.strings = c(""))
data<-mall_customers
#------------Basic exploration od data------
str(mall_customers1)
dim(mall_customers1)
summary(mall_customers1)

#checking for missing values
as.data.frame(colSums(is.na(mall_customers1)))
mall_customers1$Gender<-as.factor(mall_customers1$Gender)

##checking for outliers
library(graphics)
mall_customers1<-subset(mall_customers,select = -c(CustomerID))
boxplot(mall_customers1)
boxplot(mall_customers1$Annual.Income..k..)

summary(mall_customers1$Annual.Income..k..)
bench<-78+1.5*IQR(mall_customers1$Annual.Income..k..)

#132.75 ------ values above 132.75 are outliers

mall_customers1$Annual.Income..k..<-ifelse(mall_customers1$Annual.Income..k..>132,132,mall_customers1$Annual.Income..k..)

#Normalizing the Data for clustering 
library(caret)
preproc<-preProcess(mall_customers1)
mall_customersNorm<-predict(preproc,mall_customers1)
summary(mall_customersNorm)

#Hiearchical Clustering
distan<-dist(mall_customersNorm, method = "euclidean")
Clustermall_customers<-hclust(distan, method = "ward.D")
plot(Clustermall_customers)#Cluster Dendogram

#Assigning points to the clusters
mallCluster<-cutree(Clustermall_customers, k = 5)# 5 Clusters
table(mallCluster)

mallCluster1<-cutree(Clustermall_customers, k = 4)# 4 Clusters
table(mallCluster1)

##-----Selecting 5 clusters
#Computing the average values of the cluster groups
MeanComp<-function(var, clustergrp, meas){
  #var: Numeric Variable
  #clustergrp: Cluster Model created form Heirarchical Method
  #meas: Summary Measure - Mean
  z<-tapply(var, clustergrp, meas)#
  print(z)
}
Income_mean<-MeanComp(mall_customers1$Annual.Income..k.., mallCluster, mean)
Age_mean<-MeanComp(mall_customers1$Age, mallCluster, mean)
spending_mean<-MeanComp(mall_customers1$Spending.Score..1.100., mallCluster, mean)

df<-as.data.frame(cbind(Income_mean,Age_mean,spending_mean))

#Appending the Clusters Assignment
mall_cust_Hei<-data.frame(mall_customers1,mallCluster)#mall-customerCluster-->Heirarchical Cluster
write.csv(mall_cust_Hei,"mall_customers_Hierarchical.csv", row.names = FALSE)

write.csv(df,"properties_of_each_cluster.csv", row.names = FALSE)


