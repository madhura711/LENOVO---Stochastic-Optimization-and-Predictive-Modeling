rm(list = ls())
library(RcmdrMisc)
library(lubridate)
library(ggplot2)
library(e1071)
library(cluster)
library(FactoMineR)
library(factoextra)
library("rpart")
library("ada")
library(randomForest)
library(dplyr)
library(caret)
library(tidyverse)
library('pscl')
library('IDPmisc')

# Set working directory
setwd("C:/Users/NKARMAKAR/Google Drive/i2D Inrix/All i2D Analysis - Site by Site/Predictive Modeling")

# Reading combined i2d data
data2 <- read.csv(file.choose(),header=TRUE, sep = ",", stringsAsFactors=F)

# Checking for LOS distribution in dataset
barplot(table(data2$LOS))

# Filter unwanted columns
colname <- c('Train','TRIP_ID', 'i2D.Speed..mph.','Engine.Load','Jerk..m.s3.','RPM','Throttle.position','Acc_x_est..m.s2.', 'Total_acc','Acc_y_est..m.s2.','tmc_code','LOS','LOS.ratio', 'TMC_Length','No_int','no_lane','SP_Limit','dr_density', 'tmc_code')
# colname <- c('Acc_x_est (m/s2)', 'Total_acc')
data3  <- data2[colname]
data3[,"Speed_Change_Rate"] <- ave(data3$i2D.Speed..mph., data3$TRIP_ID, data3$tmc_code, FUN=function(x) c(0, (diff(x))))
data3[,"RPM_Change_Rate"] <- ave(data3$RPM, data3$TRIP_ID, data3$tmc_code, FUN=function(x) c(0, (diff(x))))
data3[,"Throttle_Change_Rate"] <- ave(data3$Throttle.position, data3$TRIP_ID, data3$tmc_code, FUN=function(x) c(0, (diff(x))))
data3[sapply(data3, is.infinite)] <- 0
data3[,"R1"] <- data3$Speed_Change_Rate/ data3$RPM_Change_Rate
data3[,"R2"] <- data3$Throttle_Change_Rate/ data3$RPM_Change_Rate
data3 <- NaRV.omit(data3)
R1_percentiles <- as.data.frame(quantile(data3$R1, probs = seq(0, 1, by = 0.025)))
R2_percentiles <- as.data.frame(quantile(data3$R2, probs = seq(0, 1, by = 0.025)))
RPM_Change_Rate_percentiles <- as.data.frame(quantile(data3$RPM_Change_Rate, probs = seq(0, 1, by = 0.025)))
Speed_Change_Rate_percentiles <- as.data.frame(quantile(data3$Speed_Change_Rate, probs = seq(0, 1, by = 0.025)))
Throttle_Change_Rate_percentiles <- as.data.frame(quantile(data3$Throttle_Change_Rate, probs = seq(0, 1, by = 0.025)))
Throttle_Position_percentiles <- as.data.frame(quantile(data3$Throttle.position, probs = seq(0, 1, by = 0.025)))
Engine_Load_percentiles <- as.data.frame(quantile(data3$Engine.Load, probs = seq(0, 1, by = 0.025)))


Jerk_percentiles <- as.data.frame(quantile(data3$Jerk..m.s3., probs = seq(0, 1, by = 0.025)))
ACC_percentiles <- as.data.frame(quantile(data3$Acc_x_est..m.s2., probs = seq(0, 1, by = 0.025)))
plot(RPM_Change_Rate_percentiles)
lower <- 0
upper <- 0.0015

#Function for behavior classification
labeling <- function(R1, R2,load){
  if ((R1 < lower || R1 > upper) && (R2 < lower || R2 > upper)&& (load < 20 || load > 60)) {
    label <- 1
  }  else {
    label <- 0
  }
  return (label)
}

labeling2 <- function(x){
  if ((x > 50 )) {
    label <- 1
  }  else {
    label <- 0
  }
  return (label)
}

#Creating Labels
data3[,"Label2"]<-mapply(labeling2, data3$Engine.Load)
data3[,"Label"]<-mapply(labeling, data3$R1, data3$R2, data3$Engine.Load)
hist(data3$Label)
hist(data3$Label2)
table <-xtabs(~Label2 + LOS, data = data3)
colPercents(table)
table <-xtabs(~Label2 + tmc_code, data = data3)
Label_by_tmc <- t(as.data.frame(colPercents(table)))
tmc <- rownames(Label_by_tmc)
data_tmc<- as.data.frame(cbind(tmc,Label_by_tmc))


# Creating Traiinng Set
trainset <- data3[data3$Train == 1,]
barplot(table(trainset$LOS))
hist(trainset$Label2)
colname <- c('i2D.Speed..mph.','Engine.Load','RPM','Throttle.position','Acc_x_est..m.s2.', 'Total_acc','Acc_y_est..m.s2.','LOS', 'Label2', 'TMC_Length','No_int','no_lane','SP_Limit','dr_density')
train <- trainset[colname]
train$LOS <- as.factor(train$LOS)
train$Label <- as.factor(train$Label2)

# Creating Test Data
testset <- data3[data3$Train == 0,]
barplot(table(testset$LOS))
hist(testset$Label2)
colname <- c('LOS','TMC_Length','No_int','no_lane','SP_Limit', 'dr_density','Label2')
test <- testset[colname]
test$LOS <- as.factor(test$LOS)
test$Label <- as.factor(test$Label2)


model <- glm (Label2 ~ LOS+TMC_Length+No_int+no_lane+SP_Limit+dr_density, data = train, family = binomial)
summary(model)
anova(model, test="Chisq")
#MacFadden R2 index
pR2(model)

# Prediction
table <-xtabs(~Label2 + LOS, data = test)
table(test$Label2)
colPercents(table)
rowPercents(test$Label2)
totPercents(table(test$Label2))

fitted.results <- predict(model,newdata=subset(test),type='response')
fitted.results <- ifelse(fitted.results > 0.3,1,0)
misClasificError <- mean(fitted.results != test$Label2)
table(fitted.results)
table(test$Label2)
print(paste('Accuracy',1-misClasificError))

varImp(model3)
# Reading combined i2d data
test1 <- read.csv(file.choose(),header=TRUE, sep = ",", stringsAsFactors=F)
colname <- c('LOS','TMC_Length','No_int','no_lane','SP_Limit', 'dr_density')
test2 <- test1[colname]


model2 <- glm (Label2 ~ LOS+TMC_Length+No_int+no_lane+SP_Limit+dr_density, data = data3, family = binomial)
test2.results <- as.data.frame(predict(model2, test2, type = 'response'))
pR2(model2)

model3 <- glm (Label ~ LOS+TMC_Length+No_int+no_lane+SP_Limit+dr_density, data = data3, family = binomial)
test3.results <- as.data.frame(predict(model3, test2, type = 'response'))
summary(model3)
pR2(model3)

# Precision and Recall using Caret Package
precision(tbl_2_1)
precision(data = tbl_2_1_pred, reference = tbl_2_1_truth, relevant = "Relevant")
recall(tbl_2_1)
recall(data = tbl_2_1_pred, reference = tbl_2_1_truth, relevant = "Relevant")
