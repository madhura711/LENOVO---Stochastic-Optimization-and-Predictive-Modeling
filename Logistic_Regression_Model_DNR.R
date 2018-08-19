#Setup Library
library(readxl)
library(caret)
RawData <- read_excel("**/ModelInformation.xlsx", na = "null")

#Coerce columns as factors

RawData$Machine_Status<-as.factor(RawData$Machine_Status)#Derived Column
RawData$Machine_Type<-as.factor(RawData$Machine_Type)
RawData$Machine_Type_2<-as.factor(RawData$Machine_Type_2) #Derived Column
RawData$Country_Code<-as.factor(RawData$Country_Code)
RawData$Service_Delivery_Type<-as.factor(RawData$Service_Delivery_Type)

#Creation of training and testing dataset

set.seed(123) #Seed to sample the data
train_sample <- sample(seq_len(nrow(RawData)), size = floor(0.7 * nrow(RawData)))
Train_RawData<-RawData[train_sample, ]
Test_RawData <- RawData[-train_sample, ]

#Logistic Regression Model

#Model Training
Model<-glm(Machine_Status~Service_Delivery_Type + No_of_Escalations+Total_Cost+Country_Code+Age_2017+Machine_Type_2,family=binomial(link='logit'),data = Train_RawData)
summary(Model)

#Model Testing 
Prediction<-cbind.data.frame(Test_RawData,predict(Model, newdata = Test_RawData , type = "response"))
colnames(Prediction)[12]<-"Predict_Probability"
plot(Prediction$Machine_Status,Prediction$Predict_Probability)
confusionMatrix(round(Prediction$Predict_Probability,0),Prediction$Machine_Status,positive = "1")




