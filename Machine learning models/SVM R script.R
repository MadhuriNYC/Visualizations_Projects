#==========question 1================
mydata <- read.csv("Wine.csv", header = TRUE, sep = ",")
View(mydata)
attach(mydata)
nrow(mydata)
str(mydata)
which(is.na(mydata))
summary(mydata)
mydata$color<- ifelse(mydata$color=="red",1,0)
unique(mydata$quality)
levels(mydata$quality)
#===========question 2 ===============
#splitting the data
split_data<- sample(2, nrow(mydata), replace = TRUE, prob = c(0.7, 0.3))
traindata  <- mydata[split_data==1,]
testdata <- mydata[split_data==2,]
response.test <- testdata$quality
testdata$quality <- NULL
#=================question 3=============
library(nnet)
levels(traindata$quality)
logistic <- multinom(quality~., data = traindata)
summary(logistic)
#ph co efficent values
exp(0.3528874)
exp(0.7637270)
exp(2.2348936)
#============question 4============
library(caret)
predicted_probabilty <- predict(logistic, testdata, "class")
predicted_probabilty<- as.factor(predicted_probabilty)
response.test <- as.factor(response.test)
levels(response.test)
levels(predicted_probabilty)
CM <- table(predicted_probabilty, response.test)
CM
accuracy <- (CM[1,1]+CM[2,2])/sum(CM)
accuracy
precision <- CM[2,2]/sum(CM[2,])
precision
Recall <- CM[2,2]/sum(CM[,2])
Recall
#=============question 5=============
library(klaR)
levels(mydata$quality)<-make.names(levels(factor(mydata$quality)))
t_ctrl<- trainControl(method="cv", number=10, savePredictions = TRUE, classProbs= TRUE, summaryFunction=multiClassSummary)
t_svm<- train(quality ~., data=mydata,method="glm")
t_svm

