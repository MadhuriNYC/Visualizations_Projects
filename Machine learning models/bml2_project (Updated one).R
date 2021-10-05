# ==================================== Heart Failure Prediction ===========================


## packages required
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("useful")
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(cluster)
library(arules)
library(arulesViz)
library(ggcorrplot)
library(corrplot)
library(useful)

## setting the directory and reading the data -------------------------------
setwd("C:/Users/Niharika/Documents/courses/BML_II/Project")
Heart.Failure.Data <- read.csv("Heartfailure.csv", sep = ",", header = T)

## data description ---------------------------------------------------------
View(Heart.Failure.Data)
attach(Heart.Failure.Data)
colSums(is.na(Heart.Failure.Data))
summary(Heart.Failure.Data) 
dim(Heart.Failure.Data)
sapply(Heart.Failure.Data, class)


## data prepocessing steps ---------------------------------------------------

# step2: visualization of each column and replacing outliers 
#with their respective median values 

#1 visualization for attribute1 - age
hist(Heart.Failure.Data$age, main = "Age Distribution", xlab = "Age in Years",
     ylab = "Number of patients", col = "beige")

#2 visualization for attribute2 - Anaemia 
anaemia.count <- table(Heart.Failure.Data$anaemia)
anaemia.count
anaemia.count.label <- c("Without Anaemia", "With Anaemia")
barplot(anaemia.count, names.arg = anaemia.count.label, col = "yellow",
        main = "Patients with and without Anaemia", ylab = "Number of patients")

#3 visualization for attribute3 - creatinine_phosphokinae (CPK level)
hist(Heart.Failure.Data$creatinine_phosphokinase)
boxplot(Heart.Failure.Data$creatinine_phosphokinase)$out
# replacing outliers with median
Heart.Failure.Data$creatinine_phosphokinase[creatinine_phosphokinase %in% boxplot(Heart.Failure.Data)$out] <- median(creatinine_phosphokinase)
boxplot(Heart.Failure.Data$creatinine_phosphokinase)$out
hist(Heart.Failure.Data$creatinine_phosphokinase)

#4 visualization for attribute4 - diabetes
diabetes.count <- table(Heart.Failure.Data$diabetes)
diabetes.count
diabetes.count.label <- c("Without Diabetes", "With Diabetes")
barplot(diabetes.count, names.arg = diabetes.count.label, col = "yellow",
        main = "Patients with and without diabetes", ylab = "Number of patients")

#5 visualization for attribute5 - ejection_fraction
hist(Heart.Failure.Data$ejection_fraction)
#replacing outliers with median
boxplot(Heart.Failure.Data$ejection_fraction)$out
Heart.Failure.Data$ejection_fraction[ejection_fraction %in% boxplot(Heart.Failure.Data)$out] <- median(ejection_fraction)
boxplot(Heart.Failure.Data$ejection_fraction)$out
hist(Heart.Failure.Data$ejection_fraction)

#6 visualization for attribute6 - high_bp
highbp.count <- table(Heart.Failure.Data$high_blood_pressure)
highbp.count
highbp.count.label <- c("Without High BP", "With High BP")
barplot(highbp.count, names.arg = highbp.count.label, col = "yellow",
        main = "Patients with and without highbp", ylab = "Number of Patients")

#7 visualization for attribute7 - platelets
hist(Heart.Failure.Data$platelets, xlab = "Platelets Count", col = "beige",
     ylab = "Number of Patients", main = "Distribution of Platelets Count")
boxplot(Heart.Failure.Data$platelets)$out
Heart.Failure.Data$platelets[Heart.Failure.Data$platelets %in% boxplot(Heart.Failure.Data)$out] <- median(Heart.Failure.Data$platelets)
boxplot(Heart.Failure.Data$platelets)$out
hist(Heart.Failure.Data$platelets, xlab = "Platelets Count", col = "beige",
     ylab = "Number of Patients", main = "Distribution of Platelets Count")

#8 visualization for attribute8 - serum_creatinine
hist(Heart.Failure.Data$serum_creatinine, xlab = "Serum Creatinine Levels", col = "beige",
     ylab = "Number of Patients", main = "Distribution of Serum Creatinine Levels")
boxplot(Heart.Failure.Data$serum_creatinine)$out
Heart.Failure.Data$serum_creatinine[Heart.Failure.Data$serum_creatinine %in% boxplot(Heart.Failure.Data)$out] <- median(Heart.Failure.Data$serum_creatinine)
boxplot(Heart.Failure.Data$serum_creatinine)$out
hist(Heart.Failure.Data$serum_creatinine, xlab = "Serum Creatinine Levels", col = "beige",
     ylab = "Number of Patients", main = "Distribution of Serum Creatinine Levels")

#9 visualization for attribute9 - serum_sodium
hist(Heart.Failure.Data$serum_sodium, xlab = "Serum Sodium Levels", col = "beige",
     ylab = "Number of Patients", main = "Distribution of Serum Sodium Levels")
boxplot(Heart.Failure.Data$serum_sodium)$out
Heart.Failure.Data$serum_sodium[Heart.Failure.Data$serum_sodium %in% boxplot(Heart.Failure.Data)$out] <- median(Heart.Failure.Data$serum_sodium)
boxplot(Heart.Failure.Data$serum_sodium)$out
hist(Heart.Failure.Data$serum_sodium, xlab = "Serum Sodium Levels", col = "beige",
     ylab = "Number of Patients", main = "Distribution of Serum Sodium Levels")

#10 visualization for attribute10 - sex
sex.count <- table(Heart.Failure.Data$sex)
sex.count
sex.count.label <- c("Female", "Male")
barplot(sex.count, names.arg = sex.count.label, main = "Gender of the patient", 
        col = "yellow", ylab = "Number of Patients")

#11 visualization for attribute11 - smoking
smoking.count <- table(Heart.Failure.Data$smoking)
smoking.count
smoking.count.label <- c("No Smoking", "Smoking")
barplot(smoking.count, names.arg = smoking.count.label, col = "yellow",
        main = "Smoking habits of patient", ylab = "Number of Patients")

#12 visualization for attribute12 - time
hist(Heart.Failure.Data$time, xlab = "Follow-up Time in Days ", col = "beige",
     ylab = "Number of Patients", main = "Distribution of Follow-up Time (Days)")
boxplot(Heart.Failure.Data$time)$out

#13 visualization for attribute13 - Death Event
death.count <- table(Heart.Failure.Data$DEATH_EVENT)
death.count
death.count.label <- c("Survived", "Death")
barplot(death.count, names.arg = death.count.label, col = "yellow",
        main = "Number of Heart Failures and Survivals", ylab = "Number of Patients")

summary(Heart.Failure.Data)

# correlation between each features of data

cor_result <- cor(Heart.Failure.Data)
round(cor_result, 2)
ggcorrplot(cor_result, hc.order = T) + labs(title = "Correlation between each features of dataset")


## Descriptive Statistics -------------------------------------------------

# relations between 2 variables

plot(Heart.Failure.Data$age, creatinine_phosphokinase)
plot(Heart.Failure.Data$age, anaemia)
plot(Heart.Failure.Data$age, diabetes)
plot(Heart.Failure.Data$age, ejection_fraction)
plot(Heart.Failure.Data$age, high_blood_pressure)
plot(Heart.Failure.Data$age, platelets)
plot(Heart.Failure.Data$age, serum_creatinine)
plot(Heart.Failure.Data$age, serum_sodium)
plot(Heart.Failure.Data$age, sex)
plot(Heart.Failure.Data$age, smoking)
plot(Heart.Failure.Data$age, time)
plot(Heart.Failure.Data$age, DEATH_EVENT)

plot(creatinine_phosphokinase, anaemia)
plot(creatinine_phosphokinase, diabetes)
plot(creatinine_phosphokinase, ejection_fraction)
plot(creatinine_phosphokinase, high_blood_pressure)
plot(creatinine_phosphokinase, platelets)
plot(creatinine_phosphokinase, serum_creatinine)
plot(creatinine_phosphokinase, serum_sodium)
plot(creatinine_phosphokinase, sex)
plot(creatinine_phosphokinase, smoking)
plot(creatinine_phosphokinase, time)
plot(creatinine_phosphokinase, DEATH_EVENT)

plot(ejection_fraction, anaemia)
plot(ejection_fraction, diabetes)
plot(ejection_fraction, high_blood_pressure)
plot(ejection_fraction, platelets)
plot(ejection_fraction, serum_creatinine)
plot(ejection_fraction, serum_sodium)
plot(ejection_fraction, sex)
plot(ejection_fraction, smoking)
plot(ejection_fraction, time)
plot(ejection_fraction, DEATH_EVENT)

plot(platelets, anaemia)
plot(platelets, diabetes)
plot(platelets, high_blood_pressure)
plot(platelets, serum_creatinine)
plot(platelets, serum_sodium)
plot(platelets, sex)
plot(platelets, smoking)
plot(platelets, time)
plot(platelets, DEATH_EVENT)

plot(serum_creatinine, anaemia)
plot(serum_creatinine, diabetes)
plot(serum_creatinine, high_blood_pressure)
plot(serum_creatinine, serum_sodium)
plot(serum_creatinine, sex)
plot(serum_creatinine, smoking)
plot(serum_creatinine, time)
plot(serum_creatinine, DEATH_EVENT)

plot(serum_sodium, anaemia)
plot(serum_sodium, diabetes)
plot(serum_sodium, high_blood_pressure)
plot(serum_sodium, sex)
plot(serum_sodium, smoking)
plot(serum_sodium, time)
plot(serum_sodium, DEATH_EVENT)
plot(time, anaemia)
plot(time, diabetes)
plot(time, high_blood_pressure)
plot(time, sex)
plot(time, smoking)
plot(time, DEATH_EVENT)


## creating clusters ----------------------------------------------------------

# subset
new.data <- Heart.Failure.Data[c("age", "time", "serum_creatinine", "creatinine_phosphokinase", "ejection_fraction", "platelets", "serum_sodium")]
View(new.data)

# clustering
distance2 <- dist(as.matrix(new.data))
cluster_2 <- kmeans(new.data, 2)
cluster_3 <- kmeans(new.data, 3)
cluster_4 <- kmeans(new.data, 4)
cluster_5 <- kmeans(new.data, 5)
sil_2 <- mean(silhouette(cluster_2$cluster, dist(new.data))[,3])
sil_3 <- mean(silhouette(cluster_3$cluster, dist(new.data))[,3])
sil_4 <- mean(silhouette(cluster_4$cluster, dist(new.data))[,3])
sil_5 <- mean(silhouette(cluster_5$cluster, dist(new.data))[,3])
k_values <- 2:5
silhouette_values <- c(sil_2, sil_3, sil_4, sil_5)
plot(k_values, silhouette_values, type = "b")
plot(cluster_2, new.data)




# association

View(Heart.Failure.Data)
# association
subset.boolean <- subset(Heart.Failure.Data, select = c(anaemia, diabetes, high_blood_pressure, sex, smoking))
# association on boolean dataset
subset.boolean1 <- subset.boolean[,-4]
heartfailure_boolean.matrix <- data.matrix(subset.boolean1)

rules_1 <- apriori(heartfailure_boolean.matrix, parameter = list(support=0.02, confidence = 0.05, maxlen = 15, minlen = 2))
inspect(head(rules_1, n=10, by = "confidence", decreasing=T))
plot(rules_1, measure = c("support", "confidence"), shading = "lift", jitter = 0)
plot(rules_1, method = "paracoord")
typeof(Heart.Failure.Data$DEATH_EVENT)
#logistic regression 
split_data<- sample(2, nrow(Heart.Failure.Data), replace = TRUE, prob = c(0.7, 0.3))
traindata  <- Heart.Failure.Data[split_data==1,]
testdata <- Heart.Failure.Data[split_data==2,]
nrow(traindata)
nrow(testdata)
response.test <- testdata$DEATH_EVENT
testdata$DEATH_EVENT <- NULL

library(nnet)
logistic <- glm(DEATH_EVENT~., data = traindata)
summary(logisticlibrary(caret)
predicted_probability <- predict(logistic, testdata,"response")
predicted_probability <- as.factor(predicted_probability)
response.test <- as.factor(response.test)
levels(response.test)
levels(predicted_probability)
CM <- table(predicted_probability, response.test)
CM
accuracy <- (CM[1,1]+CM[2,2])/sum(CM)
accuracy
precision <- CM[2,2]/sum(CM[2,])
precision
Recall <- CM[2,2]/sum(CM[,2])
Recall


library(klaR)
levels(Heart.Failure.Data$DEATH_EVENT)<-make.names(levels(factor(Heart.Failure.Data$DEATH_EVENT)))
t_ctrl<- trainControl(method="cv", number=10, savePredictions = TRUE, classProbs= TRUE, summaryFunction=multiClassSummary)
t_svm<- train(DEATH_EVENT ~., data=Heart.Failure.Data, method="glm")
t_svm


#decison tree
Heart.Failure.Data$DEATH_EVENT<-factor(Heart.Failure.Data$DEATH_EVENT)
traindata$DEATH_EVENT <-factor(traindata$DEATH_EVENT)
testdata$DEATH_EVENT <- factor(testdata$DEATH_EVENT)

library(party)
Heart.Failure.Data.Ctree <- ctree(DEATH_EVENT~.,traindata)
print(Heart.Failure.Data.Ctree)
plot(Heart.Failure.Data.Ctree)

train_predict <- predict(Heart.Failure.Data.Ctree, traindata)
test_predict <- predict(Heart.Failure.Data.Ctree, testdata)

library(caret)
mat1 <- table(predicted= train_predict, Actual = traindata$DEATH_EVENT)
mat2 <- table(predicted = test_predict, Actual = testdata$DEATH_EVENT)
mat1
mat2
class(testdata$DEATH_EVENT)
confusionMatrix(test_predict, testdata$DEATH_EVENT)


#other models
levels(Heart.Failure.Data$DEATH_EVENT) <- make.names(levels(factor(Heart.Failure.Data$DEATH_EVENT)))
library(klaR)
library(MLmetrics)

#ten-fold cross validation
tc<- trainControl(method = "cv",number = 10, savePredictions =TRUE, classProbs = TRUE, summaryFunction = multiClassSummary)

#svm
train_svm<- train(DEATH_EVENT~., data=Heart.Failure.Data,method="svmRadial")
train_svm

#random forest
train_rf<- train(DEATH_EVENT~., data = Heart.Failure.Data,method="rf")
train_rf

#naive bayes
 train_nb <- train(DEATH_EVENT~., data = Heart.Failure.Data, method="nb")
 train_nb
 