install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
#===========
trans_mat<-read.csv("1000-out2.csv",header=TRUE,sep=",")
#convert it into a data matrix
a_matrix<-data.matrix(trans_mat)
#remove the transaction_ID column i.e., column 1
a_matrix<-a_matrix[,-1]
View(a_matrix) 
#use logical function to convernt binary to T/F
basket2<-apply(a_matrix,2,as.logical)
View(basket2)
#Now coerce it into a matrix
basket<-as(basket2,"transactions")
basket
View(basket)
#=========question 1 ================
rules_1 <- apriori(basket, parameter = list(support=0.02, confidence = 0.9))
rules_1
inspect(head(rules_1, n= 10, by = "confidence", decreasing = TRUE))
#==========question 2 ====================
rules_2 <- apriori(basket, parameter = list(support = 0.03, confidence = 0.95))
rules_2
inspect(head(rules_2, by = "lift", decreasing = TRUE))
#===========question 3 =======================
plot(rules_1, measure = c("support", "confidence"), shading = "lift")
plot(rules_1, measure = c("support", "confidence"), shading = "lift", jitter=0)
#===========question 4 =======================
plot(rules_2, method = "paracoord")
