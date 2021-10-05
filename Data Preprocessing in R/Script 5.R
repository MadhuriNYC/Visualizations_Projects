urgent.care<-read.csv(file = "CombinedUrgentCareAssignment.csv", header = TRUE,sep = ",")

urgent.care <- as.data.frame(urgent.care)

#A
#Function to check duplicate values and valid no of visits
check<-function(data){
  for(i in 1:nrow(data)) {
    if(data$NumberOfVisits[i] <= 0) {
      data$NumberOfVisits[i] <- mean(data$NumberOfVisits,na.rm = T)
    } 
  }
  if(sum(duplicated(data)) == 0) {
    print("n")
  } else {
    print( data2<-unique(data))
  }
  
}

data <- check(urgent.care)
str(data)


#B
#New Field charges per visit 
ChargesPerVisit<-data$TotalCharges/data$NumberOfVisits
ChargesPerVisit
mean(ChargesPerVisit)

#c
#Label to check if else condition 
Label <- ifelse(ChargesPerVisit< mean(ChargesPerVisit),"Low maintenance patient", "High maintenance patient")
Label<-as.factor(Label)
Label

#d
#data frame with two new coloumns
patientdataprocessing<-function(data){
  t<-cbind(data,ChargesPerVisit)
  e<-cbind(t,Label)
}



#E
#invoking the function 
updated.data <- patientdataprocessing(data)
str(updated.data)

#2

#A
#reading csv file 
State.data <-read.csv(file = "StatesData.csv", header = TRUE,sep = ",")

#cut function  with breaks on HouseHoldIncome
State.data$HouseHoldIncome<-cut(State.data$HouseHoldIncome,breaks = c(10000, 26000, 40000,70000),labels = c("Low", "Medium", "High"))
#Assigning levels
levels(State.data$HouseHoldIncome)
str(State.data)

State.data$HouseHoldIncome<-as.factor(State.data$HouseHoldIncome)

print(State.data$HouseHoldIncome)

#B
#boxplot
library(ggplot2)
View(State.data)

ggplot(State.data, aes(HouseHoldIncome, Mortality))+ geom_boxplot()+labs(x="Income",y="Mortality Index ",title = " Mortality Index Based on Income Levels")


