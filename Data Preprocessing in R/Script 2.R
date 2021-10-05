#
urgentcare <- read.csv("CombinedUrgentCareAssignment.csv", header = TRUE, sep = ",")

patientprocessing <- function(urgentcare){
  #question 1 part a
  duplicated(urgentcare)
  #check if there are duplicated values
  urgentcare[duplicated(urgentcare),]
  #delete the duplicated value
  new.data <- urgentcare[!duplicated(urgentcare),]
  #replace the zeros and negetive values with average
  is.integer(new.data$NumberOfVisits)
  summary(new.data$NumberOfVisits)
  new.data$NumberOfVisits[new.data$NumberOfVisits<0]<-0
  new.data$NumberOfVisits
  mean.of.NumberOfVisits<-round(summary(new.data$NumberOfVisits[new.data$NumberOfVisits > 0])[4])
  new.data$NumberOfVisits[new.data$NumberOfVisits<=0]<-mean.of.NumberOfVisits
  new.data$NumberOfVisits
}

# Q1 Part b
new.data$ChargesPerVisits<- new.data$TotalCharges/new.data$NumberOfVisits
new.data

# Q1 Part c
mean.of.ChargePerVisits<- mean(new.data$ChargesPerVisits)

for (i in 1:nrow(new.data)) {
  if (new.data$ChargesPerVisits[i] >mean.of.ChargePerVisits) {
    new.data$Label[i] <- "High Maintainence Patient"
    
  }else
  { 
    new.data$Label[i] <- "Low Maintainence Patient"
  }
}  
return(new.data) }# Q1 part d


#Q1 Part e
urgent.care <- read.csv(file = "CombinedUrgentCareAssignment.csv", header = TRUE, sep = ",")

updated.urgentcare.data <- patientdataprocessing(urgent.care)

updated.urgentcare.data

#Q2 Part a

#reading csv file
states.data <- read.csv(file = "StatesData.csv", header = TRUE, sep = ",")

#Create income factor object
income.factor<-cut(states.data$HouseHoldIncome, breaks = c(0, 26000, 40000, max(states.data$HouseHoldIncome)), labels=c("Low", "Medium", "High"))

#creating labels for levels
income.labels<-c("Low", "Medium", "High")   
levels(income.factor) <- income.labels                   
print(income.factor)

#Q2 part b

#creating boxplot
boxplot(states.data$Mortality ~ income.factor, main="Mortality Index Based On Income Levels", ylab="Mortality Index", xlab="Income")




