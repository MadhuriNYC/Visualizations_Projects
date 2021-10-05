#PART A
Billing.Info <- read.csv("BillingInfoAssignment.csv", header = TRUE, sep = ",")
library(dplyr)
View(Billing.Info)
#A
arranged.data <- arrange(Billing.Info, Billing.Info$AgeInYears)
arranged.data
#B
newdata <- select(Billing.Info, FirstName, LastName, AgeInYears,Gender)
View(newdata)
#C
typeof(Billing.Info$Insurance)
typeof(Billing.Info$ChargesInDollars)
dplyr.filter <- filter(Billing.Info, ChargesInDollars >90 & Insurance=='Self Pay')
dplyr.filter
#D
dplyr.filter2 <- filter(Billing.Info, Insurance == 'Medicaid' | Insurance=='Private')
dplyr.filter2
#E
Billing.Info %>%
  group_by(Insurance) %>%
  summarise(avg.ChargesInDollars= mean(ChargesInDollars))
  

#Part B
Billing.Info <- read.csv("BillingInfoAssignment.csv", header = TRUE, sep = ",")
Vital.info <- read.csv("VitalInfoAssignment.csv", header = TRUE, sep = ",")

#merge
?merge
#A
data.inner.join <- merge(Billing.Info, Vital.info, by.x=c("FirstName", "LastName"), by.y=c("First", "Last"),all = FALSE)
View(data.inner.join)

#B
assign.outer.join <- merge(Billing.Info, Vital.info,  by.x=c("FirstName", "LastName"), by.y=c("First", "Last"), all = TRUE)
View(assign.outer.join)

#C
subset.SE <- data.inner.join[substr(data.inner.join$Insurance, 1,2)=="Se",]
subset.SE

#D
MEAN <- mean(data.inner.join$SystolicBP, na.rm = TRUE) + mean(data.inner.join$DiastolicBP, na.rm = TRUE)
MEAN
mean(MEAN)
#
merge.inner %>%
  summarise(combined.mean = mean((SystolicBP + DiastolicBP), na.rm = TRUE))


#E
nrow(data.inner.join)
delete_lastrow <- data.inner.join[-8,]
View(delete_lastrow)

#F
delete_lastrow$Date <- as.Date(delete_lastrow$Date, format = "%m/%d/%y")
delete_lastrow$YearVisited <- substr(delete_lastrow$Date, 1,4)
delete_lastrow
#date.ex <- format(as.Date(row.deleted$Date, format="%m/%d/%y"),"%y")
date.ex
with.year.visted <- row.deleted %>%
  mutate(YearVisited = date.ex)#


#G
Charge_in_dollars_DESC <- arrange(delete_lastrow, desc(delete_lastrow$ChargesInDollars))
Charge_in_dollars_DESC

#H
unique.insurance <- unique(Charge_in_dollars_DESC$Insurance)
unique.insurance

#I
class(Charge_in_dollars_DESC$FirstName)
dim(Charge_in_dollars_DESC)
str(Charge_in_dollars_DESC)

View(Charge_in_dollars_DESC)

#J
Charge_in_dollars_DESC$ChargesPerVisit <- Charge_in_dollars_DESC$ChargesInDollars/Charge_in_dollars_DESC$PriorVisits
Charge_in_dollars_DESC

#Charges_per_visit<- Charge_in_dollars_DESC %>%
  select(ChargesInDollars, PriorVisits) %>%
  mutate(ChargesPerVisit= ChargesInDollars/PriorVisits)#


#k
final_data <- Charge_in_dollars_DESC[,-c(5,10)]
final_data
