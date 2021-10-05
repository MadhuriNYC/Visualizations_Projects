shoppersdata <- read.csv("ShoppersData.csv", header = TRUE, sep=',')
View(shoppersdata)
class(shoppersdata$TimeInMins)
#graphical representation of the time spent 
hist(shoppersdata$TimeInMins)
#numerical representation of time spent
summary(shoppersdata$TimeInMins)
Timeinmins <- shoppersdata$TimeInMins
sd(Timeinmins, na.rm = TRUE)
range(Timeinmins, na.rm = TRUE)
mode(Timeinmins, na.rm= TRUE)
#graphical representation for no of page views
hist(ShoppersData1$PagesViewed)
barplot(shoppersdata$PagesViewed, main = "Number of pages viewed by shopper", ylab = "no of shoppers", xlab = "pages viewed")
inputs <- c()