#check your working directory
getwd()
#import Your .csv file
mealprice <- read.csv("mealprice.csv", header = TRUE, sep = ',')
mealprice
hist(mealprice$PriceInDollars, xlab = "Meal Prices in $", ylab = "Frequency", main = "Distribution of meal prices" )

