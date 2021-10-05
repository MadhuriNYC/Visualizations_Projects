projectdata <- read.csv("online_shoppers_intention.csv", header = TRUE, sep = ",")
View(projectdata)
nrow(is.na(projectdata))
which(is.na(projectdata))
summary(projectdata)
dim(projectdata)
colSums(is.na(projectdata))

projectdata$ProductRelated
typeof(projectdata$ProductRelated)
typeof(projectdata$ProductRelated_Duration)
boxplot(projectdata$ProductRelated_Duration)$out
hist(projectdata$ProductRelated, xlim = c(0,200))
hist(projectdata$ProductRelated_Duration, xlim = c(0,20000))
boxplot(projectdata$ProductRelated_Duration)
installed.packages()

summary(projectdata$ProductRelated)


projectdata %>% 
  ggplot2::aes(x=ProductRelated) +
  geom_bar() +
  facet_grid(Revenue ~ .,
             scales = "free_y")

hist(projectdata$Revenue)
install.packages(ggplot)
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
