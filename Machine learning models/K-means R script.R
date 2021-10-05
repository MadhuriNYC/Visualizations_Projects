#============question 1================
#load and attach dataset
mydata <- read.csv("nbc_demographics.csv", header = TRUE, sep = ",")
attach(mydata)
#data exploration
colnames(mydata)
class(mydata$Show)
class(mydata$TERRITORY.EAST.CENTRAL)
class(mydata$TERRITORY.NORTHEAST)
class(DVD.OWNER)
summary(mydata)
nrow(mydata)
ncol(mydata)
#largest no of viewers in SW region
southwest_region <- which.max(mydata$TERRITORY.SOUTHWEST)
southwest_region
mydata$Show[southwest_region]
#==========question 2========= 
inputs <- c("WHITE.COLLAR", "BLUE.COLLAR", "NOT.IN.LABOR.FORCE")
new.data <- mydata[inputs]
#applying k-means
cl_4 <- kmeans(new.data, 4)
cl_5 <- kmeans(new.data, 5)
#ratio of inter cluster to intra cluster 
cl_4$betweenss/cl_4$tot.withinss
cl_5$betweenss/cl_5$tot.withinss
#============Question 3===============
plot(new.data$WHITE.COLLAR, new.data$BLUE.COLLAR, xlab = "WHITE COLLAR", ylab = "BLUECOLLAR", col=cl_4$cluster)
plot(new.data$WHITE.COLLAR, new.data$BLUE.COLLAR, xlab = "WHITE COLLAR", ylab = "BLUECOLLAR", col=cl_5$cluster)
#silhouette method
library(cluster)
sil_4 <- mean(silhouette(cl_4$cluster,dist(new.data[c("WHITE.COLLAR", "BLUE.COLLAR")]))[,3])
sil_4
sil_5 <- mean(silhouette(cl_5$cluster,dist(new.data[c("WHITE.COLLAR", "BLUE.COLLAR")]))[,3])
sil_5
#============question 4===============
inputs2 <- c("VIDEO.GAME.OWNER", "DVD.OWNER")
new.data2 <- mydata[inputs2]
newCl_3 <- kmeans(new.data2,3)
newcl_4 <- kmeans(new.data2,4)
newcl_5 <- kmeans(new.data2,5)
newcl_6 <- kmeans(new.data2,6)
#ratio for k=3
newCl_3$betweenss/newCl_3$tot.withinss
#ratio for k=4
newcl_4$betweenss/newcl_4$tot.withinss
#ratio for k= 5
newcl_5$betweenss/newcl_5$tot.withinss
#ratio for k=6
newcl_6$betweenss/newcl_6$tot.withinss
#plot when k=3
plot(new.data2$VIDEO.GAME.OWNER, new.data2$DVD.OWNER,  xlab = "VIDEO.GAME", ylab = "DVD.OWNER", col=newCl_3$cluster)
#plot when k= 4
plot(new.data2$VIDEO.GAME.OWNER, new.data2$DVD.OWNER,  xlab = "VIDEO.GAME", ylab = "DVD.OWNER", col=newcl_4$cluster)
#plot when k =5
plot(new.data2$VIDEO.GAME.OWNER, new.data2$DVD.OWNER,  xlab = "VIDEO.GAME", ylab = "DVD.OWNER", col=newcl_5$cluster)
#plot when k= 6
plot(new.data2$VIDEO.GAME.OWNER, new.data2$DVD.OWNER,  xlab = "VIDEO.GAME", ylab = "DVD.OWNER", col=newcl_6$cluster)
#plot using elbow method
k_values <- c(3,4,5,6)
ss_values <- c(newCl_3$tot.withinss, newcl_4$tot.withinss, newcl_5$tot.withinss, newcl_6$tot.withinss)
plot(k_values,ss_values, type = "b")
#=============question 5 ==============
#ratio of WHITE.COLLAR, BLUE.COLLAR, NOT.IN.LABOR.FORCE
cl_4$betweenss/cl_4$tot.withinss
cl_5$betweenss/cl_5$tot.withinss
#ratio of VIDEO.GAME.OWNER" and "DVD.OWNER
newcl_4$betweenss/newcl_4$tot.withinss
newcl_5$betweenss/newcl_5$tot.withinss
#============question 6 =================
input3 <- c(2:16)
H_data <- mydata[input3]
d <- dist(as.matrix(H_data))
hc <- hclust(d)
plot(hc)
#plot dentrogram using single method 
hc_1 <- hclust(d,method = "single")
plot(hc_1)
#plot dentrogram using average method 
hc_2 <- hclust(d, method = "average")
plot(hc_2)
#show names 
mydata$Show[12]
mydata$Show[30]
