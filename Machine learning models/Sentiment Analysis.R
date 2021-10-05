#1
install.packages("tm")
install.packages("lda")
install.packages("caret")
install.packages("NLP")
install.packages("topicmodels")
install.packages("wordcloud")
install.packages("mlbench")
install.packages("SentimentAnalysis")
library(tm)
library(lda)
library(caret)
library(NLP)
library(topicmodels)
library(wordcloud)
library(mlbench)
library(SentimentAnalysis)
head(mydata$Text)
mydata <- read.csv("FineFoodReviews.csv")
review.corpus <- VCorpus(VectorSource(t(mydata$Text)))
inspect(review.corpus[1:2])

review.corpus <- tm_map(review.corpus, content_transformer(tolower))
inspect(review.corpus[1:2])

review.corpus <- tm_map(review.corpus, removePunctuation)

review.corpus <- tm_map(review.corpus, removeNumbers)

review.corpus <- tm_map(review.corpus, removeWords, c(stopwords("english")))

term.doc.matrix <- TermDocumentMatrix(review.corpus)

double.term.doc.matrix <- as.matrix(term.doc.matrix)
doc.term.matrix.converted <- t(double.term.doc.matrix)

#2

sort.matrix.term.doc.matrix <- sort(rowSums(double.term.doc.matrix), decreasing = TRUE)
barplot(sort.matrix.term.doc.matrix[1:50],space = 0.5, main = "word Frequencies", xlab = "word", ylab = "frequency", las=2)
sort.matrix.term.doc.matrix[1:5]
#frequently used words are like, taste, good, just, great, can
findFreqTerms(term.doc.matrix, lowfreq = 2)
rowSums(double.term.doc.matrix)


#3
lda3 <- LDA(doc.term.matrix.converted , k=3, control = list(estimate.alpha=TRUE))
term <- terms(lda3,5)
term

lda4 <- LDA(doc.term.matrix.converted, k=4, control = list(estimate.alpha=TRUE))
term <- terms(lda4,5)
term

lda5 <- LDA(doc.term.matrix.converted, k=5, control = list(estimate.alpha=TRUE))
term <- terms(lda5,5)
term

#4
install.packages("SnowballC")
library(SnowballC)
sentiment <- analyzeSentiment(term.doc.matrix)
mydata1 <-convertToBinaryResponse(sentiment)
View(mydata1)
extractdata <- mydata1[c(12:14)]
extractdata
mydata2 <- cbind(mydata, extractdata)
names(mydata2)

#4

mydata2$Score<-factor(mydata2$Score)
levels(mydata2$Score)<-make.names(levels(factor(mydata2$Score)))
library(nnet)
model <- multinom(Score~PositivityQDAP+NegativityQDAP+SentimentQDAP, data = mydata2)
summary(model)
