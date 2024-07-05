install.packages("tm")
library(tm)
txt_dt <- read.csv("D:/Dataset/MS4S09_CW_Data.csv")


length(txt_dt$Review.Text)
View(txt_dt)
str(txt_dt)
sum(is.na(txt_dt))
crps <- txt_dt$Review.Text
crps <- Corpus(VectorSource(crps))
inspect(crps[1:5])

crps <- tm_map(crps, tolower)
inspect(crps[1:5])

crps <- tm_map(crps, removePunctuation)
inspect(crps[1:5])

#View(corpus)
crps <- tm_map(crps, removeNumbers)
inspect(crps[1:5])

crps <- tm_map(crps, removeWords, stopwords('english'))
inspect(crps[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cln_data <- tm_map(crps, content_transformer(removeURL))
inspect(cln_data[1:5])

cln_data <- tm_map(cln_data, stripWhitespace)
inspect(cln_data[1:5])

#install.packages("Rcpp")
library(Rcpp)
update.packages("Rcpp")


#install.packages("wordcloud")
library(wordcloud)

dt_mtx <- TermDocumentMatrix(cln_data[1:500])
mp <- as.matrix(dt_mtx)
wd <- rowSums(mp)
barplot(wd,
        las = 2,
        col = rainbow(50))
vtx <- sort(rowSums(mp),decreasing=TRUE)

dt <- data.frame(word = names(vtx),freq=vtx)
head(dt, 15)

require(stats)
plot(mp)
plot(vtx)
plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")

#install.packages("tmap")
library(tmap)
set.seed(572)
wordcloud(words = dt$word, freq = dt$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(dt[1:15,]$freq, las = 2, names.arg = dt[1:15,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

barplot(dt[1:20,]$freq, names.arg = dt[1:20,]$word,
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Cloth Reviews')

#Sentiemnt Analysis
#install.packages("syuzhet")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("reshape2")
#install.packages("dplyr")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Obtain sentiment scores
head(dt)
get_nrc_sentiment('perfect')
get_nrc_sentiment('shit')
get_nrc_sentiment('recommend')
get_nrc_sentiment('love')
get_nrc_sentiment('like')
get_nrc_sentiment('flattering')
get_nrc_sentiment('Absolutely wonderful - silky and sexy and comfortable')
get_nrc_sentiment(txt_dt$Review.Text[2])
get_nrc_sentiment(txt_dt$Review.Text[5])

# Data visualization

#scatter plots
x <- c(txt_dt$Rating)
y <- c(txt_dt$Age)
plot(x,y, xlab='Rating', ylab='Age of the customers')

# histogram plots
hist(txt_dt$Positive.Feedback.Count)

# Bar plots
plot(as.factor(txt_dt$Division.Name), xlab= 'Division name', ylab='Count')
barplot(txt_dt$Rating[0:40],names.arg=txt_dt$Division.Name[0:40],xlab="Division Name",ylab="Rating",col="blue",
        main="Division based Rating",border="green")


txt_dt$Clothing.ID
txt_dt$Age
txt_dt$Rating
txt_dt$Title <- NULL
txt_dt$Review.Text <- NULL
txt_dt$Division.Name <- NULL
txt_dt$Department.Name <- NULL
txt_dt$Class.Name <- NULL
str(txt_dt)
library(lmtest)
library(tidyverse)
library(caTools)

# Modelling techniques: Linear Regression Analysis

linear= lm(formula = txt_dt$Positive.Feedback.Count ~ txt_dt$Age,
           data = txt_dt)
summary(linear)
coef(linear)

plot(txt_dt$Positive.Feedback.Count, txt_dt$Age,col = "blue",main = "Positive feedback and Age regression",
    abline(lm(txt_dt$Positive.Feedback.Count ~ txt_dt$Age)),cex = 1.3,pch = 16,xlab = "Age",ylab = "Positive feedback")

