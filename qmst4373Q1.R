
# 1. Text Mining
library(tm)
library(SnowballC)
library(wordcloud)
library(igraph)
library(topicmodels)
library(purrr)
source("Functions/wordVector_fxn.R")

## Retrieve data, remove stopwords and punctuation
docs <- Corpus(DirSource("Data/Platforms2016/"))
docs

getTransformations()

# Remove punctuation, convert the corpus to lower case, remove all numbers.
# Transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)

# Remove stopwords using the standard list in tm, strip whitespace
docs <- tm_map(docs, removeWords, stopwords("english"))
myStops <- c("also")
docs <- tm_map(docs, removeWords, myStops)
docs <- tm_map(docs, stripWhitespace)

# Creation of the document term matrix  (DTM)– a matrix that lists all occurrences of words in the corpus, by document.
dtm <- DocumentTermMatrix(docs)

# inspect() takes input DTM[document_index, word_index]
inspect(dtm)
inspect(dtm[1, ])  # Democratic Platform 
inspect(dtm[2, ])  # Republican Platform

## Plot 30 words with highest frequencies
sortedWordFreqs <- sort(colSums(as.matrix(dtm)))
swf_len <- length(sortedWordFreqs)
top30 <- sortedWordFreqs[(swf_len-29):swf_len]
barplot(top30, horiz = TRUE, las = 1, col = "lightblue", 
        xlab = "Term Frequencies",
        main = "Top 30 Words from 2016 Democratic \nand Republican Platforms")

## Construct a wordcloud (requires pckg "wordcloud")
set.seed(1)
wordcloud(names(sortedWordFreqs), sortedWordFreqs, min.freq=70, colors=brewer.pal(6,"Dark2"))

## Construct a network graph (requires pckg "igraph")
# termMatrix <- sortedWordFreqs%*%t(sortedWordFreqs)
# Network graph code keeps crashing R, no clue why. I will come back to this.

## Apply cluster analysis, LDA, and/or sentiment analysis




### 2. Linear Regression

housePrices <- read.csv("Data/HousePrices.csv")

## Interpret the meaning of all three beta coefficients.
## Discuss the statistical significance of the beta coefficients.
## What is the value and meaning of the coefficient of determination (R-squared)?
## Predict the value of a house price with 2 bathrooms and bricks.



### 3.1 Association
library(arules)

lastfm <- read.csv("Data/lastfm.csv")
str(lastfm)
head(lastfm)

## Write the number of records and variables. (289955 records, 4 variables)
dim(lastfm)

## Unique countries, users and artists (159 coutries, 15000 users, 1004 artists)
length(unique(lastfm$country))
length(unique(lastfm$user))
length(unique(lastfm$artist))

## Select records 200 and 201 and get info.
lastfm[200:201, ]

## Make an incidence matrix: user X artist
lastfm$user <- as.factor(lastfm$user)
incMatrix <- table(lastfm$user, lastfm$artist)
dim(incMatrix)

## Use itemFrequency() to list the support.
## Present a horizontal barplot
## Report the quartiles of item relative frequency.
## Find some rules.
## Inspect the rules.
## Explain their meaning.


# 3.2 Classification
## Analyze a decision tree and confusion matrix for CarSeats data.