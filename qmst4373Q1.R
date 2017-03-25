
# 1. Text Mining
library(tm)
library(SnowballC)
library(wordcloud)
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

## Construct a wordcloud
set.seed(1)
wordcloud(names(sortedWordFreqs), sortedWordFreqs, min.freq=70, colors=brewer.pal(6,"Dark2"))

## Construct a network graph


# 2. Linear Regression
## Interpret the meaning of all three beta coefficients.
## Discuss the statistical significance of the beta coefficients.
## What is the value and meaning of the coefficient of determination (R-squared)?
## Predict the value of a house price with 2 bathrooms and bricks.


# 3.1 Association
## Write the number of records and variables.
## Unique countries, users and artists
## Select records 200 and 201 and get info.
## Make an incidence matrix.
## Use itemFrequency() to list the support.
## Present a horizontal barplot
## Report the quartiles of item relative frequency.
## Find some rules.
## Inspect the rules.
## Explain their meaning.


# 3.2 Classification
## Analyze a decision tree and confusion matrix for CarSeats data.