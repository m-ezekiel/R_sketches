
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

## Make an incidence matrix: user by artist

# Convert integer column to factors to isolate unique users
lastfm$user <- as.factor(lastfm$user)

playlist <- split(x = lastfm[ , "artist"], f = lastfm$user)
playlist <- lapply(playlist, unique)

# The 1st two listeners listen to the following bands:
playlist[1:2];

# Convert playlist to a tansactions object
playlist.trans <- as(playlist,"transactions")

## Use itemFrequency() to list the support.
# Support of (A and B) = proportion of transactions in the dataset that contain both A and B
# Note that semantic order matters so A = antecedent, B = consequent.
# Top 5 bands: Radiohead, The Beatles, Coldplay, RHCP, Muse
tail(sort(itemFrequency(playlist.trans)), 5)

## Present a horizontal barplot for bands with support >= .10 (10% of users)
itemFrequencyPlot(playlist.trans, support = 0.10, horiz = TRUE, las = 1, col = "lightblue",
                  main = "Bands that are popular with \nat least 10% of lastfm users")

## Report the quartiles of item relative frequency (support): {.00880, .01267, .02187}
pl.summaryTable <- summary(itemFrequency(playlist.trans))
pl.Q1 <- as.numeric(pl.summaryTable[2])
pl.median <- as.numeric(pl.summaryTable[3])
pl.Q3 <- as.numeric(pl.summaryTable[5])

## Find rules w/support > upperQuantile and confidence >= .40
# Confidence that an occurence of A will also contain B is conditional probability: 
# prob(B|A) = p(A and B) / p(A)
rules <- apriori(playlist.trans, parameter=list(minlen=1, support=pl.Q3, confidence = 0.40, target="rules"))
summary(rules) # 24 rules


## Inspect the rules
# Lift = Confidence / Expected_confidence
# Expected_confidence = proportion of total transactions containing the consequent (B)
inspect(rules)

## Select the 5 rules with the highest lift
highLiftRules <- head(sort(rules, by="lift"), 5)
inspect(highLiftRules)

## Select the rule with the highest confidence. 
highConfidenceRule <- sort(highLiftRules, by="confidence")[1]
inspect(highConfidenceRule)

## Specify values for support, confidence and lift. Explain their meaning.
# support = 0.02226; 2.226 % of users listen to both Keane and Coldplay.
# confidence = 0.6374; The probability that a fan of Keane will also listen to Coldplay is 0.6374.
# lift = 4.0206; Users who listen to Keane are 4 times more likely to listen to Coldplay compared to customers from the entire data set.


# 3.2 Classification
## Analyze a decision tree and confusion matrix for CarSeats data.