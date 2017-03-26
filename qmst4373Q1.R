
# 1. Text Mining
library(tm)
library(SnowballC)
library(wordcloud)
library(igraph)
library(topicmodels)

## Retrieve data, remove stopwords and punctuation
docs <- Corpus(DirSource("Data/Aristotle_corpus/"))
docs

# Create the toSpace content transformer, remove non-standard punctuation
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")

# Remove punctuation, numbers, whitespace, transform to lowercase
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs,content_transformer(tolower))

# Stemming (useful but potentially opaque)
#docs <- tm_map(docs,stemDocument)

# Lemmalization
docs <- tm_map(docs, content_transformer(gsub), pattern = "things", replacement = "thing")
docs <- tm_map(docs, content_transformer(gsub), pattern = "parts", replacement = "part")
docs <- tm_map(docs, content_transformer(gsub), pattern = "animals", replacement = "animal")
docs <- tm_map(docs, content_transformer(gsub), pattern = "belongs", replacement = "belong")


# Remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove corpus specific stopwords
myStops <- c("also", "thing", "will", "must", "like", "first", "part", "since", "therefore", "thus", "case", "either", "neither", "something", "said", "even", "many", "whether", "just", "without", "though", "another", "every", "well", "come")
docs <- tm_map(docs, removeWords, myStops)

# Create the document term matrix
# inspect() takes input DTM[document_index, word_index]
dtm <- DocumentTermMatrix(docs)
inspect(dtm)

# Enforce word length and document bounds
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20), bounds = list(global = c(3,28))))
dtmr

## Plot 30 words with highest frequencies
freqs <- sort(colSums(as.matrix(dtmr)))
swf_len <- length(freqs)
top30 <- freqs[(swf_len-29):swf_len]
barplot(top30, horiz = TRUE, las = 1, col = "lightblue", 
        xlab = "Term Frequencies",
        main = "Main title")

## Construct a wordcloud (requires pckg "wordcloud")
set.seed(1)
wordcloud(names(freqs), freqs, min.freq=750, colors=brewer.pal(6,"Dark2"))


## Construct a network graph (requires pckg "igraph")
# termMatrix <- freqs%*%t(freqs)
# Network graph code keeps crashing R, no clue why. I will come back to this.

## Apply LDA, cluster analysis, and/or sentiment analysis

## Topic Modeling / LDA
library(topicmodels)

# Run LDA using Gibbs sampling
ldaOut <-LDA(dtmr, k = 5, method="Gibbs")

# Results
topics(ldaOut)
terms(ldaOut, 10)

# Probabilities associated with each topic assignment
# Rows are documents, columns are topics
topicProbabilities <- as.data.frame(ldaOut@gamma)
row.names(topicProbabilities) <- row.names(dtmr)
topicProbabilities

# Find relative importance of the top 2 topics according to each document
# The given topic is n times more likely to be fall under a topic than the next most [probabilistically] relevant topic (as given by column 2 of each list entry)

relativeImportance_1st <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

relativeImportance_2nd <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])



### 2. Linear Regression

housePrices <- read.csv("Data/HousePrices.csv")
str(housePrices)

m1 <- lm(Price ~ Bathrooms + Brick, data = housePrices)
summary(m1)

## Interpret the meaning of all three beta coefficients.
# 

## Discuss the statistical significance of the beta coefficients.
# All significant yes.

## What is the value and meaning of the coefficient of determination (R-squared)?
# R-squared = 0.4095 which means that only about 40% of the price variation can be explained by a linear model which uses only the number of bathrooms and presence of brick as predictors

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