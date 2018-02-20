### Load libraries ###############

library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
#library(qdap) #problem with java re 

### Load data################

df_train <- read.csv("data/train.csv", sep = ",", stringsAsFactors = F)

# http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html



# Stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')


### Creating clean corpus ####

comment_corpus <- Corpus(VectorSource(df_train$comment_text))

corpus_clean <- tm_map(comment_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean, stemDocument, language = "english")  


#### Document Term Matrix ####

comments_dtm <- DocumentTermMatrix(corpus_clean)
#comments_m <- as.matrix(comments_dtm)
comments_m_sparse <- removeSparseTerms(comments_dtm,0.98)
comments_m_sparse
comments_m <- as.matrix(comments_m_sparse)
dim(comments_m)

###############



