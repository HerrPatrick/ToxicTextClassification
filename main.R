### Load libraries ###############

library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
#library(qdap) #problem with java re 
library(e1071)
library(gmodels)
library(purrr)
library(zoo)

### Load data################

df_train <- read.csv("data/train.csv", sep = ",", stringsAsFactors = F)

# http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html


df_train$rowSum <- rowSums(df_train[,3:8])

for (i in 1:nrow(df_train)){
  df_train$class1[i] <- if(df_train$rowSum[i]==0) {'ham'} else {'spam'}
}

df_train$class1 <- as.factor(df_train$class1)

### Creating clean corpus ####

comment_corpus <- Corpus(VectorSource(df_train$comment_text))

corpus_clean <- tm_map(comment_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean, stemDocument, language = "english")  


#### Document Term Matrix ####

comments_dtm <- DocumentTermMatrix(corpus_clean, list(dictionary = ))
#comments_m <- as.matrix(comments_dtm)
comments_m_sparse<- removeSparseTerms(comments_dtm,0.995)
#comments_m_sparse
comments_m <- as.matrix(comments_m_sparse)
comments <- as.data.frame(comments_m)

#dim(comments_m)

##### Splitting into Train & Test sets #######

i = ceiling(nrow(df_train)*0.7)

df_train_model <- df_train[1:i, ]
df_test_model <- df_train[(i+1):nrow(df_train), ]



comments_train <- comments[1:i, ] 
comments_test <- comments[(i+1):nrow(df_train), ]

comment_corpus_train <- corpus_clean[1:i]
comment_corpus_test <- corpus_clean[(i+1):nrow(df_train)]
prop.table(table(df_train_model$rowSum))
prop.table(table(df_test_model$rowSum))

#comments_dict <- Terms(findFreqTerms(comments_train, 5)) 


##### Building model ########

comment_classifier <- naiveBayes(comments_train, df_train_model$class1)
comment_test_pred <- predict(comment_classifier, comments_test)

CrossTable(comment_test_pred, df_test_model$class1, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted','actual'))
