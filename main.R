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




########### Clean data###############
df_tmp <- df_train[1:500,]

df_tmp_clean <- df_tmp %>%
  mutate(comment_text = str_replace_all(comment_text, "[\r\n]", " ")) %>%
  mutate(comment_text = tolower(comment_text)) %>%
  mutate(comment_text = gsub("[[:punct:]]","",comment_text)) %>%
  mutate(comment_text = str_replace_all(comment_text,stopwords_regex,"")) %>%
  mutate(comment_text = gsub("\\s+", " ", comment_text)) %>%
  mutate(comment_text = removeNumbers(comment_text)) %>%
  mutate(comment_text = stemDocument(comment_text))

  
#mutate(comment_text = text_tokens(comment_text, "en"))

####### Create corpus ###############
comment_text <- df_tmp_clean$comment_text
comment_source <- VectorSource(comment_text)
comment_corpus <- VCorpus(comment_source)

comments_dtm <- DocumentTermMatrix(comment_corpus)
comments_m <- as.matrix(comments_dtm)


comments_m_sparse <- removeSparseTerms(comments_dtm,0.98)
comments_m_sparse
comments_m <- as.matrix(comments_m_sparse)
dim(comments_m)

###############



