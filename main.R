### Load libraries ###############

library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(qdap)

### Load data################

df_train <- read.csv("data/train.csv", sep = ",", stringsAsFactors = F)

# http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html
####### Create corpus ###############

comment_text <- df_train$comment_text
comment_source <- VectorSource(comment_text)
comment_corpus <- VCorpus(comment_source)

# Stopwords
#stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
#stopwords_regex = paste0('\\b', stopwords_regex, '\\b')




########### Clean data###############
df_tmp <- df_train[2,]

df_tmp_clean <- df_tmp %>%
  mutate(comment_text = str_replace_all(comment_text, "[\r\n]", " ")) %>%
  mutate(comment_text = tolower(comment_text)) %>%
  mutate(comment_text = gsub("[[:punct:]]","",comment_text)) %>%
  mutate(comment_text = str_replace_all(comment_text,stopwords_regex,"")) %>%
  mutate(comment_text = gsub("\\s+", " ", comment_text)) %>%
  mutate(comment_text = text_tokens(comment_text, "en"))

