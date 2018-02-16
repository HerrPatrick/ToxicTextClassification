### Load libraries ###############

library(dplyr)
library(stringr)
library(tm)

### Load data################

df_train <- read.csv("data/train.csv", sep = ",", stringsAsFactors = F)


####### Text specifics ###############

# Stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')




########### Clean data###############
df_tmp <- df_train[3,]

df_tmp_clean <- df_tmp %>%
  mutate(comment_text = str_replace_all(comment_text, "[\r\n]", " ")) %>%
  mutate(comment_text = tolower(comment_text)) %>%
  mutate(comment_text = gsub("[[:punct:]]","",comment_text)) %>%
  mutate(comment_text = str_replace_all(comment_text,stopwords_regex,""))

