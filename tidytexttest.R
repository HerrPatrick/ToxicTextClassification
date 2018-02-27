### Load libraries ###############

library(dplyr)
library(stringr)
library(tm)
library(tidytext)
library(ggplot2)
#library(SnowballC)
#library(qdap) #problem with java re 
#library(e1071)
#library(gmodels)


### Load data################

df_train <- read.csv("data/train.csv", sep = ",", stringsAsFactors = F)

data(stop_words)

#### Data wrangle ##############

text_df <- df_train %>%
  select(id,comment_text) %>%
  rename(word = comment_text) %>%
  head %>%
  unnest_tokens(word,word)


tidy_comment <- text_df %>%
  anti_join(stop_words)

tidy_comment %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

