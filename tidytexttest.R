### Source ####

#https://www.tidytextmining.com/sentiment.html

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

df_train$rowSum <- rowSums(df_train[,3:8])
df_train$classification1 <- ifelse(df_train$rowSum==0,'ham', 'spam')
                                   

text_df <- df_train %>%
  select(id,comment_text, classification1) %>%
  rename(word = comment_text) %>%
  #mutate(word = str_extract(word, "[a-z']+")) %>%
  #head(10000) %>%
  unnest_tokens(word,word) 


tidy_comment <- text_df %>%
  anti_join(stop_words)


######## Data exploration ############
tidy_comment %>%
  count(word, sort = TRUE) %>%
  filter(n>500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

tidy_class <-tidy_comment %>%
  count(word, classification1, sort = TRUE) %>%
  ungroup()
  
tidy_class %>% 
  group_by(classification1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%

  ggplot(aes(word, n, fill = classification1)) +
  geom_col(show.legend = F) +
  facet_wrap(~classification1, scales = "free_y") +
  labs(y = "Contribution to classification", x = NULL) +
  coord_flip()




