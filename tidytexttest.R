### Source ####

#https://www.tidytextmining.com/sentiment.html
# https://idc9.github.io/stor390/notes/natural_language_processing/text_classification.html
### Load libraries ###############

library(dplyr)
library(stringr)
library(tm)
library(tidytext)
library(ggplot2)
library(SnowballC)
library(klaR)
#library(qdap) #problem with java re 
#library(e1071)
#library(gmodels)


### Load data################

df_train <- read.csv("data/train.csv", sep = ",", stringsAsFactors = F)



data(stop_words)

#### Data wrangle ##############

df_train$rowSum <- rowSums(df_train[,3:8])
df_train$classification1 <- ifelse(df_train$rowSum==0,'ham', 'spam')
                                   

tidy_comment <- df_train %>%
  dplyr::select(id,comment_text, classification1) %>%
  rename(word = comment_text) %>%
  filter(!str_detect(word,"[0-9]")) %>%
  unnest_tokens(word,word)
  #%>%
  #mutate(word = wordStem(word)) %>%
  #anti_join(stop_words)

tidy_comment_n <- tidy_comment %>%
  group_by(id) %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  rename(count = n)

tidy_comment_n <- tidy_comment_n %>%
  bind_tf_idf(word, classification1, count)

bag_of_words_dtm <- tidy_comment_n %>%
  cast_dtm(classification1, word, count)

tfidf_dtm <- tidy_comment_n %>%
  cast_dtm(classification1, word, tf_idf)

tr_classes <- as.factor(c('ham','spam'))

X_bag_of_words <- as.matrix(bag_of_words_dtm)
X_tfidf <- as.matrix(tfidf_dtm)

bow_classifier <- nm(x = X_bag_of_words, grouping = tr_classes)
tfidf_classifier <- nm(x = X_tfidf,grouping = tr_classes)

bow_tr_pred <- predict(bow_classifier, newdata = X_bag_of_words)$class
tfidf_tr_pred <- predict(tfidf_classifier, newdata = X_tfidf)$class

# training error
paste0('bag of words based classifier training error: ', mean(tr_classes != bow_tr_pred))

paste0('tf-idf based classifier training error: ', mean(tr_classes != tfidf_tr_pred))

######## Data exploration ############
tidy_comment %>%
  count(word, sort = TRUE) %>%
  filter(n>5000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

tidy_class <-tidy_comment %>%
  count(word, classification1, sort = TRUE) %>%
  ungroup()

tidy_ham <- tidy_comment %>%
  filter(classification1=='ham') %>%
  count(word, sort = TRUE) %>%
  filter(n>500)
  
tidy_spam <- tidy_comment %>%
  filter(classification1=='spam') %>%
  count(word, sort = TRUE) 

test <- anti_join(tidy_spam,tidy_ham, by = "word")


  
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

########### Sentiment analysis #########
my_sentiment <- get_sentiments("afinn") 

tidy_spam %>%
  head(100) %>%
  inner_join(my_sentiment)

class_comments <- tidy_comment %>%
  count(classification1,word, sort = TRUE) %>%
  group_by(classification1) %>%
  ungroup()

total_words <- class_comments %>%
  group_by(classification1) %>%
  summarize(total = sum(n))

class_comments <- left_join(class_comments, total_words)

class_comments <- class_comments %>%
  bind_tf_idf(word, classification1,n)

class_comments %>% 
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  filter(classification1=='ham')


class_words_tfidf <- 
  
