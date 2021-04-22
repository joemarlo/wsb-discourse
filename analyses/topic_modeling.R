# Topic Modeling script

library(tidyverse)
library(tidytext)
library(topicmodels)

# Read in cleaned comments data:
data <- read.csv('data/comments_cleaned.csv')
comments$comment_text <- as.character(comments$comment_text)
comments$id_comment <- as.character(comments$id_comment)
comments <- comments %>% select(id_comment, comment_text)

# Create document-term matrix:

  # First, unnest comments to tokens and remove stop words:
  tokens_cmt <- comments %>% unnest_tokens(word, comment_text)
  tokens_cmt <- as_tibble(anti_join(tokens_cmt, stop_words, by = "word"))
  
  # Count the occurences of each word in each document (comment):
  tokens_cmt <-tokens_cmt %>% group_by(id_comment, word) %>% summarise(count = n())
  
  tokens_cmt %>% count(word, sort = T)

  # Create document-term matrix:
  tokens_dtm <- tokens_cmt %>% cast_dtm(id_comment, word, count)
  
# Fitting topic model with arbitrary 5 topics:
comments_lda <- LDA(tokens_dtm, k = 5, control = list(seed = 1234))
  
# Examine the words in each topic:
  comments_topics <- tidy(comments_lda, matrix = 'beta')
  
  top_terms <- comments_topics %>% group_by(topic) %>%
    top_n(10, beta) %>% ungroup() %>%
    arrange(topic, -beta)

  # Plot top 10 terms in each topic:
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free", ncol = 2) +
    coord_flip()
  

# Topic Assignments for each comment:
  
  comments_topic_label <- tidy(comments_lda, matrix = 'gamma')
  comments_topic_label <- comments_topic_label %>% group_by(document) %>% filter(gamma == max(gamma))
  

# Add labels to main data
  data <- full_join(data, comments_topic_label %>% select(-gamma), by = c('id_comment' = 'document'))
  

  
### Notes:
  
# Need to adjust stop_words to recognize "i'm" and "it's"
# Need to remove weblinks in comment text

