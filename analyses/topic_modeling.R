# Topic Modeling

library(tidyverse)
library(tidytext)
library(topicmodels)
library(ldatuning)

# Read in cleaned comments data:
data <- read.csv('data/comments_cleaned.csv')
data$comment_text <- as.character(data$comment_text)
data$id_comment <- as.character(data$id_comment)
comments <- data %>% select(id_comment, comment_text)

# Create document-term matrix:

  # First, unnest comments to tokens and remove stop words:
  tokens_cmt <- comments %>% unnest_tokens(word, comment_text)
  tokens_cmt <- as_tibble(anti_join(tokens_cmt, stop_words, by = "word"))
  
  # Count the occurences of each word in each document (comment):
  tokens_cmt <-tokens_cmt %>% group_by(id_comment, word) %>% summarise(count = n())
  
  tokens_cmt %>% count(word, sort = T)

  # Create document-term matrix:
  tokens_dtm <- tokens_cmt %>% cast_dtm(id_comment, word, count)
  
# Fitting topic model with 5 topics:
comments_lda <- LDA(tokens_dtm, k = 5, control = list(seed = 1234))
  
# Examine the words in each topic:
  comments_topics <- tidy(comments_lda, matrix = 'beta')
  
  top_terms <- comments_topics %>% group_by(topic) %>%
    top_n(10, beta) %>% ungroup() %>%
    arrange(topic, -beta)

  # Plot top 10 terms in each topic:
 plot_5 <- top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free", ncol = 2) +
    coord_flip()
 
 ggsave(plot_5, file = 'analyses/plots/topics_5.png')
  

# Topic Assignments for each comment:
  
  comments_topic_label <- tidy(comments_lda, matrix = 'gamma')
  comments_topic_label <- comments_topic_label %>% group_by(document) %>% filter(gamma == max(gamma))
  
# Pull labels and write to csv:
  labels <- data %>% full_join(comments_topic_label %>% select(-gamma), 
                               by = c('id_comment' = 'document')) %>% 
    select(id_comment, topic) %>% replace_na(list(topic=6))
  
  write.csv(labels, file = "data/topic.csv")
  
  
### Notes:
  
# Need to adjust stop_words to recognize "i'm" and "it's"
# Need to remove weblinks in comment text

