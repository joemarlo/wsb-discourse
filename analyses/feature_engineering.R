library(tidyverse)
library(tidytext)

comments <- read_csv("data/comments_cleaned.csv")
sentiment <- read_csv("data/sentiment.csv")
topics <- read_csv('data/topics.csv')

# join the data
comments <- comments %>% 
  left_join(sentiment, by = 'id_comment') %>% 
  left_join(select(topics, id_comment, topic), by = 'id_comment')
rm(sentiment, topics)

# remove all NAs
comments <- na.omit(comments)

# retain variables only available at time of comment
comments <- select(comments, -total_awards_received)


# identify key phrases ----------------------------------------------------

# read in key phrases
key_phrases <- read_csv("data/wsb_language.csv")

# determine which comments contain which phrases
comments <- comments %>% 
  rowwise() %>% 
  mutate(match = list(str_match_all(comment_text, regex(key_phrases$phrase, case = FALSE)))) %>% 
  ungroup()

# add dummy code if text contains this phrase
comments <- comments %>% 
  mutate(match = map(match, function(x){
    names(x) <- key_phrases$phrase
    return(x)
  })) %>% 
  unnest_wider(col = match)
  
# convert values to 0:1



# bag of words ------------------------------------------------------------




# write out
write_csv("data/comments_prepped.csv")
