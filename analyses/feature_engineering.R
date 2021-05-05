library(tidyverse)
library(tidytext)
source("analyses/plots/ggplot_settings.R")

comments <- read_csv("data/comments_cleaned.csv")
sentiment <- read_csv("data/sentiment.csv")
topics <- read_csv('data/topic.csv')

# join the data
comments <- comments %>% 
  left_join(sentiment, by = 'id_comment') %>% 
  left_join(select(topics, id_comment, topic), by = 'id_comment')
rm(sentiment, topics)

# remove all NAs
comments <- na.omit(comments)

# retain variables only available at time of comment
comments <- select(comments, -total_awards_received)


# holiday -----------------------------------------------------------------

# add a column dummy coding is the market is open
market_holidays <- c('2020-12-25', '2021-01-01', '2021-01-18', '2021-02-15', '2021-04-01', '2021-05-31')
market_holidays <- as.Date(market_holidays)
comments$is_market_day <- (comments$date %notin% market_holidays) & (comments$wday %notin% c("Sat", "Sun"))


# add comment hierarchy flag ----------------------------------------------

# add flag if comment is top level or not
# t3 prefix indicates if comment is top level
comments$is_direct_comment <- stringr::str_sub(comments$id_parent, 0, 2) == 't3'


# identify key phrases ----------------------------------------------------

# TODO: revisit ones emojis are fixed
# TODO: added stemmed versions to the key phrases list

# read in key phrases
key_phrases <- read_csv("data/wsb_language.csv")

# determine which comments contain which phrases
comments <- comments %>% 
  rowwise() %>% 
  mutate(match = list(str_count(comment_text, regex(key_phrases$phrase, case = FALSE)))) %>% 
  ungroup()

# add dummy code if text contains this phrase
comments <- comments %>%
  mutate(match = map(match, function(x){
    names(x) <- key_phrases$phrase
    return(x)
  })) %>% 
  unnest_wider(col = match)
  

# wish list ---------------------------------------------------------------

# GME stock price
# identify stocks



# write out
# write_csv(comments, "data/comments_prepped.csv")
