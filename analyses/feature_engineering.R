library(tidyverse)
library(tidytext)
source('analyses/helpers.R')
source("analyses/plots/ggplot_settings.R")

# read in all the data
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


# identify emojis ---------------------------------------------------------
# takes ~5min

# extract all emojis and place into a list within the column
comments$emoji_ascii <- str_extract_all(comments$comment_text,"[^[:ascii:]]")

# remove non-emoji characters we accidentally caught like apostrophes
#   and convert to plain text
comments <- comments %>% 
  mutate(emoji_cleaned = map(emoji_ascii, function(emoji){
    
    # remove unwanted characters like apostrophes
    char_to_remove <- c("“", "’", "”", "”", "”", "—")
    emoji_list <- setdiff(emoji, char_to_remove)
    
    # convert to plain text
    emoji_list <- textclean::replace_emoji(emoji_list)
    
    # remove emoji's that could not be translated
    emoji_list <- emoji_list[!str_detect(emoji_list, "<")]
    
    return(emoji_list)
  }))

# create list of top emojis
top_emojis <- comments$emoji_cleaned %>% 
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  .[1:20] %>% 
  enframe()

# add columns if comment contains an emoji or a specific top emoji
comments <- comments %>% 
  rowwise() %>% 
  mutate(contains_emoji = isTRUE(length(emoji_cleaned) > 0),
         emoji = list(str_detect(paste0(emoji_cleaned, collapse = ""), top_emojis$name))) %>% 
  ungroup()

# clean up emoji names
top_emojis$name <- janitor::make_clean_names(top_emojis$name)
top_emojis$name <- paste0("emoji_", top_emojis$name)

# add dummy code if text contains this phrase
comments <- comments %>%
  mutate(emoji = map(emoji, function(x){
    names(x) <- top_emojis$name
    return(x)
  })) %>% 
  unnest_wider(col = emoji)

# remove unnecessary columns
comments <- select(comments, -emoji_cleaned, -emoji_ascii)

# write out top emojis
write_csv(top_emojis, "data/top_emojis.csv")
rm(top_emojis)


# identify numbers --------------------------------------------------------

# add new column denoting if comment contains a digit
comments$contains_number <- str_detect(comments$comment_text, "\\d")


# identify key phrases ----------------------------------------------------

# read in key phrases
key_phrases <- read_csv("data/wsb_language.csv")

# determine which comments contain which phrases
comments <- comments %>% 
  rowwise() %>% 
  mutate(match = list(str_count(comment_text, regex(key_phrases$phrase, case = FALSE)))) %>% 
  ungroup()

# clean up phrases
key_phrases$phrase <- textclean::replace_emoji(key_phrases$phrase)
key_phrases$phrase[key_phrases$phrase == "<f0><9f><a6><8d>"] <- "ape_emoji"
key_phrases$phrase <- janitor::make_clean_names(key_phrases$phrase)
key_phrases$phrase <- paste0("keyphrase_", key_phrases$phrase)

# add dummy code if text contains this phrase
comments <- comments %>%
  mutate(match = map(match, function(x){
    names(x) <- key_phrases$phrase
    return(x)
  })) %>% 
  unnest_wider(col = match)
rm(key_phrases)

# contains gamestop -------------------------------------------------------

# add flag if comment contains any drect reference to gamestop
comments$contains_GME <- str_detect(comments$comment_text, regex("GME|gamestop|gamestonk", case = FALSE))


# wish list ---------------------------------------------------------------

# GME stock price
# identify stocks


# write out ---------------------------------------------------------------
# write_csv(comments, "data/comments_prepped.csv")
