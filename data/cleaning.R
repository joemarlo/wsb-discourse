library(tidyverse)
source('analyses/plots/ggplot_settings.R')

posts <- read_csv('inputs/posts_raw.csv')
comments <- read_csv('inputs/comments_raw.csv')


# cleaning ----------------------------------------------------------------

# only limit to posts with Discussion-like posts;
flairs_to_keep <- c("DD", "Daily Discussion", "Discussion", "News", 
                    "GME Thread", "Fundamentals", "Options", 'Technicals', 
                    'Stock', "Technical Analysis")
posts <- posts[posts$link_flair_text %in% flairs_to_keep,]

# only limit to posts to with gamestop (and equivalents) in the title
GME_words <- "GME|gamestop|Gamestop|GameStop|gamestonk|Gamestonk|GameStonk"
posts <- posts[str_detect(posts$title, GME_words),]

# we're only going to examine comments
comments_cleaned <- comments %>% 
  mutate(link_id = str_sub(link_id, start = 4)) %>% 
  inner_join(dplyr::select(posts, id, link_flair_text), 
             by = c("link_id" = "id"))

# remove deleted or removed comments
comments_cleaned <- comments_cleaned[!(comments_cleaned$body %in% c('[removed]', '[deleted]')),]

# remove obvious bots
comments_cleaned <- comments_cleaned[!str_detect(comments_cleaned$body, 'I am a bot'),]

# add datetime, date, hour, and weekday
comments_cleaned$datetime <- lubridate::as_datetime(comments_cleaned$created_utc)
comments_cleaned$date <- as.Date(comments_cleaned$datetime)
comments_cleaned$hour <- lubridate::hour(comments_cleaned$datetime)
comments_cleaned$wday <- lubridate::wday(comments_cleaned$datetime, label = TRUE)

# rename and organize columns; drop created_utc; drop top_award_type
comments_cleaned <- select(
  comments_cleaned,
  datetime,
  date,
  hour,
  wday,
  id_post = link_id,
  id_comment = id,
  id_parent = parent_id,
  flair = link_flair_text,
  comment_text = body,
  score,
  total_awards_received
)

writeLines(paste0("Total comments: ", scales::comma_format()(nrow(comments_cleaned))))

# write out
write_csv(comments_cleaned, 'data/comments_cleaned.csv')
