library(tidyverse)
library(tidytext)
source('analyses/plots/ggplot_settings.R')

posts <- read_csv("inputs/posts_raw.csv")
comments <- read_csv("inputs/comments_raw.csv")
# posts <- read_csv('data/posts.csv')
# comments <- read_csv('data/comments.csv')

flairs_to_keep <- c("Discussion", "Daily Discussion", "DD", "Technical Analysis", "YOLO", "News", "Gain")
# posts <- posts_raw %>% 
#   filter(link_flair_text %in% flairs_to_keep)

posts$datetime <- lubridate::as_datetime(posts$created_utc)
posts$date <- as.Date(posts$datetime)
posts$hour <- lubridate::hour(posts$datetime)
posts$wday <- lubridate::wday(posts$datetime, label = TRUE)

comments$datetime <- lubridate::as_datetime(comments$created_utc)
comments$date <- as.Date(comments$datetime)
comments$hour <- lubridate::hour(comments$datetime)
comments$wday <- lubridate::wday(comments$datetime, label = TRUE)


# frequencies -------------------------------------------------------------

# posts and comments over time
posts %>% 
  group_by(date) %>% 
  tally(name = 'posts') %>% 
  full_join(comments %>%
              group_by(date) %>%
              tally(name = "comments"),
            by = 'date') %>%
  pivot_longer(-date) %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_col(color = 'white') +
  scale_x_date(date_breaks = '2 week',
               date_labels = '%b-%d') +
  scale_y_continuous(labels = scales::comma_format()) +
  facet_wrap(~name, ncol = 1, scales = 'free_y') +
  labs(title = "Count of daily posts and comments on r/wallstreetbets",
       x = NULL,
       y = NULL)
# ggsave("analyses/plots/raw_daily_counts.png", height = 6, width = 7)

# posts and comments over time of day
posts %>% 
  group_by(hour) %>% 
  tally(name = 'posts') %>% 
  full_join(comments %>%
              group_by(hour) %>%
              tally(name = "comments"),
            by = 'hour') %>%
  pivot_longer(-hour) %>% 
  ggplot(aes(x = hour, y = value)) + 
  geom_col(color = 'white') +
  scale_x_continuous(breaks = 0:23,
                     labels = c("12am", 1:11, "12pm", 1:11)) +
  scale_y_continuous(labels = scales::comma_format()) +
  facet_wrap(~name, ncol = 1, scales = 'free_y') +
  labs(title = "Count of hourly posts and comments on r/wallstreetbets",
       caption = 'Hour in UTC time',
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))
# ggsave("analyses/plots/raw_hourly_counts.png", height = 6, width = 7)

# posts and comments by day of week
posts %>% 
  group_by(wday) %>% 
  tally(name = 'posts') %>%
  full_join(comments %>%
              group_by(wday) %>%
              tally(name = "comments"),
            by = 'wday') %>%
  pivot_longer(-wday) %>% 
  ggplot(aes(x = wday, y = value)) + 
  geom_col(color = 'white') +
  scale_y_continuous(labels = scales::comma_format()) +
  facet_wrap(~name, ncol = 1, scales = 'free_y') +
  labs(title = "Count of weekday posts and comments on r/wallstreetbets",
       caption = 'Hour in UTC time',
       x = NULL,
       y = NULL)
# ggsave("analyses/plots/raw_wday_counts.png", height = 6, width = 7)

# proportion of posts mentioning GME in the title
GME <- "GME|gamestop|gamestonk"
posts %>% 
  mutate(mentions_GME = stringr::str_detect(title, GME)) %>% 
  group_by(date, mentions_GME) %>%
  tally() %>% 
  ggplot(aes(x = date, y = n, fill = mentions_GME)) +
  geom_col(position = 'stack') +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip()

# upvotes by time of day
posts %>% 
  mutate(type = 'posts') %>% 
  bind_rows(mutate(comments, type = 'comments')) %>% 
  group_by(type, hour) %>% 
  summarize(metric = as.numeric(quantile(score)),
            .groups = 'drop') %>% 
  mutate(quantile = factor(rep(seq(0, 1, 0.25), 24*2))) %>% 
  ggplot(aes(x = hour, y = metric, group = quantile, color = quantile)) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = 0:23,
                     labels = c("12am", 1:11, "12pm", 1:11)) +
  scale_y_log10() +
  facet_wrap(~type, ncol = 1, scales = 'free_y')


# top words ---------------------------------------------------------------

# top emojis
emoji <- str_extract_all(comments$body,"[^[:ascii:]]")
emoji <- emoji[!(emoji %in% c("’", " ", '”', '“'))]
emoji_counts <- unlist(emoji) %>% table() %>% sort()
emoji_counts <- emoji_counts[emoji_counts > 10000]
ggplot(enframe(emoji_counts),
       aes(x = reorder(name, -value), y = value)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = 'Most common emojis within comments',
       x = "Emoji",
       y = 'n')

# tokenize the words
long_tokens <- comments %>%
  slice_sample(n = 50000) %>% 
  dplyr::select(link_id, id, score, date, hour, wday, body) %>%
  tidytext::unnest_tokens(word, body)

# text length vs score
long_tokens %>% 
  group_by(id, score) %>% 
  tally() %>% 
  ggplot(aes(x = n, y = score)) +
  geom_point(alpha = 0.1) +
  labs(title = "Comment's length vs score",
       subtitle = "Random sample of 15k comments",
       x = "n words",
       y = 'Score')

# top 10 words
long_tokens %>% 
  anti_join(y = select(stop_words, word), by = 'word') %>% 
  group_by(word) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 20) %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_col() +
  labs(title = "Most common words within comments",
       x = NULL) +
  theme(axis.text.x = element_text(angle = -40, hjust = 0))

# top tri-grams
comments %>%
  slice_sample(n = 50000) %>% 
  filter(!str_detect(body, "I am a bot")) %>% 
  dplyr::select(link_id, id, score, date, hour, wday, body) %>%
  tidytext::unnest_tokens(word, body, token = "ngrams", n = 3) %>% 
  group_by(word) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 100) %>% 
  na.omit() %>% 
  View()


# how many comments are we missing ----------------------------------------

comment_tally <- comments %>% 
  group_by(id = link_id) %>% 
  tally() %>% 
  mutate(id = str_sub(id, start = 4))
posts %>% 
  select(id, num_comments) %>% 
  left_join(comment_tally, by = 'id') %>% 
  replace_na(list(n = 0)) %>% 
  mutate(diff = num_comments - n) %>% 
  arrange(desc(diff)) %>% 
  View
