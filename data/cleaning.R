library(tidyverse)
library(tidytext)
theme_set(theme_minimal())

scraped_posts <- read_csv('inputs/scraped_posts_test.csv')

# combine title and body
scraped_posts$text <- paste0(scraped_posts$title, " ", scraped_posts$body)

# add hour column
scraped_posts$hour <- lubridate::hour(scraped_posts$date)




# scratch work ------------------------------------------------------------

long_tokens <- scraped_posts %>%
  dplyr::select(id, score, flair, hour, text) %>%
  tidytext::unnest_tokens(word, text)

# text length vs score
long_tokens %>% 
  group_by(id, score) %>% 
  tally() %>% 
  ggplot(aes(x = n, y = score)) +
  geom_point(alpha = 0.3)

# top 20 words
top_20_words <- long_tokens %>% 
  anti_join(y = select(stop_words, word), by = 'word') %>% 
  group_by(word) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 20)

# dummy code the top 20 words within the training and testing dfs
scraped_posts_top <- scraped_posts %>% 
  select(id, score, hour, text) %>% #flair
  mutate(index = row_number()) %>% 
  tidytext::unnest_tokens(word, text, drop = FALSE) %>% 
  nest(word) %>% 
  mutate(contains_top_20_word = map(data, function(df){
    tibble(match = top_20_words$word %in% df$word) %>% 
      t() %>% 
      as_tibble() %>% 
      setNames(top_20_words$word)
  })) %>% 
  unnest_wider(contains_top_20_word) %>% 
  select(-data, -index)

# scale hour
scraped_posts_top$hour <- scale(scraped_posts_top$hour)

# split the data into train and testing sets
tt_split <- 0.8
posts_training <- scraped_posts_top[1:(nrow(scraped_posts_top)*tt_split),]
posts_testing <- anti_join(scraped_posts_top, posts_training)

# fit the knn model on the train set
ks <- c(2:15, 20, 30, 40, 50)
k_search <- lapply(ks, function(k){
  posts_train_knn <- FNN::knn.reg(
    train = select(posts_training,-id,-score,-text),
    test = select(posts_testing,-id,-score,-text),
    y = posts_training$score,
    k = k
  )
})

# calculate RMSE
RMSE <- function(y, y_hat) sqrt(mean((y - y_hat)^2))
sapply(k_search, function(knn) RMSE(posts_testing$score, knn$pred)) %>% 
  enframe() %>% 
  mutate(k = ks) %>% 
  ggplot(aes(x = k, y = value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = ks) +
  labs(x = 'k', 
       y = 'RMSE')
  