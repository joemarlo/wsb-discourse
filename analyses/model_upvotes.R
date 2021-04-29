library(tidyverse)
library(tidymodels)
library(tidytext)
source('analyses/helpers.R')
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in the processed data
comments <- read_csv("data/comments_prepped.csv")


# create document term matrix ---------------------------------------------

# First, unnest comments to tokens and remove stop words:
comments_tokenized <- comments %>% 
  select(id_comment, comment_text) %>% 
  unnest_tokens(word, comment_text) %>% 
  anti_join(stop_words, by = 'word')

# Count the occurrences of each word in each document (comment):
comments_counts <- comments_tokenized %>% 
  group_by(id_comment, word) %>% 
  summarise(count = n(),
            .groups = 'drop')

# create bag-of-words matrix but remove words with less than 5000 total mentions
comments_dtm <- comments_counts %>% 
  group_by(word) %>% 
  mutate(count_overall = sum(count)) %>% 
  ungroup() %>% 
  filter(count_overall >= 5000) %>% 
  select(-count_overall) %>% 
  pivot_wider(values_from = count, names_from = word)

# add back to original dataframe
comments <- comments %>% 
  left_join(comments_dtm, by = 'id_comment', suffix = c("_remove_", "")) %>% 
  select(-ends_with("_remove_"))

# replace NAs with zeros
replacement_dict <- colnames(comments_dtm)[-1]
replacement_dict <- rep(0, length(replacement_dict)) %>% 
  as.list() %>% 
  setNames(replacement_dict)
comments <- replace_na(comments, replacement_dict)

# memory management
rm(comments_tokenized, comments_counts, comments_dtm, replacement_dict)


# clean up dataframe ------------------------------------------------------

# drop comment_text column
comments <- comments %>% 
  select(-comment_text)


# create train-test split --------------------------------------------------

# set initial splits for train, validate test
split_ratios <- c(0.6, 0.2, 0.2)

split_data <- function(tbl, splits){
  # function creates train, test, validate sets for a given dataset
  
  # create train dataset
  indices_train <- sample(c(TRUE, FALSE), 
                          size = nrow(tbl), 
                          replace = TRUE, 
                          prob = c(splits[1], 1-splits[1]))
  train_df <- tbl[indices_train,]
  train_df$type <- 'train'
  
  # create validate dataset
  indices_validate <- sample(c(TRUE, FALSE), 
                             size = sum(!indices_train), 
                             replace = TRUE,
                             prob = c(split_ratios[2], split_ratios[3]))
  validate_df <- tbl[!indices_train,][indices_validate,]
  validate_df$type <- 'validate'
  
  # create test dataset
  test_df <- tbl[!indices_train,][!indices_validate,]
  test_df$type <- 'test'
  
  return(bind_rows(train_df, validate_df, test_df))
}

# stratify by month
comments_split <- comments %>% 
  group_by(month = lubridate::month(datetime)) %>% 
  group_split() %>% 
  map(function(tbl) split_data(tbl, split_ratios)) %>% 
  bind_rows()

# verify it worked
comments_split %>% 
  group_by(month, type) %>% 
  tally() %>% 
  mutate(prop = n / sum(n))
table(comments_split$type) / sum(table(comments_split$type))

# replace emoji in column names
colnames(comments_split)[54:58] <- c('diamond_hands_emoji', "rocket_emoji", "gay_bear_emoji", "ape_emoji", "strong_emoji")
colnames(comments_split) <- janitor::make_clean_names(colnames(comments_split))

# create final datasets
comments_train <- filter(comments_split, type == 'train')
comments_validate <- filter(comments_split, type == 'validate')
comments_test <- filter(comments_split, type == 'test')

# drop unneccessary columns
cols_to_drop <- c('date', 'datetime', 'type', 'id_post', 'id_comment', 'id_parent')
comments_train <- select(comments_train, -any_of(cols_to_drop))
comments_validate <- select(comments_validate, -any_of(cols_to_drop))
comments_test <- select(comments_test, -any_of(cols_to_drop))

# memory management
rm(cols_to_drop)


# linear regression -------------------------------------------------------

model_lm <- lm(score ~ ., data = comments_train)
broom::tidy(model_lm)


# lasso -------------------------------------------------------------------

# glmnet


# knn ---------------------------------------------------------------------

# dummycode and scale the data
train_knn <- comments_train %>% 
  select(-score) %>% 
  fastDummies::dummy_cols(c('hour', 'wday', 'flair', 'topic'),
                          remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>% 
  mutate(across(everything(), ~scale(.x)[,1]))

# TODO remove NAs, grid search
# fit knn via cross validation  
model_knn <- FNN::knn.reg(
  train = train_knn, 
  y = comments_train$score,
  k = 5
)


# decision tree -----------------------------------------------------------

model_tree <- rpart::rpart(
  score ~ .,
  data = comments_train
)


# random forest -----------------------------------------------------------

model_rf <- ranger::ranger(
  score ~ .,
  data = comments_train,
  num.trees = 500,
  respect.unordered.factors = TRUE,
  num.threads = 4L
)


# boosting ----------------------------------------------------------------

# TODO: dummy code
model_xgb <- xgboost::xgboost(
  data = select(comments_train, -score),
  label = comments_train$score,
  nthread = 4L,
  nrounds = 2,
  objective = 'regression'
)


# RNN ---------------------------------------------------------------------





# comparison --------------------------------------------------------------

# make predictions
y_hat_lm <- predict(model_lm, newdata = comments_validate)
y_hat_tree <- predict(model_tree, newdata = comments_validate)
y_hat_rf <- predict(model_rf, data = comments_validate)$predictions

# RMSE
y_hats <- list(y_hat_lm, y_hat_tree, y_hat_rf)
y_names <- c("Linear model", "Decision tree", "Random forest")
map(y_hats, function(y_hat) RMSE(comments_validate$score, y_hat)) %>% 
  setNames(y_names) %>% 
  enframe() %>% 
  unnest(value) %>% 
  ggplot(aes(x = value, y = reorder(name, value))) +
  geom_col() +
  geom_text(aes(label = round(value, 2)),
            hjust = 1.1, color = 'white', fontface = 'bold') +
  labs(title = "RMSE by model",
       subtitle = 'Lower is better',
       x = "RMSE",
       y = NULL)
