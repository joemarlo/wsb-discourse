library(tidyverse)
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

# add month column
comments$month <- lubridate::month(comments$datetime)

# drop unnecessary columns
cols_to_drop <- c('comment_text', 'date', 'datetime', 'type', 'id_post', 'id_comment', 'id_parent')
comments <- select(comments, -any_of(cols_to_drop))
rm(cols_to_drop)


# feature selection via LASSO ---------------------------------------------

# dummy code
vars_to_dummy <- c("wday", "hour", "flair", "topic")
comments <- fastDummies::dummy_cols(comments, vars_to_dummy, remove_selected_columns = TRUE)

# extract x and y values
x_vars_lasso <- as.matrix(comments[, setdiff(colnames(comments), "score")])
y_var_lasso <- comments$score
lambda_seq <- 10^seq(2, -2, by = -0.2)

# split into test train
train_lasso <- sample(1:nrow(comments), nrow(comments)/2)
test_lasso_x <- -train_lasso
test_lasso_y <- y_var_lasso[test_lasso_x]

# cross validate to find optimal lambda
cv_output <- glmnet::cv.glmnet(x_vars_lasso[train_lasso,], 
                               y_var_lasso[train_lasso],
                               alpha = 1, 
                               lambda = lambda_seq, 
                               nfolds = 5)

# retrieve optimal lambda
lambda_optimal <- cv_output$lambda.min

# build model with optimal lambda
lasso_best <- glmnet::glmnet(x_vars_lasso[train_lasso,], 
                             y_var_lasso[train_lasso], 
                             alpha = 1, 
                             lambda = lambda_optimal)
preds_lasso <- predict(lasso_best, s = lambda_optimal, newx = x_vars_lasso[test_lasso_x,])
# plot(test_lasso_y, preds_lasso)

# create dataframe of features selected from the LASSO method
cols_to_retain <- broom::tidy(lasso_best)
rm(preds_lasso, cv_output, lambda_optimal, train_lasso, test_lasso_x, 
   test_lasso_y, vars_to_dummy, x_vars_lasso, y_var_lasso, lasso_best, lambda_seq)

# retain only these variables
comments <- select(comments, all_of(c('score', cols_to_retain$term[-1])))


# write out ---------------------------------------------------------------

# write_csv(cols_to_retain, "data/lasso_features.csv")
# write_csv(comments, "data/comments_selected.csv")
