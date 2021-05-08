library(tidyverse)
library(tidymodels)
source('analyses/helpers.R')
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in the processed data
comments <- read_csv("data/comments_selected.csv")


# balance the data --------------------------------------------------------

# plot distribution of scores
ggplot(comments, aes(x = score)) +
  geom_histogram(bins = 300) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Distribution of upvotes is highly imbalanced",
       x = 'Upvotes',
       y = 'n')
# ggsave('analyses/plots/imbalance.png', width = 8, height = 4)

# balance the data by stratify on binned scores and randomly sample 10k from each
comments <- comments %>% 
  mutate(score_bin = cut(score, breaks = c(-1000, 0, 5, 10, 100, 1000, 1e6))) %>% 
  group_by(score_bin) %>% 
  slice_sample(n = 10000) %>% 
  ungroup() %>% 
  select(-score_bin)

# plot distribution of scores
ggplot(comments, aes(x = score)) +
  geom_histogram(bins = 300) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Distribution of upvotes after balancing",
       x = 'Upvotes',
       y = 'n')
# ggsave('analyses/plots/balanced.png', width = 8, height = 4)


# create stratified train-test split --------------------------------------

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

# create train test split but stratify it by month
comments_split <- comments %>% 
  group_by(month) %>% 
  group_split() %>% 
  map(function(tbl) split_data(tbl, split_ratios)) %>% 
  bind_rows()

# verify it worked
comments_split %>% 
  group_by(month, type) %>% 
  tally() %>% 
  mutate(prop = n / sum(n))
table(comments_split$type) / sum(table(comments_split$type))

# clean up column names
comments_split <- janitor::clean_names(comments_split)

# create final datasets
comments_train <- filter(comments_split, type == 'train') %>% select(-type)
comments_validate <- filter(comments_split, type == 'validate') %>% select(-type)
comments_test <- filter(comments_split, type == 'test') %>% select(-type)


# linear regression -------------------------------------------------------

model_lm <- lm(score ~ ., data = comments_train)
broom::tidy(model_lm) %>% View


# lasso -------------------------------------------------------------------

# glmnet


# knn ---------------------------------------------------------------------

# grid search for optimal k value based on subset (for speed)
control_knn <- caret::trainControl(method = "repeatedcv", repeats = 3)
tune_knn <- caret::train(
  score ~ .,
  data = slice_sample(comments_train, n = 1000),
  method = "knn",
  trControl = control_knn,
  preProcess = c("center", "scale"),
  tuneLength = 20
)
plot(tune_knn)

# scale the data
train_knn <- comments_train %>% 
  select(-score) %>% 
  mutate(across(everything(), ~scale(.x)[,1]))
validate_knn <- comments_validate %>% 
  select(-score) %>% 
  mutate(across(everything(), ~scale(.x)[,1]))

# fit final knn model 
model_knn <- FNN::knn.reg(
  train = train_knn, 
  test = validate_knn,
  y = comments_train$score,
  k = 20
)


# decision tree -----------------------------------------------------------

# fit model
model_tree <- rpart::rpart(
  score ~ .,
  data = comments_train
)


# random forest -----------------------------------------------------------

# grid search for mtry
grid_rf <- expand.grid(mtry = c(5, 10, 12, 15, 20),
                       splitrule = 'variance',
                       min.node.size = 5)
control_rf <- caret::trainControl(method = "CV",
                                  number = 5,
                                  verboseIter = TRUE)
tune_rf <- caret::train(
  score ~ .,
  data = comments_train,
  method = 'ranger',
  num.trees = 100,
  tuneGrid = grid_rf,
  trControl = control_rf
)
plot(tune_rf$results$mtry, tune_rf$results$RMSE)

# fit final model
model_rf <- ranger::ranger(
  score ~ .,
  data = comments_train,
  num.trees = 1000,
  respect.unordered.factors = TRUE,
  num.threads = 4L,
  importance = 'impurity',
  mtry = 5
)

# variable importance
model_rf$variable.importance %>% 
  enframe() %>% 
  slice_max(order_by = value, n = 10) %>% 
  ggplot(aes(x = value, y = reorder(name, value))) +
  geom_col() +
  scale_x_continuous(labels = NULL) +
  labs(title = "Variable importance from random forest",
       x = "Importance",
       y = NULL)
# ggsave("analyses/plots/rf_importance", width = 6, height = 8)


# boosting ----------------------------------------------------------------

# grid search 
grid_xgb <- expand.grid(nrounds = 50,
                        max_depth = c(1, 5, 10, 15, 20),
                        eta = c(0.1, 0.4),
                        gamma = 0,
                        colsample_bytree = 0.7,
                        min_child_weight = 1,
                        subsample = c(0.8, 1))
tune_xgb <- caret::train(
  score ~ .,
  data = comments_train,
  method = 'xgbTree',
  nthread = 4L,
  objective = "reg:squarederror",
  tuneGrid = grid_xgb,
  trControl = control_rf
)
plot(tune_xgb$results$max_depth, tune_xgb$results$RMSE)

# fit final model
model_xgb <- xgboost::xgboost(
  data = xgboost::xgb.DMatrix(as.matrix(comments_train[,-1]),
                              label = comments_train$score), 
  objective = "reg:squarederror",
  nrounds = 100,
  max_depth = tune_xgb$bestTune$max_depth,
  eta = tune_xgb$bestTune$eta,
  gamma = tune_xgb$bestTune$gamma,
  colsample_bytree = tune_xgb$bestTune$colsample_bytree,
  min_child_weight = tune_xgb$bestTune$min_child_weight,
  subsample = tune_xgb$bestTune$subsample
)


# RNN ---------------------------------------------------------------------





# comparison --------------------------------------------------------------

# make predictions
y_hat_lm <- predict(model_lm, newdata = comments_validate)
y_hat_knn <- model_knn$pred
y_hat_tree <- predict(model_tree, newdata = comments_validate)
y_hat_rf <- predict(model_rf, data = comments_validate)$predictions
xgb_validate <- xgboost::xgb.DMatrix(data = as.matrix(comments_validate[,-1]))
y_hat_xgb <- predict(model_xgb, newdata = xgb_validate)

# RMSE
y_hats <- list(y_hat_lm, y_hat_knn, y_hat_tree, y_hat_rf, y_hat_xgb)
y_names <- c("Linear model", "KNN", "Decision tree", "Random forest", "XGBoost")
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
# ggsave("analyses/plots/RMSEs.png", width = 8, height = 4)


# final model -------------------------------------------------------------

final_y_hats <- predict(model_rf, data = comments_test)$predictions
final_RMSE <- RMSE(comments_test$score, final_y_hats)

# plot distribution of scores
enframe(final_y_hats) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = 'white', bins = 100) +
  labs(title = "Distribution of test predictions from final model",
       x = 'Upvotes',
       y = 'n')
# ggsave("analyses/plots/final_preds.png", width = 8, height = 5)

# plot preds against actuals
tibble(y = comments_test$score,
       y_hat = final_y_hats) %>% 
  ggplot(aes(x = y, y = y_hat)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = 'grey40', linetype = 'dashed') +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Predictions of final model vs. actuals on test set",
       subtitle = paste0("RMSE: ", round(final_RMSE, 1)),
       x = "Actual upvotes",
       y = "Predicted upvotes")
# ggsave("analyses/plots/preds_vs_actuals.png", width = 8, height = 5)
