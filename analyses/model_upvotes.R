library(tidyverse)
library(tidymodels)
source('analyses/helpers.R')
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in the data
comments_train <- read_csv("data/comments_train.csv")
comments_validate <- read_csv("data/comments_validate.csv")
comments_test <- read_csv("data/comments_test.csv")


# linear regression -------------------------------------------------------

model_lm <- lm(score ~ ., data = comments_train)
broom::tidy(model_lm) %>% View


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
  k = 18
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
  mtry = tune_rf$bestTune$mtry
)

# variable importance
model_rf$variable.importance %>% 
  enframe() %>% 
  slice_max(order_by = value, n = 10) %>% 
  ggplot(aes(x = value, y = reorder(name, value))) +
  geom_col() +
  scale_x_continuous(labels = NULL) +
  labs(title = "Top 10 variables by importance in random forest model",
       x = "Importance",
       y = NULL)
# ggsave("analyses/plots/importance_rf.png", width = 8, height = 4)


# boosting ----------------------------------------------------------------

# grid search 
grid_xgb <- expand.grid(nrounds = 100,
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
# model_xgb$evaluation_log$train_rmse %>% plot()

# variable importance
xgboost::xgb.importance(model = model_xgb) %>% 
  tibble() %>% 
  slice_max(order_by = Gain, n = 10) %>% 
  ggplot(aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_col() +
  scale_x_continuous(labels = NULL) +
  labs(title = "Top 10 variables by importance in XGBoost model",
       x = "Importance",
       y = NULL)
# ggsave("analyses/plots/importance_xgb.png", width = 8, height = 4)


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

# precision at k
precision_at_k <- function(y, y_hat){
  # calculates the precision-at-k
  
  # rank the predictions
  ranked_preds <- tibble(y = y,
                         y_hat = y_hat) %>% 
    mutate(rank_y = rank(-y),
           rank_y_hat = rank(-y_hat)) %>% 
    arrange(-y_hat)
  
  # calculate precision
  precision <- sapply(seq_along(ranked_preds$rank_y_hat), function(i){
    sliced_tbl <- ranked_preds[1:i,]
    prop_y <- mean(sliced_tbl$y <= i)
    return(prop_y)
  }) %>% 
    enframe() %>% 
    rename(k = name, precision = value)
  
  return(precision)
}

precisions_df <- map2_dfr(y_hats, y_names, function(y_hat, y_name){
  precision <- precision_at_k(comments_validate$score, y_hat)
  precision$model <- y_name
  return(precision)
})

precisions_df %>% 
  ggplot(aes(x = k, y = precision, color = model)) +
  geom_line() +
  scale_x_continuous(labels = scales::comma_format(),
                     limits = c(0, 1000)) +
  labs(title = "Precision-at-k for each model on the validation set",
       subtitle = 'Only 1,000 highest ranked predictions shown',
       caption = "Decision tree predictions are invariant",
       x = 'Predictions ranked by value (k)',
       y = 'Precision',
       color = NULL) +
  theme(legend.position = 'bottom')
# ggsave("analyses/plots/precision.png", width = 8, height = 6)


# final model -------------------------------------------------------------

# final RMSE
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
final_preds <- tibble(y = comments_test$score,
                      y_hat = final_y_hats)
final_preds %>% 
  ggplot(aes(x = y, y = y_hat)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = 'grey40', linetype = 'dashed') +
  scale_x_log10(labels = scales::comma_format(1)) +
  scale_y_log10(labels = scales::comma_format(1)) +
  labs(title = "Final model: test set predictions vs. actuals",
       subtitle = paste0("RMSE: ", round(final_RMSE, 1)),
       x = "Actual upvotes",
       y = "Predicted upvotes")
# ggsave("analyses/plots/preds_vs_actuals.png", width = 8, height = 5)

# calibration plot
bin_breaks <- seq(min(c(0, final_y_hats)), 
                  max(final_y_hats),
                  by = 50)
final_preds %>% 
  mutate(y_hat_binned = cut(y_hat, breaks = bin_breaks)) %>%
  left_join(tibble(bin_label = bin_breaks,
                   y_hat_binned = cut(bin_label, breaks = bin_breaks)),
            by = 'y_hat_binned') %>% 
  ggplot(aes(x = bin_label, y = y, group = bin_label)) +
  geom_boxplot() +
  scale_x_continuous(breaks = bin_breaks, labels = scales::comma_format()) +
  scale_y_log10(labels = scales::comma_format(1)) +
  labs(title = "Calibration: Actual upvotes vs. predicted",
       x = "Midpoint of binned predictions",
       y = "Actual upvotes") +
  theme(axis.text.x = element_text(angle = -55))
# ggsave("analyses/plots/calibration.png", width = 8, height = 5)

# precision at k
precision_at_k(comments_test$score, final_y_hats) %>% 
  # filter(rank > 100) %>%
  ggplot(aes(x = k, y = precision)) +
  geom_line() +
  scale_x_continuous(labels = scales::comma_format()) +
  labs(title = "Precision-at-k of final model",
       x = 'Predictions ranked by value (k)',
       y = 'Precision',
       color = NULL) +
  theme(legend.position = 'bottom')
# ggsave("analyses/plots/precision_xgb.png", width = 8, height = 6)


# key variables -----------------------------------------------------------

# variable importance
xgboost::xgb.importance(model = model_xgb) %>% 
  tibble() %>% 
  filter(str_detect(Feature, "^(sentiment)|(keyphrase)|(top_bigram)")) %>% 
  ggplot(aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_col() +
  scale_x_continuous(labels = NULL) +
  labs(title = "Comment content by feature importance",
       subtitle = 'XGBoost model',
       x = "Importance",
       y = NULL)
# ggsave("analyses/plots/importance_xgb_content.png", width = 8, height = 4)

# lasso features
lasso_features <- read_csv("data/lasso_features.csv")
lasso_features %>% 
  filter(str_detect(term, "^(sentiment)|(keyphrase)|(top_bigram)")) %>% 
  ggplot(aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey50') +
  geom_col() +
  labs(title = "Comment content by LASSO estimate",
       subtitle = 'LASSO feature selection model',
       x = "Estimate",
       y = NULL)
# ggsave("analyses/plots/importance_lasso_content.png", width = 8, height = 6)
