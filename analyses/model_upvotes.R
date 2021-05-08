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
  k = 30
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
# ggsave("analyses/plots/rf_importance.png", width = 8, height = 4)


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
tibble(y = comments_test$score,
       y_hat = final_y_hats) %>% 
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
tibble(y = comments_test$score,
       y_hat = final_y_hats) %>% 
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
