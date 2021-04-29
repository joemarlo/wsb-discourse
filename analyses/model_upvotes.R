library(tidyverse)
library(tidymodels)
set.seed(44)

comments <- read_csv("data/comments_prepped.csv")



# create train-test split --------------------------------------------------

# split the data into train, validate, test
split_ratios <- c(0.7, 0.15, 0.15)
indices_train <- sample(c(TRUE, FALSE), size = nrow(comments), replace = TRUE, 
                        prob = c(split_ratios[1], 1-split_ratios[1]))
comments_train <- comments[indices_train,]
indices_validate <- sample(c(TRUE, FALSE), size = sum(!indices_train), replace = TRUE,
                           prob = c(split_ratios[2], split_ratios[3]))
comments_validate <- comments[!indices_train,][indices_validate,]
comments_test <- comments[!indices_train,][!indices_validate,]



# linear regression -------------------------------------------------------





# decision tree -----------------------------------------------------------




# random forest -----------------------------------------------------------




# xgboost -----------------------------------------------------------------




# RNN ---------------------------------------------------------------------



# split the data
comments_split <- initial_split(comments, prop = 0.8)
comments_other <- training(comments_split)
comments_validate <- validation_split(comments_other, prop = 0.8)
comments_test <- testing(comments_split)


# fit model ---------------------------------------------------------------

# specify the model types
model_linear <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')
model_linear_bayesian <- linear_reg() %>% 
  set_engine('stan') %>% 
  set_mode('regression') 
model_rf <- rand_forest() %>% 
  set_mode('regression') %>% 
  set_engine('ranger')
model_xgb <- boost_tree() %>% 
  set_engine('xgboost') %>% 
  set_mode('regression')
# model_lasso <- logistic_reg(penalty = tune(), mixture = 1) %>% 
#   set_engine("glmnet")

# specify the model formula
covariates <- c(TBD)
model_formula <- reformulate(
  termlabels = covariates,
  response = upvotes,
  intercept = TRUE
)

# fit the model
model_linear_fit <- model_linear %>% 
  fit(model_formula,
      data = posts_train)

# view parameters
tidy(model_linear_fit)

# make predictions
posts_test$y_hat <- predict(model_linear_fit, new_data = posts_test)


# evaluate ----------------------------------------------------------------

model_linear_last_fit <- model_linear %>% 
  last_fit(model_formula, split = posts_split)
model_linear_last_fit %>% collect_predictions()
model_linear_last_fit %>% collect_metrics()


### for classifiers
metrics_upvotes <- metric_set(accuracy, sensitivity, specificity)
metrics_upvotes(posts_test, truth = TBD, estimate = y_hat)
model_linear_results <- model_linear_last_fit %>% collect_predictions()
model_linear_results %>% roc_curve(truth = TBD, .pred_yes) %>% autoplot()
roc_auc(model_linear_results,
        truth = TBD, 
        .pred_yes)
conf_mat(model_linear_results,
         truth = TBD,
         estimate = .pred_class) %>% 
  # autoplot(type = 'heatmap')
  autoplot(type = 'mosaic')


## flow
# train the model
logistic_fit <- logistic_model %>% 
  last_fit(model_formula, split = posts_split)

# collect metrics
logistic_fit %>% 
  collect_metrics()

# plot ROC curve
logistic_fit %>% 
  collect_predictions() %>% 
  roc_curve(truth = TBD, .pred_yes) %>% 
  autoplot()
