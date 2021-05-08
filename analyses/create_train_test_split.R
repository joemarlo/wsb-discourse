library(tidyverse)
source('analyses/helpers.R')
source('analyses/plots/ggplot_settings.R')
set.seed(44)

# read in the processed data
comments <- read_csv("data/comments_selected.csv")


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


# write out ---------------------------------------------------------------

# write_csv(comments_train, "data/comments_train.csv")
# write_csv(comments_validate, "data/comments_validate.csv")
# write_csv(comments_test, "data/comments_test.csv")
