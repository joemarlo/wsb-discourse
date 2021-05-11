library(tidyverse)
source('analyses/helpers.R')
source("analyses/plots/ggplot_settings.R")

# read in all the data
comments <- read_csv("data/comments_cleaned.csv")

# pull just the ids
comments <- comments[, c('id_post', 'id_parent', 'id_comment', 'score')]
comments <- na.omit(comments)
comments$is_parent_top_level <- stringr::str_sub(comments$id_parent, 0, 2) == 't3'
comments$id_parent <- stringr::str_sub(comments$id_parent, 4)

# define recursive function to get all the parents of a given comment
get_parents <- function(id_comment, parents = NULL){
  id_parent <- comments$id_parent[comments$id_comment == id_comment]
  parents <- c(parents, id_parent)
  if (length(id_parent) == 0) return(NULL)
  is_top_level <- comments$is_parent_top_level[comments$id_comment == id_comment]
  if (isTRUE(is_top_level)) return(parents)
  get_parents(id_parent, parents)
}
get_parents('gnd7lh6')

# apply function to all ids
# takes 3+ hours
comment_parents <- parallel::mclapply(X = comments$id_comment, 
                                      FUN = get_parents,
                                      mc.cores = parallel::detectCores())

# calculate comment depth
comments$comment_level <- sapply(comment_parents, length)
comments$comment_level[comments$comment_level == 0] <- NA

# ensure NAs do not correlate with upvotes
split(comments$score, is.na(comments$comment_level)) %>% 
  lapply(., summary)

  
# write out ---------------------------------------------------------------
# comments %>%
#   select(id_comment, comment_level) %>%
#   write_csv("data/comment_level.csv")
