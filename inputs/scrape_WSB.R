library(tidyverse)
# pushshift documentation here: https://github.com/pushshift/api


# getting posts -----------------------------------------------------------
# submissions == posts

get_submissions_limited <- function(epoch_start, epoch_end = NULL){
  
  second_arg_exists <- !is.null(epoch_end)
  
  # construct the url
  url <- paste0(
    'https://api.pushshift.io/reddit/search/submission/?subreddit=wallstreetbets',
    '&after=', epoch_start, 
    if_else(second_arg_exists, paste0('&before=', epoch_end), ''),
    '&limit=1000'
  )
  
  # pull the data and convert to dataframe
  api_response <- jsonlite::fromJSON(url)
  df <- api_response$data %>% 
    as_tibble() %>% 
    select(all_of(c('created_utc', 'id', 'title', 'is_original_content', 'is_video', 'media_only', 'num_comments', 'num_crossposts', 'spoiler', 'over_18', 'full_link', 'url', 'is_gallery', 'score', 'total_awards_received')))
  return(df)
}

# example
date_start <- as.Date('2021-01-01')
date_end <- date_start + 1
epoch_start <- as.numeric(as.POSIXct(date_start))
epoch_end <- as.numeric(as.POSIXct(date_end))
posts <- get_submissions_limited(epoch_start, epoch_end) # it limits it to 100 despite documentation allowing 1000
posts$created_utc %>% lubridate::as_datetime() %>% range()

get_submissions <- function(epoch_start, epoch_end){
  
  posts <- get_submissions_limited(epoch_start, epoch_end)
  end_time <- max(posts$created_utc)
  
  buffer_in_seconds <- 60*30
  while (end_time <= (epoch_end - buffer_in_seconds)) {
    Sys.sleep(0.35) # 200 requests per minute is the limit
    new_posts <- get_submissions_limited(epoch_start = end_time, epoch_end = NULL)
    posts <- bind_rows(posts, new_posts)
    end_time <- max(posts$created_utc)
  }
  
  return(posts)
}
# example
posts_full_day <- get_submissions(epoch_start, epoch_end)
posts_full_day$created_utc %>% lubridate::as_datetime() %>% range()


# getting comments --------------------------------------------------------

get_comments <- function(submission_ids){
  ids <- paste0(submission_ids, collapse = ',')
  
  # theres a url length limit of 2048
  limit <- 2048 - 68
  if (nchar(ids) > limit){
    n_ids_to_keep <- round(limit / nchar(ids) * length(submission_ids)) - 1
    submission_ids <- submission_ids[1:n_ids_to_keep]
    ids <- paste0(submission_ids, collapse = ',')
    warning(paste0('Too many submission ids. Using the first ', n_ids_to_keep, '.'))
  } 
  
  # construct the url
  url <- paste0('https://api.pushshift.io/reddit/comment/search/?link_id=', 
                ids,
                '&limit=20000')
  
  # pull the data and convert to dataframe
  api_response <- jsonlite::fromJSON(url)
  df <- api_response$data %>% 
    as_tibble() %>% 
    select(all_of(c('created_utc', 'body', 'link_id', 'parent_id', 'score', 'total_awards_received')))
  
  return(df)
}

# example
submission_ids <- posts_full_day %>% filter(num_comments > 0) %>% pull(id)
comments <- get_comments(submission_ids) # it does appear to adhere to the 20k limit
comments$created_utc %>% lubridate::as_datetime() %>% range()
