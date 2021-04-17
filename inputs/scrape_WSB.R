library(tidyverse)
# pushshift documentation here: https://github.com/pushshift/api


# getting posts -----------------------------------------------------------
# 'submissions' == 'posts'

get_submissions_limited <- function(epoch_start, epoch_end = NULL){
  # return the submissions (ie posts) between epoch_start and epoch_end
  # epoch_* should be a unix timestamp (eg as.numeric(as.POSIXct(date)))
  # if epoch_end is not provided then the first 100 submissions after epoch_start 
    # will be returned
  
  end_epoch_arg_exists <- !is.null(epoch_end)
  
  # construct the url
  url <- paste0(
    'https://api.pushshift.io/reddit/search/submission/?subreddit=wallstreetbets',
    '&after=', epoch_start, 
    if_else(end_epoch_arg_exists, paste0('&before=', epoch_end), ''),
    '&limit=1000'
  )
  
  # pull the data and convert to dataframe
  df <- tibble()
  try({
    api_response <- jsonlite::fromJSON(url)
    cols_to_keep <- c('link_flair_text', 'created_utc', 'id', 'title', 'selftext', 'is_original_content', 
                      'is_video', 'media_only', 'num_comments', 'num_crossposts', 
                      'full_link', 'url', 'is_gallery', 'score', 'up_vote_ratio', 'total_awards_received')
    df <- api_response$data %>% 
      as_tibble() %>% 
      select(any_of(cols_to_keep))
    return(df)
  })
  return(df)
}

# example
# date_start <- as.Date('2021-01-01')
# date_end <- date_start + 1
# epoch_start <- as.numeric(as.POSIXct(date_start))
# epoch_end <- as.numeric(as.POSIXct(date_end))
# posts <- get_submissions_limited(epoch_start, epoch_end) # it limits it to 100 despite documentation allowing 1000
# posts$created_utc %>% lubridate::as_datetime() %>% range()

get_submissions <- function(epoch_start, epoch_end){
  # a wrapper around get_submissions_limited
  # re-reuns get_submissions_limited until all submissions between epoch_start and
    # epoch_end are found
  
  posts <- get_submissions_limited(epoch_start, epoch_end)
  end_time <- max(posts$created_utc)
  pb <- txtProgressBar(min = 1, max = 100, style = 3)
  
  while (end_time <= epoch_end) {
    Sys.sleep(0.4) # 200 requests per minute is the limit
    new_posts <- get_submissions_limited(epoch_start = end_time, epoch_end = NULL)
    posts <- bind_rows(posts, new_posts)
    end_time <- max(posts$created_utc)
    
    progress <- ceiling((end_time - epoch_start) / (epoch_end - epoch_start) * 100)
    setTxtProgressBar(pb, progress)
  }
  
  setTxtProgressBar(pb, 100)
  return(posts)
}
# example
# posts_full_day <- get_submissions(epoch_start, epoch_end)
# posts_full_day$created_utc %>% lubridate::as_datetime() %>% range()


# getting comments --------------------------------------------------------

get_comments <- function(submission_ids){
  # return the comments for a set of submission ids
  
  # tranche ids into groups of 250 b/c of url length limit of 2048
  group_size <- 250
  ids_n <- length(submission_ids)
  group_n <- ceiling(ids_n / group_size)
  id_groups <- sort(rep(1:group_n, group_size))
  submission_ids <- suppressWarnings(split(submission_ids, id_groups))
  
  # initiate progress bar
  pb <- txtProgressBar(min = 1, max = 100, style = 3)

  # for each group of ids, construct the url, make the request, and convert to df
  df <- map2_dfr(submission_ids, seq_along(submission_ids), function(submission_id_group, i){

    # 200 requests per minute is the limit
    Sys.sleep(0.4)
    
    # concat the ids into one string
    ids <- paste0(submission_id_group, collapse = ',')
    
    # construct the url
    url <- paste0('https://api.pushshift.io/reddit/comment/search/?link_id=', 
                  ids,
                  '&limit=20000')
    
    # pull the data and convert to dataframe
    df <- tibble()
    try({
      # make api call
      api_response <- jsonlite::fromJSON(url)
      
      # convert to dataframe
      cols_to_keep <- c('created_utc', 'body', 'link_id', 'parent_id', 
                        'score', 'total_awards_received', 'id', 'top_awarded_type')
      df <- api_response$data %>% 
        as_tibble() %>% 
        select(any_of(cols_to_keep))
      
      # update progress bar
      progress <- ceiling((i / length(submission_ids) * 100))
      setTxtProgressBar(pb, progress)
      
      return(df)
    })
    return(df)
  })
  setTxtProgressBar(pb, 100)
  return(df)
}

# example
# submission_ids <- posts_full_day %>% filter(num_comments > 0) %>% pull(id)
# comments <- get_comments(submission_ids) # it does appear to adhere to the 20k limit
# comments$created_utc %>% lubridate::as_datetime() %>% range()
# rm(posts, posts_full_day)


# final scraping ----------------------------------------------------------

# get posts
date_range <- c(as.Date("2020-12-01"), as.Date("2021-03-15"))
epoch_range <- as.numeric(as.POSIXct(date_range))
posts_raw <- get_submissions(epoch_range[1], epoch_range[2]) # takes ~6 hours

# get comments
submission_ids <- posts_raw$id[posts_raw$num_comments > 0]
comments_raw <- get_comments(submission_ids) # takes >4 hours

# write_csv(posts_raw, "inputs/posts_raw.csv")
# write_csv(comments_raw, "inputs/comments_raw.csv")
