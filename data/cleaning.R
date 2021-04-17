library(tidyverse)
library(tidytext)
source('analyses/plots/ggplot_settings.R')

posts <- read_csv('inputs/posts_raw.csv')
comments <- read_csv('inputs/comments_raw.csv')


# remove megathreads?
# remove deleted and removed posts/comments?
# remove bots (there are automated bots and usually self identify with "I am a bot")
# remove top_awarded_type since its all NAs
# only concentrate on certain flairs?
  # flairs_to_keep <- c("Discussion", "Daily Discussion", "DD", "Technical Analysis", "YOLO", "News", "Gain")
# add flag for holidays
# add date, weekend, hour
# add flair to the comments dataframe from the posts dataframe

## preprocessing notes
# keywords to think about -> see google doc
# add flag if post contains a specific stock?
# add flag if post includes an image
  # one way to do this is to check if the url column ends in .png, .jpg, etc.
# add flag if post includes emojis
# add flag if post contains a link
# add flag for key phrases


# add datetime, date, hour, and weekday
posts$datetime <- lubridate::as_datetime(posts$created_utc)
posts$date <- as.Date(posts$datetime)
posts$hour <- lubridate::hour(posts$datetime)
posts$wday <- lubridate::wday(posts$datetime, label = TRUE)

comments$datetime <- lubridate::as_datetime(comments$created_utc)
comments$date <- as.Date(comments$datetime)
comments$hour <- lubridate::hour(comments$datetime)
comments$wday <- lubridate::wday(comments$datetime, label = TRUE)





# write out ---------------------------------------------------------------

# posts <- write_csv(posts, 'data/posts.csv')
# comments <- write_csv(comments, 'data/comments.csv')
