library(tidyverse)
source('analyses/plots/ggplot_settings.R')

comments_cleaned <- read_csv('data/comments_cleaned.csv')
comments_processed <- comments_cleaned

# preprocessing -----------------------------------------------------------

# add flag for holidays


## preprocessing notes
# keywords to think about -> see google doc
# add flag if post includes emojis
# add flag if post contains a link
# add flag for key phrases
# sentiment
# topic modeling

# write out
# write_csv(comments_processed, 'data/comments_processed.csv')
