library(tidyverse)
library(tidyquant)
source("analyses/plots/ggplot_settings.R")
source("analyses/helpers.R")

# get price data
GME_price <- tq_get("GME")

# df of key dates
key_dates <- tribble(
  ~date, ~y, ~label,
  "2021-01-05", 470, 'All time high',
  "2021-02-02", 320, 'Second peak',
  '2021-02-10', 200, "Congressional\nhearing\n(2/18)"
) %>% 
  mutate(date = as.Date(date))

# plot candlestick
GME_price %>% 
  filter(date > as.Date('2020-12-01')) %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close),
                   colour_up = 'grey30', colour_down = 'grey30',
                   fill_up = 'grey70', fill_down = 'grey70') +
  geom_text(data = key_dates, aes(x = date, y = y, label = label), hjust = 0) +
  scale_x_date(breaks = semi_monthly, date_labels = "%m/%d") +
  labs(title = 'GameStop stock price spikes in late January',
       subtitle = 'Candlestick chart shows the high, low, open, and close prices',
       x = NULL,
       y = "Stock price (USD)")
# ggsave("analyses/plots/GME_price.png", height = 4.5, width = 8)
