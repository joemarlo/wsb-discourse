library(tidyverse)
library(tidyquant)
source("analyses/plots/ggplot_settings.R")
source("analyses/helpers.R")

# get price data
GME_price <- tq_get("GME") %>% filter(date > as.Date('2020-11-26'))

# write out for feature engineering
# write_csv(GME_price, "data/GME_price.csv")

# df of key dates
key_dates <- tribble(
  ~date, ~y, ~label,
  "2021-01-05", 470, 'All time high',
  "2021-02-02", 320, 'Second\npeak',
  '2021-02-20', 440, "Congressional\nhearing\n(2/18)",
  '2021-03-10', 370, "Third peak"
) %>% 
  mutate(date = as.Date(date))

# plot candlestick
GME_price %>% 
  filter(date <= as.Date('2021-04-15')) %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close),
                   colour_up = 'grey30', colour_down = 'grey30',
                   fill_up = 'grey70', fill_down = 'grey70') +
  geom_vline(xintercept = as.Date('2021-02-18'), 
             color = 'grey50', linetype = 'dashed') +
  geom_text(data = key_dates, aes(x = date, y = y, label = label), 
            color = 'grey30', hjust = 0) +
  scale_x_date(breaks = semi_monthly, date_labels = "%b-%d") +
  labs(title = 'GameStop stock price spikes in late January',
       subtitle = 'Candlestick chart shows the high, low, open, and close prices',
       x = NULL,
       y = "Stock price (USD)")
# ggsave("analyses/plots/GME_price.png", height = 4.5, width = 8)
