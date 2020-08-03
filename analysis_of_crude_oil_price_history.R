library(tidyverse)
library(ggplot2)
load("rda/commodities.rdata")

### First, this script plots the 2010-2020 timeline of crude oil daily closing prices
### Then, it saves the plot in a .png file entitled "crude_oil_price_history"

commodities %>%
  ggplot(aes(date, crude_oil_net_change_since_start_date)) +
  geom_point(color = "red") +
  xlab("Date") +
  ylab("Net Change of Closing Price") +
  ggtitle("Crude Oil Price History August 2010 to July 2020")

ggsave("crude_oil_price_history.png")

  