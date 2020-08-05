library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
load("rda/commodities.rdata")

### First, this script plots the 2010-2020 timeline of crude oil daily closing prices
### Then, it saves the plot in a .png file entitled "crude_oil_price_history"

crude_oil_plot <- commodities %>%
  ggplot(aes(date, crude_oil_net_change_since_start_date)) +
  geom_point(aes(color = "Crude Oil")) +
  labs(color = "Commodity") +
  xlab("Date") +
  ylab("Net Change of Closing Price") +
  ggtitle("Crude Oil Price History: 8/2010 - 7/2020") +
  theme_economist() +
    theme(legend.position="top",
          legend.title = element_blank(),
          legend.box = "horizontal" ,
          legend.text=element_text(size=8.5)) +
    guides(col = guide_legend(nrow = 1))

ggsave("fig/crude_oil_price_history.png")

  