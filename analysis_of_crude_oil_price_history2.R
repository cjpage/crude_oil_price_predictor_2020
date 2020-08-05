library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
load("rda/commodities2.rdata")

### First, this script plots the 2010-2020 timeline of crude oil daily closing prices
### Then, it saves the plot in a .png file entitled "crude_oil_price_history"

crude_oil <- commodities2 %>%
  filter(Commodity == 'Crude Oil') %>%
  mutate(net_price_change = `Closing Price` - `Closing Price` [1])

crude_oil_plot2 <- crude_oil %>%
  ggplot(aes(date, net_price_change)) +
  geom_point(aes(color = Commodity)) +
  xlab("Date") +
  ylab("Closing Price") +
  ggtitle("Crude Oil Price History: 8/2010 - 7/2020") +
  theme_economist() +
    theme(legend.position="top",
          legend.title = element_blank(),
          legend.box = "horizontal" ,
          legend.text=element_text(size=8.5)) +
    guides(col = guide_legend(nrow = 1))

crude_oil_plot2

ggsave("fig/crude_oil_price_history2.png")

  