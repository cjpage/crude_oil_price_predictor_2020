library(tidyverse)
library(ggplot2)
library(ggthemes)
load("rda/commodities.rdata")

### First, this script generates a date frame with crude oil and three competing commodities from the precious metals sector
### Then, it plots the 2010-2020 timeline of crude oil daily closing prices compared to those of the three precious metals sector commodities
### Finally, it saves the plot in a .png file entitled "crude_oil_price_history_in_comparison_to_three_competing_precious_metal_sector_commodities"

precious_metals_sector <- data.frame(date = commodities$date, 
                                     crude_oil = commodities$crude_oil_net_change_since_start_date, 
                                     gold = commodities$gold_net_change_since_start_date,
                                     silver = commodities$silver_net_change_since_start_date,
                                     platinum = commodities$platinum_net_change_since_start_date)

precious_metals_sector_plot <- precious_metals_sector %>%
  ggplot() +
  geom_point(aes(date, gold), color = "#E69F00", size = 0.75) +
  geom_point(aes(date, silver), color = "#CC79A7", size = 0.75) +
  geom_point(aes(date, platinum), color = "#0072B2", size = 0.75) +
  geom_point(aes(date, crude_oil), color = "#D55E00", size = 1) +
  xlab("Date") +
  ylab("Net Change of Closing Price") +
  ggtitle("Crude Oil vs Precious Metals Commodities") +
  theme_economist() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.box = "horizontal" ,
        legend.text=element_text(size=8.5)) +
  guides(col = guide_legend(nrow = 1))

precious_metals_sector_plot

ggsave("fig/crude_oil_in_comparison_to_three_precious_metals_sector_commodities.png")

