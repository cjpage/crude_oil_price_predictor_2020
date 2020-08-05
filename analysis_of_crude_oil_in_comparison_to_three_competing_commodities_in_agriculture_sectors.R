library(tidyverse)
library(ggplot2)
library(ggthemes)
load("rda/commodities.rdata")

### First, this script generates a date frame with crude oil and three competing commodities from the agriculture sector
### Then, it plots the 2010-2020 timeline of crude oil daily closing prices compared to those of the three agricultural commodities
### Finally, it saves the plot in a .png file entitled "crude_oil_price_history_in_comparison_to_three_competing_agriculture_sector_commodities"

agriculture_sector <- data.frame(date = commodities$date, 
                                 crude_oil = commodities$crude_oil_net_change_since_start_date,
                                 wheat = commodities$wheat_net_change_since_start_date,
                                 rice = commodities$rice_net_change_since_start_date,
                                 soybeans = commodities$soybean_net_change_since_start_date)

agriculture_sector_plot <- agriculture_sector %>%
  ggplot() +
  geom_point(aes(date, wheat), color = "#E69F00", size = 0.1) +
  geom_point(aes(date, rice), color = "#CC79A7", size = 0.1) +
  geom_point(aes(date, soybeans), color = "#0072B2", size = 0.1) +
  geom_point(aes(date, crude_oil), color = "#D55E00", size = 1) +
  xlab("Date") +
  ylab("Net Change of Closing Price") +
  ggtitle("Crude Oil vs Agriculture Commodities") +
  theme_economist() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.box = "horizontal" ,
        legend.text=element_text(size=8.5)) +
  guides(col = guide_legend(nrow = 1))

agriculture_sector_plot

ggsave("fig/crude_oil_in_comparison_to_three_agriculture_sector_commodities.png")

