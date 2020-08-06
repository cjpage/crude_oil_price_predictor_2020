library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(lubridate)
library(caret)

load("rda/commodities.rdata")

### First, this script generates a date frame with crude oil and three competing commodities from the precious metals sector
### Then, it plots the 2010-2020 timeline of crude oil daily closing prices compared to those of the three precious metals sector commodities
### Finally, it saves the plot in a .png file entitled "crude_oil_price_history_in_comparison_to_three_competing_precious_metal_sector_commodities"

crude_oil <- commodities %>%
  filter(commodity == 'Crude Oil')

precious_metals <- commodities %>%
  filter(sector == 'Precious Metals')

crude_oil_vs_precious_metals <- bind_rows(crude_oil, precious_metals)

crude_oil_vs_precious_metals_plot <- crude_oil_vs_precious_metals %>%
  ggplot(aes(date, closing_price)) +
  geom_point(aes(color = commodity)) +
  xlab("Date") +
  ylab("Closing Price") +
  ggtitle("Crude Oil versus Precious Metals") +
  theme_economist() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.box = "horizontal" ,
        legend.text=element_text(size=8.5)) +
  guides(col = guide_legend(nrow = 1))

crude_oil_vs_precious_metals_plot

ggsave("fig/crude_oil_in_comparison_to_three_precious_metals_sector_commodities.png")

