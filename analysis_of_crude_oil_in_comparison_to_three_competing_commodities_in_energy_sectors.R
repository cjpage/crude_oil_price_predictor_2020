library(tidyverse)
library(ggplot2)
library(ggthemes)
load("rda/commodities.rdata")

### First, this script generates a date frame with crude oil and three competing commodities from the energy sector
### Then, it plots the 2010-2020 timeline of crude oil daily closing prices compared to those of the other energy sector commodities
### Finally, it saves the plot in a .png file entitled "crude_oil_price_history_in_comparison_to_three_other_energy_sector_commodities"

energy_sector <- data.frame(date = commodities$date, 
                            crude_oil = commodities$crude_oil_net_change_since_start_date, 
                            natural_gas = commodities$natural_gas_net_change_since_start_date,
                            heating_oil = commodities$heating_oil_net_change_since_start_date,
                            gasoline = commodities$gasoline_net_change_since_start_date)

energy_sector_plot <- energy_sector %>%
  ggplot() +
  geom_point(aes(date, natural_gas), color = "#E69F00", size = 0.75) +
  geom_point(aes(date, heating_oil), color = "#CC79A7", size = 0.75) +
  geom_point(aes(date, gasoline), color = "#0072B2", size = 0.75) +
  geom_point(aes(date, crude_oil), color = "#D55E00") +
  xlab("Date") +
  ylab("Net Change of Closing Price") +
  ggtitle("Crude Oil vs. Other Energy Commodities") +
  theme_economist() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.box = "horizontal" ,
        legend.text=element_text(size=8.5)) +
  guides(col = guide_legend(nrow = 1))

energy_sector_plot

ggsave("fig/crude_oil_in_comparison_to_three_other_energy_sector_commodities.png")

