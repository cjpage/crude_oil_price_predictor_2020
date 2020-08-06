### This script plots the 2010-2020 timeline of crude oil daily closing prices compared to those of the other energy sector commodities
### Then, it saves the plot in a .png file entitled "crude_oil_price_history_in_comparison_to_three_other_energy_sector_commodities"

crude_oil_vs_other_energy_plot <- energy %>%
  ggplot(aes(date, closing_price)) +
  geom_point(aes(color = commodity)) +
  xlab("Date") +
  ylab("Closing Price") +
  ggtitle("Crude Oil versus Other Energy Sector Commodities") +
  theme_economist() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.box = "horizontal" ,
        legend.text=element_text(size=8.5)) +
  guides(col = guide_legend(nrow = 1))

crude_oil_vs_other_energy_plot


ggsave("fig/crude_oil_in_comparison_to_three_other_energy_sector_commodities.png")

