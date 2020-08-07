### First, this script plots the average crude oil closing prices by quarter of the year for each year in 2010-2020
### Then, it saves the plot in a .png file entitled "crude_oil_average_by_quarter_of_the_year"

crude_oil_average_by_quarter_of_the_year <- crude_oil %>%
  group_by(date_quarter) %>%
  mutate(quarter_avg = mean(closing_price))

crude_oil_average_by_quarter_of_the_year_plot <- crude_oil_average_by_quarter_of_the_year %>%
  ggplot(aes(date_quarter, quarter_avg)) +
  geom_point(aes(color = commodity), size = 5, show.legend = FALSE) +
  xlab("Quarter of the Year") +
  ylab("Closing Price") +
  ggtitle("Average Crude Oil Price by Quarter of the Year: 2010 - 2020") +
  theme_economist()

crude_oil_average_by_quarter_of_the_year_plot

ggsave("fig/crude_oil_average_by_quarter_of_the_year.png")

