### First, this script plots the average crude oil closing prices by day of the week for each year in 2010-2020
### Then, it saves the plot in a .png file entitled "crude_oil_average_by_day_of_the_week"

crude_oil_average_by_day_of_the_week <- crude_oil %>%
  group_by(date_weekday) %>%
  mutate(weekday_avg = mean(closing_price))

crude_oil_average_by_day_of_the_week_plot <- crude_oil_average_by_day_of_the_week %>%
  ggplot(aes(date_weekday, weekday_avg)) +
  geom_point(aes(color = commodity), size = 5, show.legend = FALSE) +
  xlab("Day of the Week") +
  ylab("Closing Price") +
  ggtitle("Average Crude Oil Price by Day of the Week: 2010 - 2020") +
  theme_economist()

crude_oil_average_by_day_of_the_week_plot

ggsave("fig/crude_oil_average_by_day_of_the_week.png")

