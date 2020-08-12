### First, this script identitifies the maximum, minimum, mean, and standard deviation of crude oil closing prices for the 2010-2010 period of observation

crude_oil_price_at_beginning_of_period <- crude_oil$closing_price[1]
crude_oil_price_at_end_of_period <- crude_oil$closing_price[2519]
minimum_crude_oil_price_during_period <- min(crude_oil$closing_price)
maximum_crude_oil_price_during_period <- max(crude_oil$closing_price)
mean_crude_oil_price_during_period <- mean(crude_oil$closing_price)
standard_deviation_during_period <- sd(crude_oil$closing_price)

print("Crude Oil Commodity Prices began the August 2010 to July 2020 period at")
crude_oil_price_at_beginning_of_period

print("They ended that ten-year period at")
crude_oil_price_at_end_of_period

print("During that time, they averaged")
mean_crude_oil_price_during_period

print("Running from a minimum of")
minimum_crude_oil_price_during_period

print("To a maximum of")
maximum_crude_oil_price_during_period

print("With a standard deviation of")
standard_deviation_during_period

### Second, this script plots the 2010-2020 timeline of crude oil daily closing prices
### Then, it saves the plot in a .png file entitled "crude_oil_price_history"

crude_oil_plot <- crude_oil %>%
  ggplot(aes(date, closing_price)) +
  geom_point(aes(color = commodity)) +
  xlab("Date") +
  ylab("Closing Price") +
  ggtitle("Crude Oil Price History: 8/2010 - 7/2020") +
  theme_economist() +
    theme(legend.position="top",
          legend.title = element_blank(),
          legend.box = "horizontal" ,
          legend.text=element_text(size=8.5)) +
    guides(col = guide_legend(nrow = 1))

crude_oil_plot

ggsave("fig/crude_oil_price_history.png")

### Third, this script explores crude oil closing prices in terms of the cyclical components of time
#### It plots the average crude oil closing prices by day of the trading week for 2010-2020
#### Then, it saves the plot in a .png file entitled "crude_oil_average_by_day_of_the_week"

crude_oil_average_by_day_of_the_week_plot <- crude_oil_in_commodities_market %>%
  group_by(date_weekday) %>%
  ggplot(aes(as.factor(date_weekday), closing_price)) +
  geom_boxplot(aes(color = commodity), size = 0.5, show.legend = FALSE) +
  xlab("Day of the Trading Week") +
  ylab("Closing Price") +
  ggtitle("Average Crude Oil Closing Price by Day of the Trading Week: 2010 - 2020") +
  theme_economist()

crude_oil_average_by_day_of_the_week_plot

ggsave("fig/crude_oil_average_by_day_of_the_week.png")

#### It plots the average crude oil closing prices by month of the year for 2010-2020
#### Then, it saves the plot in a .png file entitled "crude_oil_average_by_month_of_the_year

crude_oil_average_by_month_of_the_year_plot <- crude_oil_in_commodities_market %>%
  group_by(date_month_of_the_year) %>%
  ggplot(aes(as.factor(date_month_of_the_year), closing_price)) +
  geom_boxplot(aes(color = commodity), size = 0.5, show.legend = FALSE) +
  xlab("Month of the Year") +
  ylab("Closing Price") +
  ggtitle("Average Crude Oil Closing Price by Month of the Year: 2010 - 2020") +
  theme_economist()

crude_oil_average_by_month_of_the_year_plot

ggsave("fig/crude_oil_average_by_month_of_the_year.png")

#### It plots the average crude oil closing prices by quarter of the year for 2010-2020
#### Then, it saves the plot in a .png file entitled "crude_oil_average_by_month_of_the_year

crude_oil_average_by_quarter_of_the_year_plot <- crude_oil_in_commodities_market %>%
  group_by(date_quarter_of_the_year) %>%
  ggplot(aes(as.factor(date_quarter_of_the_year), closing_price)) +
  geom_boxplot(aes(color = commodity), size = 0.5, show.legend = FALSE) +
  xlab("Quarter of the Year") +
  ylab("Closing Price") +
  ggtitle("Average Crude Oil Closing Price by Quarter of the Year: 2010 - 2020") +
  theme_economist()

crude_oil_average_by_quarter_of_the_year_plot

ggsave("fig/crude_oil_average_by_quarter_of_the_year.png")

### Fourth, this script explores crude oil closing prices in terms of the linear components of time
#### It plots the average crude oil closing prices by month for each month in 2010-2020
#### Then, it saves the plot in a .png file entitled "crude_oil_average_by_month"

crude_oil_average_by_month <- crude_oil_in_commodities_market %>%
  group_by(date_month) %>%
  mutate(month_avg = mean(closing_price))

crude_oil_average_by_month_plot <- crude_oil_average_by_month %>%
  ggplot(aes(date_month, month_avg)) +
  geom_point(aes(color = commodity), shape = 18, size = 3, show.legend = FALSE) +
  xlab("Month") +
  ylab("Closing Price") +
  ggtitle("Average Crude Oil Closing Price by Month: 2010 - 2020") +
  theme_economist()

crude_oil_average_by_month_plot

ggsave("fig/crude_oil_average_by_month.png")

#### It plots the average crude oil closing prices by year for each year in 2010-2020
#### Then, it saves the plot in a .png file entitled "crude_oil_average_by_year"

crude_oil_average_by_year <- crude_oil_in_commodities_market %>%
  group_by(date_year) %>%
  mutate(year_avg = mean(closing_price))

crude_oil_average_by_year_plot <- crude_oil_average_by_year %>%
  ggplot(aes(as.factor(date_year), year_avg)) +
  geom_point(aes(color = commodity), shape = 18, size = 5, show.legend = FALSE) +
  xlab("Year") +
  ylab("Closing Price") +
  ggtitle("Average Crude Oil Closing Price by Year: 2010 - 2020") +
  theme_economist()

crude_oil_average_by_year_plot

ggsave("fig/crude_oil_average_by_year.png")

### Fourth, this script explores crude oil closing prices in comparison to those of other complementary and competing commodities
#### It plots the 2010-2020 timeline of crude oil daily closing prices compared to those of the other energy sector commodities
#### Then, it saves the plot in a .png file entitled "crude_oil_price_history_in_comparison_to_three_other_energy_sector_commodities"

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

### It plots the 2010-2020 timeline of crude oil daily closing prices compared to those of the three precious metals sector commodities
### Then, it saves the plot in a .png file entitled "crude_oil_price_history_in_comparison_to_three_competing_precious_metal_sector_commodities"

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

### It plots the 2010-2020 timeline of crude oil daily closing prices compared to those of the three agricultural commodities
### Then, it saves the plot in a .png file entitled "crude_oil_price_history_in_comparison_to_three_competing_agriculture_sector_commodities"

crude_oil_vs_agriculture <- bind_rows(crude_oil, agriculture)

crude_oil_vs_agriculture_plot <- crude_oil_vs_agriculture %>%
  ggplot(aes(date, closing_price)) +
  geom_point(aes(color = commodity)) +
  xlab("Date") +
  ylab("Closing Price") +
  ggtitle("Crude Oil versus Agricultural Commodities") +
  theme_economist() +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.box = "horizontal" ,
        legend.text=element_text(size=8.5)) +
  guides(col = guide_legend(nrow = 1))

crude_oil_vs_agriculture_plot

ggsave("fig/crude_oil_in_comparison_to_three_agricultural_sector_commodities.png")


  