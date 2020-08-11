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

  