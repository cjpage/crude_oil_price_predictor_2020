### This script evaluates a series of potentially viable algorithms for predicting cruide oil prices
### Each of these four algorithms considers crude oil in isolation from other commodities

### Algorithm 01: Predicted Crude Oil Price Based on Average Crude Oil Price

mu <- mean(crude_oil_train$closing_price)

mu

predicted_price_algorithm_01 <- mu

RMSE01 <- RMSE(predicted_price_algorithm_01, crude_oil_test$closing_price)

RMSE01

### Algorithm 02: Predicted Crude Oil Price Based on Average Crude Oil Price plus Day of the Week Effects

crude_oil_average_by_day_of_the_week <- crude_oil_train %>%
  group_by(date_weekday) %>%
  summarize(b_w = mean(closing_price))

predicted_price_algorithm_02 <- crude_oil_test %>%
  left_join(crude_oil_average_by_day_of_the_week, by= 'date_weekday') %>%
  mutate(pred = b_w) %>%
  pull(pred)

RMSE02 <- RMSE(predicted_price_algorithm_02, crude_oil_test$closing_price)

RMSE02

### Algorithm 03: Predicted Crude Oil Price Based on Average Crude Oil Price plus Month of the Year Effects

crude_oil_average_by_month_of_the_year <- crude_oil_train %>%
  group_by(date_month) %>%
  summarize(b_m = mean(closing_price))

predicted_price_algorithm_03 <- crude_oil_test %>%
  left_join(crude_oil_average_by_month_of_the_year, by= 'date_month') %>%
  mutate(pred = b_m) %>%
  pull(pred)

RMSE03 <- RMSE(predicted_price_algorithm_03, crude_oil_test$closing_price)

RMSE03

### Algorithm 04: Predicted Crude Oil Price Based on Average Crude Oil Price plus Quarter of the Year Effects

crude_oil_average_by_quarter_of_the_year <- crude_oil_train %>%
  group_by(date_quarter) %>%
  summarize(b_q = mean(closing_price - mu))

predicted_price_algorithm_04 <- mu + crude_oil_test %>%
  left_join(crude_oil_average_by_quarter_of_the_year, by= 'date_quarter') %>%
  pull(b_q)

RMSE04 <- RMSE(predicted_price_algorithm_04, crude_oil_test$closing_price)

RMSE04

### Algorithm 05: Predicted Crude Oil Price Based on Average Crude Oil Price plus Year Effects

crude_oil_average_by_year <- crude_oil_train %>%
  group_by(date_year) %>%
  summarize(b_y = mean(closing_price - mu))

predicted_price_algorithm_05 <- mu + crude_oil_test %>%
  left_join(crude_oil_average_by_year, by= 'date_year') %>%
  pull(b_y)

RMSE05 <- RMSE(predicted_price_algorithm_05, crude_oil_test$closing_price)

RMSE05


