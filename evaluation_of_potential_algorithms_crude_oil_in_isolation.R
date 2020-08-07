### This script evaluates a series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these six algorithms considers crude oil in isolation from other globally traded commodities
### In isolation, the available predictors are components (day, month, quarter, year) of time

### Algorithm 01: Predicted Crude Oil Price Based on Average Crude Oil Price for 2010-2020

mu <- mean(crude_oil_train$closing_price)

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

### Algorithm 06: Predicted Crude Oil Price Based on Random Forest of Time Effects

#### The first step is to look at a random forest incorporating year, quarter, month, and day components of time

rf = randomForest(closing_price ~ date_year + date_quarter + date_month + date_weekday, data = crude_oil_train)

##### The second is to build a plot to help determine which of those components is/are the most important predictors

varImp(rf)

###### Based on that plot, random forest will be revised to focus on the year and month components of time

rf = randomForest(closing_price ~ date_year + date_month, data = crude_oil_train)

####### That revised random forest is the basis of the sixth algorithm in this series

pred <- predict(rf, newdata = crude_oil_train)
RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_06 <- predict(rf, newdata = crude_oil_test)
RMSE06 <- RMSE(predicted_price_algorithm_06, crude_oil_test$closing_price)

RMSE06


