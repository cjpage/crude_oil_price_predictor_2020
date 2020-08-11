### This script evaluates a series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these eight algorithms considers crude oil in isolation from other globally traded commodities
### In isolation, the available predictors include both the cyclical and the linear components of time
### One cyclical component is each observation's day of the trading week (Monday, Tuesday,...Friday)
### Others are the month (1,2..12) and the quarter (1,2,..4) of each year on the 2010-2020 timeline
### Liner components are the year (e.g., 2010) and month (e.g., 8/2010) of each observation

### ALGORITHM 01 (AVERAGE)

##### This algorithm predicts crude oil closing prices based solely on the average crude oil price for 2010-2020
##### It will serve as a baseline from which to compare the analytical viability of other, more sophisticated algorithms

mu <- mean(crude_oil_train$closing_price)

predicted_price_algorithm_01 <- mu

RMSE01 <- RMSE(predicted_price_algorithm_01, crude_oil_test$closing_price)

RMSE01

### ALGORITHM 02 (WEEKDAY EFFECTS)

##### This algorithm predicts crude oil closing prices based on average crude oil price plus the effects of the day of the trading week

crude_oil_average_by_day_of_the_week <- crude_oil_train %>%
  group_by(date_weekday) %>%
  summarize(b_w = mean(closing_price))

predicted_price_algorithm_02 <- crude_oil_test %>%
  left_join(crude_oil_average_by_day_of_the_week, by= 'date_weekday') %>%
  mutate(pred = b_w) %>%
  pull(pred)

RMSE02 <- RMSE(predicted_price_algorithm_02, crude_oil_test$closing_price)

RMSE02

### ALGORITHM 03 (MONTH OF THE YEAR EFFECTS)
##### This algorithm predicts crude oil closing prices based on average crude oil price plus the effects of the month of the year

crude_oil_average_by_month <- crude_oil_train %>%
  group_by(date_month_of_the_year) %>%
  summarize(b_m_y = mean(closing_price))

predicted_price_algorithm_03 <- crude_oil_test %>%
  left_join(crude_oil_average_by_month, by= 'date_month_of_the_year') %>%
  mutate(pred = b_m_y) %>%
  pull(pred)

RMSE03 <- RMSE(predicted_price_algorithm_03, crude_oil_test$closing_price)

RMSE03

### ALGORITHM 04 (QUARTER OF THE YEAR EFFECTS)
##### This algorithm predicts crude oil closing prices based on average crude oil price plus the effects of the quarter of the year

crude_oil_average_by_quarter_of_the_year <- crude_oil_train %>%
  group_by(date_quarter_of_the_year) %>%
  summarize(b_q_y = mean(closing_price))

predicted_price_algorithm_03 <- crude_oil_test %>%
  left_join(crude_oil_average_by_quarter_of_the_year, by= 'date_quarter_of_the_year') %>%
  mutate(pred = b_q_y) %>%
  pull(pred)

RMSE04 <- RMSE(predicted_price_algorithm_04, crude_oil_test$closing_price)

RMSE04

### ALGORITHM 05 (YEAR EFFECTS)
##### This algorithm predicts crude oil closing prices based on average crude oil price plus the effects of the year

crude_oil_average_by_year <- crude_oil_train %>%
  group_by(date_year) %>%
  summarize(b_y = mean(closing_price))

predicted_price_algorithm_05 <- crude_oil_test %>%
  left_join(crude_oil_average_by_year, by= 'date_year') %>%
  pull(b_y)

RMSE05 <- RMSE(predicted_price_algorithm_05, crude_oil_test$closing_price)

RMSE05

### ALGORITHM 06 (MONTH EFFECTS)
##### This algorithm predicts crude oil closing prices based on average crude oil price plus the effects of the month

crude_oil_average_by_month <- crude_oil_train %>%
  group_by(date_month) %>%
  summarize(b_m = mean(closing_price))

predicted_price_algorithm_06 <- crude_oil_test %>%
  left_join(crude_oil_average_by_month, by= 'date_month') %>%
  mutate(pred = b_m) %>%
  pull(pred)

RMSE06 <- RMSE(predicted_price_algorithm_06, crude_oil_test$closing_price)

RMSE06

### ALGORITHM 07 (RANDOM FOREST - TIME)
##### This algorithm predicts crude oil closing prices based on a Random Forest of time components
####### The first step is to look at a Random Forest incorporating all of above-listed components of time

nodesize <- seq(1, 5, 1)

rmses <- sapply(nodesize, function(n){
  rf_time = randomForest(closing_price ~ 
                           date_weekday +
                           date_month_of_the_year +
                           date_quarter_of_the_year +
                           date_year +
                           date_month, 
                         data = crude_oil_train, nodesize = n)
  pred <- predict(rf_time, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf_time = randomForest(closing_price ~ 
                         date_weekday +
                         date_month_of_the_year +
                         date_quarter_of_the_year +
                         date_year +
                         date_month, 
                       data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

####### The second is to build a plot to help determine which of those components is/are the most important predictors

varImpPlot(rf_time)

####### Based on that plot, Random Forest will be revised to focus on the month and year

nodesize <- seq(1, 2, 1)

rmses <- sapply(nodesize, function(n){
  rf_time = randomForest(closing_price ~ 
                           date_month +
                           date_year, 
                         data = crude_oil_train, nodesize = n)
  pred <- predict(rf, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf_time = randomForest(closing_price ~ 
                         date_month +
                         date_year, 
                       data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

pred <- predict(rf_time, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_07 <- predict(rf_time, newdata = crude_oil_test)

RMSE07 <- RMSE(predicted_price_algorithm_07, crude_oil_test$closing_price)

RMSE07

### ALGORITHM 08 (KNN - TIME)
##### This algorithm predicts crude oil closing prices based on a K-Nearest Neighbors (KNN) model of the two key time components

knn_time <- train(closing_price ~ 
                    date_year + 
                    date_month, 
                   method = "knn",
                   data = crude_oil_train)

ggplot(knn_time, highlight = TRUE)

pred <- predict(knn_time, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_08 <- predict(knn_time, newdata = crude_oil_test)

RMSE08 <- RMSE(predicted_price_algorithm_08, crude_oil_test$closing_price)

RMSE08

### ALGORITHM 09 (RANGER - TIME)
##### This algorithm predicts crude oil closing prices based on a Ranger model of the two key time components

ranger_time <- train(closing_price ~ 
                       date_year + 
                       date_month, 
                     method = "ranger",
                     data = crude_oil_train)

pred <- predict(ranger_time, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_08 <- predict(ranger_time, newdata = crude_oil_test)

RMSE09 <- RMSE(predicted_price_algorithm_09, crude_oil_test$closing_price)

RMSE09

### SUMMARY TO THIS POINT

ALGORITHM_tab <- c("ALGORITHM 01 - Average Closing Price",
                   "ALGORITHM 02 - Average Closing Price plus Day of the Week Effects",
                   "ALGORITHM 03 - Average Closing Price plus Month of the Year Effects",
                   "ALGORITHM 04 - Average Closing Price plus Quarter of the Year Effects",
                   "ALGORITHM 05 - Average Closing Price plus Year Effects",
                   "ALGORITHM 06 - Average Closing Price Plus Month Effects",
                   "ALGORITHM 07 - Random Forest of Time Components",
                   "ALGORITHM 08 - KNN Model of Time Components",
                   "ALGORITHM 09 - Ranger Model of Time Components")

RMSE_tab <- c(RMSE01,
              RMSE02,
              RMSE03,
              RMSE04,
              RMSE05,
              RMSE06,
              RMSE07,
              RMSE08,
              RMSE09)

Results <- data.frame(ALGORITHM_tab, RMSE_tab)

Results %>%
  arrange(RMSE_tab) %>%
  print(right = FALSE)


