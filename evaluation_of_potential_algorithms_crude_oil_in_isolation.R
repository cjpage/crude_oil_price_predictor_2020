### This script evaluates a series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these six algorithms considers crude oil in isolation from other globally traded commodities
### In isolation, the available predictors are components (day, month, quarter, year) of time

### ALGORITHM 01 (AVERAGE)
##### This algorithm predicts crude oil closing prices based on average crude oil price for 2010-2020

mu <- mean(crude_oil_train$closing_price)

predicted_price_algorithm_01 <- mu

RMSE01 <- RMSE(predicted_price_algorithm_01, crude_oil_test$closing_price)

RMSE01

### ALGORITHM 02 (WEEKDAY EFFECTS)
##### This algorithm predicts crude oil closing prices based on average crude oil price plus weekday effects

crude_oil_average_by_day_of_the_week <- crude_oil_train %>%
  group_by(date_weekday) %>%
  summarize(b_w = mean(closing_price))

predicted_price_algorithm_02 <- crude_oil_test %>%
  left_join(crude_oil_average_by_day_of_the_week, by= 'date_weekday') %>%
  mutate(pred = b_w) %>%
  pull(pred)

RMSE02 <- RMSE(predicted_price_algorithm_02, crude_oil_test$closing_price)

RMSE02

### ALGORITHM 03 (MONTH EFFECTS)
##### This algorithm predicts crude oil closing prices based on average crude oil price plus month effects

crude_oil_average_by_month_of_the_year <- crude_oil_train %>%
  group_by(date_month) %>%
  summarize(b_m = mean(closing_price))

predicted_price_algorithm_03 <- crude_oil_test %>%
  left_join(crude_oil_average_by_month_of_the_year, by= 'date_month') %>%
  mutate(pred = b_m) %>%
  pull(pred)

RMSE03 <- RMSE(predicted_price_algorithm_03, crude_oil_test$closing_price)

RMSE03

### ALGORITHM 04 (QUARTER EFFECTS)
##### This algorithm predicts crude oil closing prices based on average crude oil price plus quarter effects

crude_oil_average_by_quarter_of_the_year <- crude_oil_train %>%
  group_by(date_quarter) %>%
  summarize(b_q = mean(closing_price - mu))

predicted_price_algorithm_04 <- mu + crude_oil_test %>%
  left_join(crude_oil_average_by_quarter_of_the_year, by= 'date_quarter') %>%
  pull(b_q)

RMSE04 <- RMSE(predicted_price_algorithm_04, crude_oil_test$closing_price)

RMSE04

### ALGORITHM 05 (YEAR EFFECTS)
##### This algorithm predicts crude oil closing prices based on average crude oil price plus year effects

crude_oil_average_by_year <- crude_oil_train %>%
  group_by(date_year) %>%
  summarize(b_y = mean(closing_price - mu))

predicted_price_algorithm_05 <- mu + crude_oil_test %>%
  left_join(crude_oil_average_by_year, by= 'date_year') %>%
  pull(b_y)

RMSE05 <- RMSE(predicted_price_algorithm_05, crude_oil_test$closing_price)

RMSE05

### ALGORITHM 06 (RANDOM FOREST - TIME)
##### This algorithm predicts crude oil closing prices based on a Random Forest of time components
####### The first step is to look at a random forest incorporating date, year, quarter, month, and weekday components of time

nodesize <- seq(1, 5, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      date +
                      date_year + 
                      date_quarter +
                      date_month +
                      date_weekday, 
                    data = crude_oil_train, nodesize = n)
  pred <- predict(rf, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf = randomForest(closing_price ~ 
                    date +
                    date_year + 
                    date_quarter +
                    date_month +
                    date_weekday,
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

####### The second is to build a plot to help determine which of those components is/are the most important predictors

varImpPlot(rf)

####### Based on that plot, Random Forest will be revised to focus on the date, year, and month components of time

nodesize <- seq(1, 3, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      date +
                      date_year + 
                      date_month, 
                    data = crude_oil_train, nodesize = n)
  pred <- predict(rf, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf = randomForest(closing_price ~ 
                    date +
                    date_year + 
                    date_month, 
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

pred <- predict(rf, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_06 <- predict(rf, newdata = crude_oil_test)

RMSE06 <- RMSE(predicted_price_algorithm_06, crude_oil_test$closing_price)

RMSE06

### ALGORITHM 07 (KNN - TIME)
##### This algorithm predicts crude oil closing prices based on a K-Nearest Neighbors (KNN) model of time components

train_knn <- train(closing_price ~ 
                     date +
                     date_year + 
                     date_month, 
                   method = "knn",
                   data = crude_oil_train)

ggplot(train_knn, highlight = TRUE)

pred <- predict(train_knn, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_07 <- predict(train_knn, newdata = crude_oil_test)

RMSE07 <- RMSE(predicted_price_algorithm_07, crude_oil_test$closing_price)

RMSE07

### ALGORITHM 08 (RANGER - TIME)
##### This algorithm predicts crude oil closing prices based on a RANGER model of time components

train_ranger <- train(closing_price ~ 
                     date +
                     date_year + 
                     date_month, 
                   method = "ranger",
                   data = crude_oil_train)

pred <- predict(train_ranger, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_08 <- predict(train_ranger, newdata = crude_oil_test)

RMSE08 <- RMSE(predicted_price_algorithm_08, crude_oil_test$closing_price)

RMSE08



