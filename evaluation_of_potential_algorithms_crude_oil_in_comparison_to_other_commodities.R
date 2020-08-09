r### This script evaluates a second series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these algorithms considers crude oil in comparison to other commodities from the energy, precious metals, and agriculture sector
### The predictors include not only time but the closing prices of natural gas, heating oil, gasoline, gold, silver, platinum, wheat, oil, and soybeans

### ALGORITHM 12 (RANDOM FOREST - TIME, ENERGY, PRECIOUS METALS, AGRICULTURE)
##### This algorithm predicts crude oil closing prices based on a Random Forest of time, energy, precious metals, and agriculture sector commodities
####### The first step is to look at a random forest incorporating date, year, month, and closing prices of nine other complementary and competing commodities

nodesize <- seq(1, 12, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      date +
                      date_year + 
                      date_month + 
                      natural_gas_closing_price + 
                      heating_oil_closing_price + 
                      gasoline_closing_price +
                      gold_closing_price +
                      silver_closing_price +
                      platinum_closing_price +
                      wheat_closing_price +
                      rice_closing_price +
                      soybeans_closing_price,
                  data = crude_oil_train, nodesize = n)
  pred <- predict(rf, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf = randomForest(closing_price ~ 
                    date +
                    date_year + 
                    date_month + 
                    natural_gas_closing_price + 
                    heating_oil_closing_price + 
                    gasoline_closing_price +
                    gold_closing_price +
                    silver_closing_price +
                    platinum_closing_price +
                    wheat_closing_price +
                    rice_closing_price +
                    soybeans_closing_price, 
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

##### The second is to build a plot to help determine which of those components is/are the most important predictors

varImpPlot(rf)

####### The random forest is the basis of the twelfth algorithm in this series

pred <- predict(rf, newdata = crude_oil_train)
RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_12 <- predict(rf, newdata = crude_oil_test)
RMSE12 <- RMSE(predicted_price_algorithm_12, crude_oil_test$closing_price)

RMSE12

### ALGORITHM 13 (KNN - TIME, ENERGY, PRECIOUS METALS, AGRICULTURE)
##### This algorithm predicts crude oil closing prices based on a KNN model incorporating date, year, month, and closing prices of nine other complementary and competing commodities

train_knn <- train(closing_price ~ 
                     date +
                     date_year + 
                     date_month +
                     natural_gas_closing_price +
                     heating_oil_closing_price + 
                     gasoline_closing_price +
                     gold_closing_price +
                     platinum_closing_price +
                     silver_closing_price +
                     wheat_closing_price +
                     rice_closing_price +
                     soybeans_closing_price, 
                   method = "knn",
                   data = crude_oil_train)

ggplot(train_knn, highlight = TRUE)

pred <- predict(train_knn, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_13 <- predict(train_knn, newdata = crude_oil_test)

RMSE13 <- RMSE(predicted_price_algorithm_13, crude_oil_test$closing_price)

RMSE13

### ALGORITHM 14 (RANGER - TIME, ENERGY, PRECIOUS METALS, AGRICULTURE)
##### This algorithm predicts crude oil closing prices based on a RANGER model incorporating date, year, month, and closing prices of nine other complementary and competing commodities

train_ranger <- train(closing_price ~ 
                     date +
                     date_year + 
                     date_month +
                     natural_gas_closing_price +
                     heating_oil_closing_price + 
                     gasoline_closing_price +
                     gold_closing_price +
                     platinum_closing_price +
                     silver_closing_price +
                     wheat_closing_price +
                     rice_closing_price +
                     soybeans_closing_price, 
                   method = "ranger",
                   data = crude_oil_train)

pred <- predict(train_ranger, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_14 <- predict(train_ranger, newdata = crude_oil_test)

RMSE14 <- RMSE(predicted_price_algorithm_14, crude_oil_test$closing_price)

RMSE14



