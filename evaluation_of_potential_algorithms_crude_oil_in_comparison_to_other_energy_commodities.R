### This script evaluates a series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these eight algorithms considers crude oil in comparison to other commodities from the energy sector
### The available predictors are the key components of time and the closing prices of natural gas, heating oil, and gasoline

### ALGORITHM 09 (RANDOM FOREST - TIME AND ENERGY)
##### This algorithm predicts crude oil closing prices based on a Random Forest of time and energy sector commodities
####### The first step is to look at a random forest incorporating date, year, month, and closing prices of natural gas, heating oil, and gasoline 

nodesize <- seq(1, 6, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      date +
                      date_year + 
                      date_month + 
                      natural_gas_closing_price + 
                      heating_oil_closing_price + 
                      gasoline_closing_price, 
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
                    gasoline_closing_price, 
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

##### The second is to build a plot to help determine which of those components is/are the most important predictors

varImpPlot(rf)

####### The random forest is the basis of the ninth algorithm in this series

pred <- predict(rf, newdata = crude_oil_train)
RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_09 <- predict(rf, newdata = crude_oil_test)
RMSE09 <- RMSE(predicted_price_algorithm_09, crude_oil_test$closing_price)

RMSE09

### ALGORITHM 10 (KNN - TIME AND ENERGY)
##### This algorithm predicts crude oil closing prices based on a KNN model of time and energy components

train_knn <- train(closing_price ~ 
                     date +
                     date_year + 
                     date_month + 
                     natural_gas_closing_price + 
                     heating_oil_closing_price + 
                     gasoline_closing_price,  
                   method = "knn",
                   data = crude_oil_train)

ggplot(train_knn, highlight = TRUE)

pred <- predict(train_knn, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_10 <- predict(train_knn, newdata = crude_oil_test)

RMSE10 <- RMSE(predicted_price_algorithm_10, crude_oil_test$closing_price)

RMSE10

### ALGORITHM 11 (RANGER - TIME AND ENERGY)
##### This algorithm predicts crude oil closing prices based on a RANGER model of time components

train_ranger <- train(closing_price ~ 
                        date +
                        date_year + 
                        date_month + 
                        natural_gas_closing_price + 
                        heating_oil_closing_price + 
                        gasoline_closing_price,  
                      method = "ranger",
                      data = crude_oil_train)

pred <- predict(train_ranger, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_11 <- predict(train_ranger, newdata = crude_oil_test)

RMSE11 <- RMSE(predicted_price_algorithm_11, crude_oil_test$closing_price)

RMSE11

