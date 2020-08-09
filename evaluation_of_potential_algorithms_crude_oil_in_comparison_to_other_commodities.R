r### This script evaluates a second series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these algorithms considers crude oil in comparison to other commodities from the energy, precious metals, and agriculture sector
### The predictors include not only time but the closing prices of natural gas, heating oil, gasoline, gold, silver, platinum, wheat, oil, and soybeans

### Algorithm 07: Predicted Crude Oil Price Based on Random Forest of Time and Energy Effects

#### The first step is to look at a random forest incorporating the key time components plus natural gas, heating oil, and gasoline

nodesize <- seq(1, 5, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
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
                    date_year + 
                    date_month + 
                    natural_gas_closing_price + 
                    heating_oil_closing_price + 
                    gasoline_closing_price, 
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

##### The second is to build a plot to help determine which of those components is/are the most important predictors

varImpPlot(rf)

###### Based on that plot, random forest will be revised to focus on heating oil, gasoline, year, and natural gas effects

nodesize <- seq(1, 4, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      date_year + 
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
                    date_year + 
                    natural_gas_closing_price + 
                    heating_oil_closing_price + 
                    gasoline_closing_price, 
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

####### That revised random forest is the basis of the seventh algorithm in this series

pred <- predict(rf, newdata = crude_oil_train)
RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_07 <- predict(rf, newdata = crude_oil_test)
RMSE07 <- RMSE(predicted_price_algorithm_07, crude_oil_test$closing_price)

RMSE07

### Algorithm 08: Predicted Crude Oil Price Based on Random Forest of Time, Energy, and Precious Metal Effects

#### The first step is to look at a random forest incorporating the key time and energy components plus gold, silver, and platinum

nodesize <- seq(1, 7, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      date_year + 
                      natural_gas_closing_price + 
                      heating_oil_closing_price + 
                      gasoline_closing_price +
                      gold_closing_price +
                      silver_closing_price +
                      platinum_closing_price,
                    data = crude_oil_train, nodesize = n)
  pred <- predict(rf, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf = randomForest(closing_price ~ 
                    date_year + 
                    natural_gas_closing_price + 
                    heating_oil_closing_price + 
                    gasoline_closing_price +
                    gold_closing_price +
                    silver_closing_price +
                    platinum_closing_price,
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

##### The second is to build a plot to help determine which of those components is/are the most important predictors

varImpPlot(rf)

###### Based on that plot, random forest will be revised to focus on heating oil, platinum, gasoline, year, and silver effects

nodesize <- seq(1, 6, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      heating_oil_closing_price + 
                      platinum_closing_price +
                      gasoline_closing_price +
                      date_year + 
                      silver_closing_price +
                      natural_gas_closing_price, 
                    data = crude_oil_train, nodesize = n)
  pred <- predict(rf, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf = randomForest(closing_price ~ 
                    heating_oil_closing_price + 
                    platinum_closing_price +
                    gasoline_closing_price +
                    date_year + 
                    silver_closing_price +
                    natural_gas_closing_price, 
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

####### That revised random forest is the basis of the eighth algorithm in this series

pred <- predict(rf, newdata = crude_oil_train)
RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_08 <- predict(rf, newdata = crude_oil_test)
RMSE08 <- RMSE(predicted_price_algorithm_08, crude_oil_test$closing_price)

RMSE08

### Algorithm 09: Predicted Crude Oil Price Based on Random Forest of Time, Energy, Precious Metal, and Agriculture Effects

#### The first step is to look at a random forest incorporating the key time, energy, and precious metal components plus wheat, rice, and soybeans

nodesize <- seq(1, 9, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      heating_oil_closing_price + 
                      platinum_closing_price +
                      gasoline_closing_price +
                      date_year + 
                      silver_closing_price +
                      natural_gas_closing_price +
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
                    heating_oil_closing_price + 
                    platinum_closing_price +
                    gasoline_closing_price +
                    date_year + 
                    silver_closing_price +
                    natural_gas_closing_price +
                    wheat_closing_price +
                    rice_closing_price +
                    soybeans_closing_price,
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

##### The second is to build a plot to help determine which of those components is/are the most important predictors

varImpPlot(rf)

###### Based on that plot, random forest will be revised to focus on heating oil, platinum, year, gasoline, soybean, and wheat effects

nodesize <- seq(1, 6, 1)

rmses <- sapply(nodesize, function(n){
  rf = randomForest(closing_price ~ 
                      heating_oil_closing_price +
                      platinum_closing_price +
                      date_year + 
                      gasoline_closing_price +
                      soybeans_closing_price +
                      wheat_closing_price, 
                    data = crude_oil_train, nodesize = n)
pred <- predict(rf, newdata = crude_oil_train)
RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf = randomForest(closing_price ~ 
                    heating_oil_closing_price +
                    platinum_closing_price +
                    date_year + 
                    gasoline_closing_price +
                    soybeans_closing_price +
                    wheat_closing_price, 
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

pred <- predict(rf, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_09 <- predict(rf, newdata = crude_oil_test)

RMSE09 <- RMSE(predicted_price_algorithm_09, crude_oil_test$closing_price)

RMSE09

###

rf = randomForest(closing_price ~ .,
                  data = crude_oil_train, nodesize = 1)

pred <- predict(rf, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_10 <- predict(rf, newdata = crude_oil_test)

RMSE10 <- RMSE(predicted_price_algorithm_10, crude_oil_test$closing_price)

RMSE10

varImpPlot(rf)

#### KNN

train_knn <- train(closing_price ~ 
                     date +
                     date_year + 
                     date_month +
                     date_weekday +
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

predicted_price_algorithm_11 <- predict(train_knn, newdata = crude_oil_test)

RMSE11 <- RMSE(predicted_price_algorithm_11, crude_oil_test$closing_price)

RMSE11

train_glm <- train(closing_price ~ 
                     date +
                     date_year + 
                     date_month +
                     date_weekday +
                     natural_gas_closing_price +
                     heating_oil_closing_price + 
                     gasoline_closing_price +
                     gold_closing_price +
                     platinum_closing_price +
                     silver_closing_price +
                     wheat_closing_price +
                     rice_closing_price +
                     soybeans_closing_price, 
                   method = "glm",
                   data = crude_oil_train)

pred <- predict(train_glm, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_12 <- predict(train_glm, newdata = crude_oil_test)

RMSE12 <- RMSE(predicted_price_algorithm_12, crude_oil_test$closing_price)

RMSE12

train_kknn <- train(closing_price ~ 
                     date +
                     date_year + 
                     date_month +
                     date_weekday +
                     natural_gas_closing_price +
                     heating_oil_closing_price + 
                     gasoline_closing_price +
                     gold_closing_price +
                     platinum_closing_price +
                     silver_closing_price +
                     wheat_closing_price +
                     rice_closing_price +
                     soybeans_closing_price, 
                   method = "kknn",
                   data = crude_oil_train)

pred <- predict(train_kknn, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_13 <- predict(train_kknn, newdata = crude_oil_test)

RMSE13 <- RMSE(predicted_price_algorithm_13, crude_oil_test$closing_price)

RMSE13

train_ranger <- train(closing_price ~ 
                      date +
                      date_year + 
                      date_month +
                      date_weekday +
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
