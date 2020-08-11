### This script evaluates a series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these eight algorithms considers crude oil in comparison to other commodities from the energy sector
### The available predictors are the key components of time and the closing prices of natural gas, heating oil, and gasoline

### ALGORITHM 10 (RANDOM FOREST - TIME AND ENERGY)
##### This algorithm predicts crude oil closing prices based on a Random Forest of time and energy sector commodities
####### The first step is to look at a random forest incorporating year, month, and closing prices of natural gas, heating oil, and gasoline 

nodesize <- seq(1, 5, 1)

rmses <- sapply(nodesize, function(n){
  rf_time_energy = randomForest(closing_price ~ 
                                  date_year + 
                                  date_month + 
                                  natural_gas_closing_price + 
                                  heating_oil_closing_price + 
                                  gasoline_closing_price, 
                                data = crude_oil_train, nodesize = n)
  pred <- predict(rf_time_energy, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf_time_energy = randomForest(closing_price ~  
                                date_year + 
                                date_month + 
                                natural_gas_closing_price + 
                                heating_oil_closing_price + 
                                gasoline_closing_price, 
                  data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

##### The second is to build a plot to help determine which of those components is/are the most important predictors

varImpPlot(rf_time_energy)

####### Based on that plot, Random Forest will be revised to focus on year, month, heating oil, and gasoline

nodesize <- seq(1, 4, 1)

rmses <- sapply(nodesize, function(n){
  rf_time_energy = randomForest(closing_price ~ 
                                  date_year + 
                                  date_month +
                                  heating_oil_closing_price + 
                                  gasoline_closing_price, 
                                data = crude_oil_train, nodesize = n)
  pred <- predict(rf_time_energy, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf_time_energy = randomForest(closing_price ~ 
                                date_year + 
                                date_month +
                                heating_oil_closing_price + 
                                gasoline_closing_price, 
                              data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

pred <- predict(rf_time_energy, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_10 <- predict(rf_time_energy, newdata = crude_oil_test)

RMSE10 <- RMSE(predicted_price_algorithm_10, crude_oil_test$closing_price)

RMSE10

### ALGORITHM 11 (KNN - TIME AND ENERGY)
##### This algorithm predicts crude oil closing prices based on a KNN model of the four key time and energy components

knn_time_energy <- train(closing_price ~ 
                           date_year + 
                           date_month +
                           heating_oil_closing_price + 
                           gasoline_closing_price,  
                         method = "knn",
                         data = crude_oil_train)

ggplot(knn_time_energy, highlight = TRUE)

pred <- predict(knn_time_energy, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_10 <- predict(knn_time_energy, newdata = crude_oil_test)

RMSE11 <- RMSE(predicted_price_algorithm_10, crude_oil_test$closing_price)

RMSE11

### ALGORITHM 12 (RANGER - TIME AND ENERGY)
##### This algorithm predicts crude oil closing prices based on a RANGER model of the key time and energy components

ranger_time_energy<- train(closing_price ~ 
                             date +
                             date_year + 
                             heating_oil_closing_price + 
                             gasoline_closing_price,  
                           method = "ranger",
                           data = crude_oil_train)

pred <- predict(ranger_time_energy, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_12 <- predict(ranger_time_energy, newdata = crude_oil_test)

RMSE12 <- RMSE(predicted_price_algorithm_12, crude_oil_test$closing_price)

RMSE12

### SUMMARY TO THIS POINT

ALGORITHM_tab <- c("ALGORITHM 01 - Average Closing Price",
                   "ALGORITHM 02 - Average Closing Price plus Day of the Week Effects",
                   "ALGORITHM 03 - Average Closing Price plus Month of the Year Effects",
                   "ALGORITHM 04 - Average Closing Price plus Quarter of the Year Effects",
                   "ALGORITHM 05 - Average Closing Price plus Year Effects",
                   "ALGORITHM 06 - Average Closing Price Plus Month Effects",
                   "ALGORITHM 07 - Random Forest of Time Components",
                   "ALGORITHM 08 - KNN Model of Time Components",
                   "ALGORITHM 09 - Ranger Model of Time Components",
                   "ALGORITHM 10 - Random Forest of Time and Energy Components",
                   "ALGORITHM 11 - KNN Model of Time and Energy Components",
                   "ALGORITHM 12 - Ranger Model of Time and Energy Components")

RMSE_tab <- c(RMSE01,
              RMSE02,
              RMSE03,
              RMSE04,
              RMSE05,
              RMSE06,
              RMSE07,
              RMSE08,
              RMSE09,
              RMSE10,
              RMSE11,
              RMSE12)

Results <- data.frame(ALGORITHM_tab, RMSE_tab)

Results %>%
  arrange(RMSE_tab) %>%
  print(right = FALSE)
