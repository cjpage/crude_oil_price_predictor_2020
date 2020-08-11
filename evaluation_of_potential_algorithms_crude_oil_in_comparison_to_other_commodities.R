### This script evaluates a second series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these algorithms considers crude oil in comparison to other commodities from the energy, precious metals, and agriculture sector
### The predictors include not only time but the closing prices of natural gas, heating oil, gasoline, gold, silver, platinum, wheat, oil, and soybeans

### ALGORITHM 13 (RANDOM FOREST - TIME, ENERGY, PRECIOUS METALS, AGRICULTURE)
##### This algorithm predicts crude oil closing prices based on a Random Forest of time, energy, precious metals, and agriculture sector commodities
####### The first step is to look at a random forest incorporating date, year, and closing prices of eight complementary and competing commodities

nodesize <- seq(1, 10, 1)

rmses <- sapply(nodesize, function(n){
  rf_time_energy_metals_agriculture = randomForest(closing_price ~ 
                                                     date_year + 
                                                     date_month +
                                                     heating_oil_closing_price + 
                                                     gasoline_closing_price +
                                                     gold_closing_price +
                                                     silver_closing_price +
                                                     platinum_closing_price +
                                                     wheat_closing_price +
                                                     rice_closing_price +
                                                     soybeans_closing_price,
                                                   data = crude_oil_train, nodesize = n)
  pred <- predict(rf_time_energy_metals_agriculture, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf_time_energy_metals_agriculture = randomForest(closing_price ~ 
                                                   date_year + 
                                                   date_month +
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

varImpPlot(rf_time_energy_metals_agriculture)

####### Based on that plot, Random Forest will be revised to focus on year, month, heating oil, gasoline, platinum, and soybeans

nodesize <- seq(1, 6, 1)

rmses <- sapply(nodesize, function(n){
  rf_time_energy_metals_agriculture = randomForest(closing_price ~ 
                                                     date_year + 
                                                     date_month +
                                                     heating_oil_closing_price + 
                                                     gasoline_closing_price +
                                                     platinum_closing_price +
                                                     soybeans_closing_price,
                                                   data = crude_oil_train, nodesize = n)
  pred <- predict(rf_time_energy_metals_agriculture, newdata = crude_oil_train)
  RMSE(pred, crude_oil_train$closing_price)
})

qplot(nodesize, rmses)

nodesize[which.min(rmses)]

rf_time_energy_metals_agriculture = randomForest(closing_price ~ 
                                                   date_year + 
                                                   date_month +
                                                   heating_oil_closing_price + 
                                                   gasoline_closing_price +
                                                   platinum_closing_price +
                                                   soybeans_closing_price,
                                                 data = crude_oil_train, nodesize = nodesize[which.min(rmses)])

pred <- predict(rf_time_energy_metals_agriculture, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_13 <- predict(rf_time_energy_metals_agriculture, newdata = crude_oil_test)

RMSE13 <- RMSE(predicted_price_algorithm_13, crude_oil_test$closing_price)

RMSE13

### ALGORITHM 14 (KNN - TIME, ENERGY, PRECIOUS METALS, AGRICULTURE)
##### This algorithm predicts crude oil closing prices based on a KNN model incorporating date, heating oil, platinum, year, gasoline, and soybeans

knn_time_energy_metals_agriculture <- train(closing_price ~ 
                                              date_year + 
                                              date_month +
                                              heating_oil_closing_price + 
                                              gasoline_closing_price +
                                              platinum_closing_price +
                                              soybeans_closing_price, 
                                            method = "knn",
                                            data = crude_oil_train)

ggplot(knn_time_energy_metals_agriculture, highlight = TRUE)

pred <- predict(knn_time_energy_metals_agriculture, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_14 <- predict(knn_time_energy_metals_agriculture, newdata = crude_oil_test)

RMSE14 <- RMSE(predicted_price_algorithm_14, crude_oil_test$closing_price)

RMSE14

### ALGORITHM 15 (RANGER - TIME, ENERGY, PRECIOUS METALS, AGRICULTURE)
##### This algorithm predicts crude oil closing prices based on a RANGER model incorporating date, heating oil, platinum, year, gasoline, and soybeans

ranger_time_energy_metals_agriculture <- train(closing_price ~ 
                                                 date_year + 
                                                 date_month +
                                                 heating_oil_closing_price + 
                                                 gasoline_closing_price +
                                                 platinum_closing_price +
                                                 soybeans_closing_price, 
                                               method = "ranger",
                                               data = crude_oil_train)

pred <- predict(ranger_time_energy_metals_agriculture, newdata = crude_oil_train)

RMSE(pred, crude_oil_train$closing_price)

predicted_price_algorithm_15 <- predict(ranger_time_energy_metals_agriculture, newdata = crude_oil_test)

RMSE15 <- RMSE(predicted_price_algorithm_15, crude_oil_test$closing_price)

RMSE15

### SUMMARY OF FINDINGS

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
                   "ALGORITHM 12 - Ranger Model of Time and Energy Components",
                   "ALGORITHM 13 - Random Forest of Time Components and Energy, Precious Metals,and Agriculture Commodities",
                   "ALGORITHM 14 - KNN Model of Time Components and Energy, Precious Metals,and Agriculture Commodities",
                   "ALGORITHM 15 - Ranger Model of Time Components and Energy, Precious Metals,and Agriculture Commodities")

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
              RMSE12,
              RMSE13,
              RMSE14,
              RMSE14)

Results <- data.frame(ALGORITHM_tab, RMSE_tab)

Results %>%
  arrange(RMSE_tab) %>%
  print(right = FALSE)



