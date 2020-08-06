### This script evaluates a series of potentially viable algorithms for predicting cruide oil prices

### Algorithm 01: Predicted Crude Oil Price Based on Average Crude Oil Price

mu <- mean(crude_oil_train_set$closing_price)

predicted_price_algorithm_01 <- mu

RMSE01 <- RMSE(predicted_price_algorithm_01, crude_oil_test_set$closing_price)

RMSE01

