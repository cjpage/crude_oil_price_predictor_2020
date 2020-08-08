### This script splits "crude_oil" into training and testing data sets

set.seed(755)

test_index <- createDataPartition(y = crude_oil_in_commodities_market$commodity, times = 1, p = 0.2, list = FALSE)

crude_oil_train <- crude_oil_in_commodities_market[-test_index,]

crude_oil_test <- crude_oil_in_commodities_market[test_index,]

