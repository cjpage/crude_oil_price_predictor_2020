### This script splits "crude_oil" into training and testing data sets

set.seed(755)

test_index <- createDataPartition(y = crude_oil$commodity, times = 1, p = 0.2, list = FALSE)

crude_oil_train_set <- crude_oil[-test_index,]

crude_oil_test_set <- crude_oil[test_index,]

