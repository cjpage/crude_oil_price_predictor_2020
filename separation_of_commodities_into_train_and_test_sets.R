library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(lubridate)
library(caret)

load("rda/commodities.rdata")

### This script separates "commodity" into train and test sets

set.seed(755)

test_index <- createDataPartition(y = commodities$Date, times = 1, p = 0.2,
                                  list = FALSE)

commodities_train <- commodities[-test_index,]

commodities_test <- commodities[test_index,]

## Then, it defines RMSE

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



