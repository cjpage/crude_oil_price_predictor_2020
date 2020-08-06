### This script splits "commodities" into training and testing data sets

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(lubridate)
library(caret)

load("rda/commodities.rdata")

set.seed(755)

test_index <- createDataPartition(y = crude_oil$commodity, times = 1, p = 0.2, list = FALSE)

crude_oil_train_set <- crude_oil[-test_index,]

crude_oil_test_set <- crude_oil[test_index,]

