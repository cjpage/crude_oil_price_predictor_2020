### Generate a data frame from commodity_prices_2010_2020.csv
### Save that data frame as commodities.rdata
library(tidyverse)
commodities <- read_csv("data/commodity_prices_2010_2020.csv")
save(commodities, file = "rda/commodities.rdata")

