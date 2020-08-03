### First, this script reads "data/commodity_prices_2010_2020.csv" into a new data frame named" commodities"
### Then, it saves that data frame into a compressed .RDATA file under the RDA subdirectory
library(lubridate)
commodities <- read_csv("data/commodity_prices_2010_2020.csv")
save(commodities, file = "rda/commodities.rdata")
commodities <- commodities %>%
  mutate(Date = as.Date(mdy(Date)))
