### First, this script reads "data/commodity_closing_prices_2010_2020.csv" into a new data frame named "commodities2"
### Then, it saves that data frame into a compressed .RDATA data set under the RDA subdirectory
### Finally, it mutates the data set to support enable the envisioned analysis
library(lubridate)
commodities2 <- read_csv("data/commodity_closing_prices_2010_2020.csv")
commodities2 <- commodities2 %>%
  mutate(date = as.Date(mdy(Date)))

save(commodities2, file = "rda/commodities2.rdata")

