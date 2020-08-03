### First, this script reads "data/commodity_prices_2010_2020.csv" into a new data frame named" commodities"
### Then, it saves that data frame into a compressed .RDATA file under the RDA subdirectory
library(lubridate)
commodities <- read_csv("data/commodity_prices_2010_2020.csv")
save(commodities, file = "rda/commodities.rdata")
commodities <- commodities %>%
  mutate(date = as.Date(mdy(Date))) %>%
  mutate(crude_oil_net_change_since_start_date = `Crude Oil` - `Crude Oil`[1]) %>%
  mutate(natural_gas_net_change_since_start_date = `Natural Gas` - `Natural Gas`[1]) %>%
  mutate(heating_oil_net_change_since_start_date = `Heating Oil` - `Heating Oil`[1]) %>%
  mutate(gasoline_net_change_since_start_date = Gasoline - Gasoline[1]) %>%
  mutate(gold_net_change_since_start_date = Gold - Gold[1]) %>%
  mutate(silver_net_change_since_start_date = Silver - Silver[1]) %>%
  mutate(platinum_net_change_since_start_date = Platinum - Platinum[1]) %>%
  mutate(wheat_net_change_since_start_date = Wheat - Wheat[1]) %>%
  mutate(rice_net_change_since_start_date = Rice - Rice[1]) %>%
  mutate(soybean_net_change_since_start_date = Soybeans - Soybeans[1])
  

