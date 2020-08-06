### First, this script reads "data/commodity_closing_prices_2010_2020.csv" into a new data frame named "commodities"
### Then, it saves that data frame into a compressed .RDATA data set under the RDA subdirectory
### Finally, it mutates the data set to support enable the envisioned analysis

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(lubridate)
library(caret)

commodities <- read_csv("data/commodity_closing_prices_2010_2020.csv")
commodities <- commodities %>%
  mutate(date = as.Date(mdy(Date))) %>%
  mutate(commodity = Commodity) %>%
  mutate(sector = Sector) %>%
  mutate(closing_price = `Closing Price`)

save(commodities, file = "rda/commodities.rdata")

