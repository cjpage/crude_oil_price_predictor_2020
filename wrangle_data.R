library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(lubridate)
library(caret)
library(randomForest)

### This part of the script reads "data/commodity_closing_prices_2010_2020.csv" into a new data frame named "commodities"
### Then, it mutates the resulting data set in way that will enable the envisioned analysis
### Next, it saves that data frame into a .RDATA data set under the RDA subdirectory

commodities <- read_csv("data/commodity_closing_prices_2010_2020.csv")

commodities <- commodities %>%
  mutate(date = as.Date(mdy(Date))) %>%
  mutate(date_year = year(date)) %>%
  mutate(date_quarter = quarter(date)) %>%
  mutate(date_month = month(date)) %>%
  mutate(date_day = day(date)) %>%
  mutate(date_weekday = weekdays(date)) %>%
  mutate(commodity = Commodity) %>%
  mutate(sector = Sector) %>%
  mutate(closing_price = `Closing Price`)
save(commodities, file = "rda/commodities.rdata")

### This part generates and saves data sets focused on crude oil and the energy, precious metals, and agricultural sectors

crude_oil <- commodities %>%
  filter(commodity == 'Crude Oil')
save(commodities, file = "rda/crude_oil.rdata")

energy <- commodities %>%
  filter(sector == 'Energy')
save(commodities, file = "rda/energy.rdata")

precious_metals <- commodities %>%
  filter(sector == 'Precious Metals')
save(commodities, file = "rda/precious_metals.rdata")

agriculture <- commodities %>%
  filter(sector == 'Agriculture')
save(commodities, file = "rda/agriculture.rdata")


