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
  rename(date = Date) %>%
  mutate(date = as.Date(mdy(date))) %>%
  mutate(date_year = year(date)) %>%
  mutate(date_quarter = quarter(date)) %>%
  mutate(date_month = month(date)) %>%
  mutate(date_day = day(date)) %>%
  mutate(date_weekday = weekdays(date)) %>%
  rename(commodity = Commodity) %>%
  rename(sector = Sector) %>%
  rename(closing_price = `Closing Price`)

save(commodities, file = "rda/commodities.rdata")

### This part generates and saves data sets focused on crude oil and the energy, precious metals, and agricultural sectors

energy <- commodities %>%
  filter(sector == 'Energy')
save(energy, file = "rda/energy.rdata")

precious_metals <- commodities %>%
  filter(sector == 'Precious Metals')
save(precious_metals, file = "rda/precious_metals.rdata")

agriculture <- commodities %>%
  filter(sector == 'Agriculture')
save(agriculture, file = "rda/agriculture.rdata")

### This part generates and saves data sets focused on each of the individual commodities in the three sectors

crude_oil <- energy %>%
  filter(commodity == 'Crude Oil') %>%
  mutate(crude_oil_closing_price = closing_price)
save(crude_oil, file = "rda/crude_oil.rdata")

natural_gas <- energy %>%
  filter(commodity == 'Natural Gas') %>%
  mutate(natural_gas_closing_price = closing_price)
save(natural_gas, file = "rda/natural_gas.rdata")

heating_oil <- energy %>%
  filter(commodity == 'Heating Oil') %>%
  mutate(heating_oil_closing_price = closing_price)
save(natural_gas, file = "rda/heating_oil.rdata")

gasoline <- energy %>%
  filter(commodity == 'Gasoline') %>%
  mutate(gasoline_closing_price = closing_price)
save(gasoline, file = "rda/gasoline.rdata")


