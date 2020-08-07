### This script evaluates a second series of potentially viable algorithms for predicting cruide oil closing prices
### Each of these six algorithms considers crude oil in comparison to other commodities from the energy sector
### The predictors include not only time but the closing prices of natural gas, heating oil, and gasoline

natural_gas <- energy %>%
  filter(commodity == 'Natural Gas') %>%
  mutate(natural_gas_closing_price = closing_price)
save(commodities, file = "rda/natural_gas.rdata")

heating_oil <- energy %>%
  filter(commodity == 'Heating Oil') %>%
  mutate(heating_oil_closing_price = closing_price)
save(commodities, file = "rda/heating_oil.rdata")

gasoline <- energy %>%
  filter(commodity == 'Gasoline') %>%
  mutate(gasoline_closing_price = closing_price)
save(commodities, file = "rda/gasoline.rdata")
