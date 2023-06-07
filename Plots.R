library(R.utils)
library(tidyverse)
library(cowplot)

sourceDirectory("functions", modifiedOnly = FALSE)
bid_ask_data = readRDS("bid_ask_data")

plot_grid(plot_bids_asks(df = bid_ask_data, 
                         month = 3, 
                         day = 5, 
                         hour = 7, 
                         side = "Sell"),
          plot_supply(df = bid_ask_data, 
                      month = 3, 
                      day = 5, 
                      hour = 7)
          )


plot_grid(plot_bids_asks(df = bid_ask_data, 
                         month = 3, 
                         day = 5, 
                         hour = 7, 
                         side = "Buy"),
          plot_demand(df = bid_ask_data, 
                      month = 3, 
                      day = 5, 
                      hour = 7)
          )

plot_supply_demand(df = bid_ask_data, month = 3, day = 5, hour = 7)
