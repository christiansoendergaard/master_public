# packages, sourcing functions and load data ------------------------------

# packages
library(purrr)
library(tidyverse)
library(R.utils)
library(tictoc)
library(keras)

# load data
data = readRDS(file = "RDS/data_input_output")
data_plot = readRDS(file = "RDS/bid_ask_data")
output_w_quant = readRDS(file = "RDS/output_vectors")

# check for seasonality ---------------------------------------------------

# function computing the daily average MCP
get_daily_avg_MCP = function(output_w_quant, month, day){ 
  date = paste0(
    "2022-",
    sprintf('%0.2d', month),
    "-",
    sprintf('%0.2d', day)
  )
  
  if (date != "2022-10-30" &
      date != "2022-03-27") {
    dates = map(
      .x = 1:24,
      .f = function(x) {
        paste0(
          "2022-",
          sprintf('%0.2d', month),
          "-",
          sprintf('%0.2d', day),
          " hour ",
          sprintf('%0.2d', x)
        )
      }
    ) %>% unlist()   # extract dates 
  } else if (date == "2022-10-30") {
    dates = map(
      .x = 1:25,
      .f = function(x) {
        paste0(
          "2022-",
          sprintf('%0.2d', month),
          "-",
          sprintf('%0.2d', day),
          " hour ",
          sprintf('%0.2d', x)
        )
      }
    ) %>% unlist()   # extract dates
  } else if (date == "2022-03-27"){
    dates = map(
      .x = c(1:2,4:24),
      .f = function(x) {
        paste0(
          "2022-",
          sprintf('%0.2d', month),
          "-",
          sprintf('%0.2d', day),
          " hour ",
          sprintf('%0.2d', x)
        )
      }
    ) %>% unlist()   # extract dates
  }
  
  MCPs = map_dbl(.x = dates,
                 .f = function(x){
                   get(x = x, pos = output_w_quant)[1]
                 })
  
  mean = sum(MCPs) / length(MCPs)
  
  return(mean)
}

# compute the average MCP for each day
MCP_tt = map2_dbl(.x = m[which(h == 1)], 
                  .y = d[which(h == 1)],
                  .f = get_daily_avg_MCP,
                  output_w_quant = output_w_quant)

ggplot() +
  geom_line(mapping = aes(x = 1:325, y = MCP_tt)) +
  scale_x_continuous(breaks = which(d[which(h == 1)] == 1),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")) + 
  theme_bw() +
  labs(title = "Daily average prices", x = "", y = "Price")

ggplot() +
  scale_x_continuous(breaks = which(d[which(h == 1)] == 1),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")) + 
  theme_bw() +
  geom_line(mapping = aes(x = 1:325, y = output[which(h == 1)])) + 
  geom_line(mapping = aes(x = seq(5,325,7), y = output[which(h == 1)][seq(5,325,7)]), col = "red") +
  geom_line(mapping = aes(x = seq(1,325,7), y = output[which(h == 1)][seq(1,325,7)]), col = "blue")



