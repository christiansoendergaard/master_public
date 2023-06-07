
# -------------------------------------------------------------------------
# Shift curves (data generation) ------------------------------------------
# -------------------------------------------------------------------------

# packages, sourcing functions and load data ------------------------------

# packages
library(tidyverse)
library(R.utils)
library(tictoc)
library(dplyr)

# source functions
sourceDirectory("functions", modifiedOnly = FALSE) 

# load data
data_plot = readRDS(file = "RDS/bid_ask_data")
data = readRDS(file = "RDS/data_input_output")
output = readRDS(file = "RDS/output_vectors")
m = readRDS(file = "RDS/month_indexes")
d = readRDS(file = "RDS/day_indexes")
h = readRDS(file = "RDS/hour_indexes")


# shift curves ------------------------------------------------------------

# function that creates shifted curves. These are shifted by adding constants to
# all aggregated quantities (hence shifted along the first axis). For each day
# ten shifted curves are made
shift_curve = function(data, output, month, day, hour){ 
  
  date = paste0(
    "2022-",
    sprintf('%0.2d', month),
    "-",
    sprintf('%0.2d', day),
    " hour ",
    sprintf('%0.2d', hour)
  )
  
  demand_quant = get("quantity_bid" , get(date, data))   # aggregates quantities from demand curve
  supply_quant = get("quantity_ask" , get(date, data))   # aggregates quantities from supply curve
  demand_price = get("price_bid" , get(date, data))      # prices from demand curve
  supply_price = get("price_ask" , get(date, data))      # prices from supply curve
  
  output = get(date, output)   # c(price, agg_quantity), crossing point of the curves
  
  left_demand_quant = demand_quant[demand_quant < output[2]]    # aggregated quantities from points before the MCP point (demand curve) 
  right_demand_quant = demand_quant[demand_quant > output[2]]   # aggregated quantities from points after the MCP point (demand curve)
  
  # demand_quant = c(left_demand_quant, output[2], output[2], output[2], right_demand_quant)   # REMOVE THIS
  # demand_price = c(demand_price[1:108], 50.8 ,50.5, 50.1, 50.05, demand_price[110:292])  # REMOVE THIS
  
  n_left_demand = left_demand_quant %>% length()     # number of aggregated quantities smaller than the MCP quantity
  n_right_demand = right_demand_quant %>% length()   # number of aggregated quantities greater than the MCP quantity
  
  if ( (n_left_demand + n_right_demand)  != length(demand_quant) ){   # run if multiple MCP aggregated quantities
    index_output_quant = which(demand_quant == output[2])   # indexes of MCP point and points having the same aggregated quantity 
    
    left_index = index_output_quant[which(demand_price[index_output_quant] < output[1])]    # index of point(s) to the left of the MCP
    right_index = index_output_quant[which(demand_price[index_output_quant] > output[1])]   # index of point(s) to the right of the MCP
    
    left_demand_quant = c(left_demand_quant, demand_quant[left_index])      # adding point(s) to the left of the MCP
    right_demand_quant = c(demand_quant[right_index], right_demand_quant)   # adding point(s) to the right of the MCP
    
    n_left_demand = left_demand_quant %>% length()     
    n_right_demand = right_demand_quant %>% length()   
  }
  
  if ( (n_left_demand + n_right_demand) == length(demand_quant) ) {   # run if when the MCP point does not directly lie on the demand curve
    n_left_shift = (n_left_demand / length(demand_quant) * 10)  %>% round()    # number of shifts to the left
    n_right_shift = (n_right_demand / length(demand_quant)  * 10) %>% round()   # number of shifts to the right
  } else {
    n_left_shift = (n_left_demand / (length(demand_quant) - 1)   * 10)  %>% round()    # number of shifts to the left
    n_right_shift = (n_right_demand / (length(demand_quant) - 1) * 10) %>% round()   # number of shifts to the right
  }
  
  left_demand_quant = left_demand_quant[-(1:10)]                                                          # remove the ten most extreme points
  right_demand_quant = right_demand_quant[-((length(right_demand_quant)-9):length(right_demand_quant))]   # remove the ten most extreme points
  
  max_shift_length = c(output[2] - left_demand_quant[1], 
                       right_demand_quant[length(right_demand_quant)] - output[2])   # maximum shift to the left and the right
  
  shift_length = c(seq(-max_shift_length[1], - max_shift_length[1] / n_left_shift, max_shift_length[1] / n_left_shift),
                   seq(max_shift_length[2] / n_right_shift, max_shift_length[2], max_shift_length[2] / n_right_shift ))   # constant we add to the aggregated quantities
  
  shifted_quant = map(   # created the shifted aggregates quantities (add constants)
    .x = shift_length,
    .f = function(x) {
      supply_quant + x
    }
  ) %>% set_names(map(.x = 1:10, .f = function(x) {paste0("quantity_ask_shift", x)}) %>% unlist)
  
  diff_shifted_quant = map(   # take the difference to be able to apply the find_equilibrium function
    .x = shifted_quant,
    .f = function(x){
      c(x[1], diff(x))
    }
  )
  
  diff_demand_quant = c(demand_quant[1], diff(demand_quant))
  
  shifted_output = map(   # find the MCP using the shifted supply curves
    .x = diff_shifted_quant,
    .f = function(x) {find_equilibrium(price_supply = supply_price,
                            quant_supply = x,
                            price_demand = demand_price,
                            quant_demand = diff_demand_quant) %>% 
        unlist() %>% 
        as.vector 
      }
  ) %>% set_names(map(.x = 1:10, .f = function(x) {paste0("output_shift", x)}) %>% unlist)
  
  original = list(price_bid = demand_price,   # the non-shifted data
                  quantity_bid = demand_quant, 
                  price_ask = supply_price,
                  quantity_ask = supply_quant,
                  output = output)
  
  return(append(original, append(shifted_quant, shifted_output)))
  
}

tic()
shifted_data = pmap(.l = list(month = m, day = d, hour = h),
                         .f = shift_curve,
                         data = data,
                         output = output) %>%
  setNames(nm = paste0(
    "2022-",
    sprintf('%0.2d', m),
    "-",
    sprintf('%0.2d', d),
    " hour ",
    sprintf('%0.2d', h)
  ))
toc()

saveRDS(object = shifted_data, file = "RDS/shifted_data")

# # plot curve and shifted curves
# date = paste0("2022-", sprintf('%0.2d', 9), "-", sprintf('%0.2d', 12), " hour ", sprintf('%0.2d', 21))
# plot(x = get("quantity_bid" , get(date, data)), y = get("price_bid" , get(date, data)), xlim = c(10000,55000))
# points(x = get("quantity_ask" , get(date, data)), y = get("price_ask" , get(date, data)), col = "red")
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift1", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift2", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift3", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift4", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift5", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift6", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift7", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift8", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift9", y = get("price_ask" , get(date, data)), col = "maroon" )
# points(x = shift_curve(data,output,9,12,21)$"quantity_ask_shift10", y = get("price_ask" , get(date, data)), col = "maroon" )










