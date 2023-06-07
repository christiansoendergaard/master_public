
# Packages ----------------------------------------------------------------

library(tidyverse)
library(R.utils)
library(tictoc)

sourceDirectory("functions", modifiedOnly = FALSE)   


# Reading the data --------------------------------------------------------

data = readRDS("RDS/bid_ask_data")
m = readRDS("RDS/month_indexes")
d = readRDS("RDS/day_indexes")
h = readRDS("RDS/hour_indexes")


# input vectors -----------------------------------------------------------

# number of hours in all months 
hours_in_months = map_dbl(
  .x = 1:11,
  .f = function(x) {
    count_hours_days_in_month(data = data, month = x)$hours_in_month
  }
)   

# days in all months (due to summer time and standard time, we do not have an integer number of days in March and October)
days_in_months = map_dbl(
  .x = 1:11,
  .f = function(x) {
    count_hours_days_in_month(data = data, month = x)$days_in_month
  }
)   

# using pmap() we need indexes for months, days and hours to run through all data
# month indexes 
m = map2(
  .x = 1:11,
  .y = hours_in_months,
  .f = rep
) %>% unlist() 

# day indexes
d = map(
  .x = 1:11, 
  .f = function(x){
    get_hour_day_indexes(data = data, month = x)$day_index_list
  }
) %>% unlist()

# day indexes
h = map(                                                                         
  .x = 1:11,
  .f = function(x) {
    get_hour_day_indexes(data = data, month = x)$hour_index_list
  }
) %>% unlist()




tic()
# conctructing all input vectors (using the month, day and hour indexes above)
inputs = pmap(.l = list(month = m , day = d , hour = h ),
                     .f = get_input_vector,
                     data = data) %>%
  setNames(nm = paste0(
    "2022-",
    sprintf('%0.2d', m ),
    "-",
    sprintf('%0.2d', d ),
    " hour ",
    sprintf('%0.2d', h )
  ))
toc()

# save all input vectors
saveRDS(object = inputs, file = "RDS/input_vectors_raw")
inputs = readRDS("RDS/input_vectors")

# New input vectors
tic()
input_vectors = aggregate_quantities(data = data, 
                     input_vectors = inputs, 
                     month_indexes = m, 
                     day_indexes = d, 
                     hour_indexes = h)
toc()
saveRDS(object = input_vectors, file = "RDS/input_vectors")

# output vectors ----------------------------------------------------------

# conctructing all output vectors (using the month, day and hour indexes above)
tic()
output_vectors = pmap(.l = list(month = m, day = d, hour = h ),
                      .f = get_output_vector,
                      df = data) %>%
  setNames(nm = paste0(
    "2022-",
    sprintf('%0.2d', m),
    "-",
    sprintf('%0.2d', d),
    " hour ",
    sprintf('%0.2d', h)
  ))
toc()

# save all output vectors
saveRDS(object = output_vectors, file = "RDS/output_vectors")
output_vectors = readRDS("RDS/output_vectors")

# Below will generate a list of over all hours with both normalized and non-normalized data
tic()
prices_quantities = pmap(.l = list(month = m, day = d, hour = h),
                         .f = get_prices_quantities,
                         data = data) %>%
  setNames(nm = paste0(
    "2022-",
    sprintf('%0.2d', m),
    "-",
    sprintf('%0.2d', d),
    " hour ",
    sprintf('%0.2d', h)
  ))
toc()

saveRDS(object = prices_quantities, file = "RDS/data_input_output")

