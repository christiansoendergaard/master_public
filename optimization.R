# Loading required packages and functions
library(nloptr)
library(purrr)
library(tidyverse)
library(forecast)
library(R.utils)
library(keras)
library(tensorflow)
library(tictoc)
sourceDirectory("functions", modifiedOnly = FALSE)

# Loading all of the data needed in the optimization
x_test_opt = readRDS("RDS/x_test_opt")
y_test_opt = readRDS("RDS/y_test_opt")
x_train_with_day = readRDS("RDS/x_train_with_day")
y_train = readRDS("RDS/y_train")
max = readRDS("RDS/max")
m = readRDS(file = "RDS/month_indexes")
d = readRDS(file = "RDS/day_indexes")
h = readRDS(file = "RDS/hour_indexes")

# Loading the keras model
model = load_model_tf("model_shifted_5")

# Check that the correct model is loaded
model %>% evaluate(t(x_test), y_test) # loss = 1225.9066, mae = 21.9069

# Optimization problem ----------------------------------------------------
# Before running any of the functions make sure the correct model is loaded, 
# and make sure to use x_data_with_day even if the model only takes x_train as input


# Scenario 1 with no constraints ------------------------------------------

months = 1:11
tic()
scenario1_train = map(.x = months,
                      .f = opt_monthly_train,
                      x_data = x_train_with_day,
                      y_data = output,
                      max = max,
                      budget = 0.4) %>% set_names(paste0("month", months))
toc()
saveRDS(object = scenario1_train, file = "RDS/monthly_no_constraints")
scenario1_train = readRDS("RDS/monthly_no_constraints")

# to get a list of all the found epsilons
epsilon_list = map(.x = paste0("month", 1:11), 
                   .f = function(x) get("epsilons", get(x, scenario1_train))) %>% flatten_dbl()

# Using the found epsilons to test
scenario1_test = map2(.x = epsilon_list, 
                      .y = 1:11,
                      .f = opt_monthly_test,
                      x_data = x_test_opt,
                      y_data = y_test_opt,
                      max = max,
                      budget = 0.4) %>% set_names(paste0("month", months))

#plot_epsilons_monthly(month = 1, epsilon_list = epsilon_list, min_quant = min_quant, max_quant = max_quant, days = days_month1, x_data = )
# Scenario 2 with no constraints ------------------------------------------

days = c(25:31, #Jan
         (31+22):(31+28), #Feb
         (31+28+25):(31+28+31), #Mar
         (31+28+31+24):(31+28+31+30), #Apr
         (31+28+31+30+25):(31+28+31+30+31), #May
         (31+28+31+30+31+24):(31+28+31+30+31+30), #June
         (31+28+31+30+31+30+25):(31+28+31+30+31+30+31), #July
         (31+28+31+30+31+30+31+25):(31+28+31+30+31+30+31+31), #Aug
         (31+28+31+30+31+30+31+31+24):(31+28+31+30+31+30+31+31+30), #Sep
         (31+28+31+30+31+30+31+31+30+25):(31+28+31+30+31+30+31+31+30+31), #Oct
         (31+28+31+30+31+30+31+31+30+31+15):(31+28+31+30+31+30+31+31+30+31+21)) #Nov
tic()
scenario2_sarima = map(.x = days, 
                       .f = opt_daily,
                       x_train_data = x_train_with_day,
                       x_test_data = x_test_opt,
                       y_data = output,
                       max = max,
                       budget = 1,
                       perfect_forecast = FALSE) %>% set_names(paste0("day", days))
toc()

saveRDS(object = scenario2_sarima, file = "RDS/daily_sarima_no_constraints")
scenario2_sarima = readRDS("RDS/daily_sarima_no_constraints")

obj_list = map_dbl(.x = paste0("day", days),
                   .f = function(x) get("objective_func", get(x, scenario2_sarima)))

prices = map(.x = paste0("day", days),
                 .f = function(x) get("prices", get(x, scenario2_sarima))) %>% flatten_dbl()

new_prices = map(.x = paste0("day", days),
                    .f = function(x) get("new_prices", get(x, scenario2_sarima))) %>% flatten_dbl()

target = map_dbl(.x = paste0("day", days),
              .f = function(x) get("target", get(x, scenario2_sarima)))

values = map(.x = 1:11,
             .f = plot_weekly,
             prices = prices,
             new_prices = new_prices,
             target = target,
             perfect_forecast = FALSE) %>% set_names(paste0("month", 1:11))




tic()
scenario2_perfect_cobyla = map(.x = days, 
                       .f = opt_daily,
                       x_train_data = x_train_with_day,
                       x_test_data = x_test_opt,
                       y_data = output,
                       max = max,
                       budget = 0.5,
                       perfect_forecast = TRUE) %>% set_names(paste0("day", days))
toc()

saveRDS(object = scenario2_perfect_cobyla, file = "RDS/daily_perfect_no_constraints_cobyla")
scenario2_perfect_cobyla = readRDS("RDS/daily_perfect_no_constraints_cobyla")

obj_list = map_dbl(.x = paste0("day", days),
                   .f = function(x) get("objective_func", get(x, scenario2_perfect_cobyla)))

prices = map(.x = paste0("day", days),
             .f = function(x) get("prices", get(x, scenario2_perfect_cobyla))) %>% flatten_dbl()

new_prices = map(.x = paste0("day", days),
                 .f = function(x) get("new_prices", get(x, scenario2_perfect_cobyla))) %>% flatten_dbl()

target = map_dbl(.x = paste0("day", days),
                 .f = function(x) get("target", get(x, scenario2_perfect_cobyla)))

values = map(.x = 1:11,
             .f = plot_weekly,
             prices = prices,
             new_prices = new_prices,
             target = target,
             perfect_forecast = TRUE) %>% set_names(paste0("month", 1:11))


epsilon_list = map(.x = paste0("day", days), 
                   .f = function(x) get("epsilons", get(x, scenario2_perfect_cobyla))) %>% flatten_dbl()


# Creating plots of the strategies ------------------------------------------

days_month1 = c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday")
days_month2 = c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday")
days_month3 = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday")
days_month4 = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
days_month5 = c("Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday")
days_month6 = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday")
days_month7 = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
days_month8 = c("Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")
days_month9 = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
days_month10 = c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday")
days_month11 = c("Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")

weekdays = list(days_month1, 
                days_month2, 
                days_month3, 
                days_month4, 
                days_month5, 
                days_month6, 
                days_month7, 
                days_month8, 
                days_month9, 
                days_month10, 
                days_month11)

# Perfect forecast
epsilon_list = map(.x = paste0("day", days), 
                   .f = function(x) get("epsilons", get(x, scenario2_perfect_cobyla))) %>% flatten_dbl()
map2(.x = 1:11, 
     .y = weekdays, 
     .f = plot_epsilons,
     epsilon_list = epsilon_list,
     min_quant = min_quant,
     max_quant = max_quant,
     perfect_forecast = TRUE)


# SARIMA forecast
epsilon_list = map(.x = paste0("day", days), 
                   .f = function(x) get("epsilons", get(x, scenario2_sarima))) %>% flatten_dbl()

map2(.x = 1:11, 
     .y = weekdays, 
     .f = plot_epsilons,
     epsilon_list = epsilon_list,
     min_quant = min_quant,
     max_quant = max_quant,
     perfect_forecast = FALSE)


# For the monthly appraoch
epsilon_list = map(.x = paste0("month", 1:11), 
                   .f = function(x) get("epsilons", get(x, scenario1_train))) %>% flatten_dbl()

map2(.x = 1:11,
     .y = weekdays,
     .f = plot_epsilons_monthly,
     epsilon_list = epsilon_list,
     min_quant = min_quant,
     max_quant = max_quant,
     x_data = x_test_opt)



# Creating the correct values and plots --------------------------------------------

# Firstly, we load the data
data = readRDS("RDS/bid_ask_data")

# We define new days vector for the find_equilibrium function
days_new = list(25:31, 22:28, 25:31, 24:30, 25:31, 24:30, 25:31, 25:31, 24:30, 25:31, 17:23)


# We want to use the "find_equilibrium" function to determine the new intercepts between S and D
# This is done in the following

# For the perfect forecast
epsilon_list = map(.x = paste0("day", days), 
                   .f = function(x) get("epsilons", get(x, scenario2_perfect_cobyla))) %>% flatten_dbl()

tic()
new_prices = map2(.x = 1:11,
                  .y = days_new,
                  .f = get_new_price_daily,
                  data = data,
                  epsilon_list = epsilon_list,
                  max_quant = max_quant,
                  min_quant = min_quant)
toc()

new_prices = new_prices %>% flatten_dbl()

# The unaffected prices
prices = map(.x = paste0("day", days),
             .f = function(x) get("prices", get(x, scenario2_perfect_cobyla))) %>% flatten_dbl()

# The target prices
target = map_dbl(.x = paste0("day", days),
                 .f = function(x) get("target", get(x, scenario2_perfect_cobyla)))

# We use the plot_weekly function to make the plots and get all the values
values = map(.x = 1:11,
             .f = plot_weekly,
             prices = prices,
             new_prices = new_prices,
             target = target,
             perfect_forecast = TRUE) %>% set_names(paste0("month", 1:11))


# For the SARIMA forecast
epsilon_list = map(.x = paste0("day", days), 
                   .f = function(x) get("epsilons", get(x, scenario2_sarima))) %>% flatten_dbl()
epsilon_list[4*168 + 116 - 1] = (epsilon_list[4*168 + 116 - 2] + epsilon_list[4*168 + 116]) / 2

tic()
new_prices = map2(.x = 1:11,
                  .y = days_new,
                  .f = get_new_price_daily,
                  data = data,
                  epsilon_list = epsilon_list,
                  max_quant = max_quant,
                  min_quant = min_quant) %>% flatten_dbl()
toc()

# The unaffected prices
prices = map(.x = paste0("day", days),
             .f = function(x) get("prices", get(x, scenario2_sarima))) %>% flatten_dbl()

# The target prices
target = map_dbl(.x = paste0("day", days),
                 .f = function(x) get("target", get(x, scenario2_sarima)))

# We use the plot_weekly function to make the plots and get all the values
values = map(.x = 1:11,
             .f = plot_weekly,
             prices = prices,
             new_prices = new_prices,
             target = target,
             perfect_forecast = FALSE) %>% set_names(paste0("month", 1:11))


# ---------------------------------------------------- The monthly approach ----------------------------------------------------

# ---------------------------------------------------- Training 
epsilon_list = map(.x = paste0("month", 1:11), 
                   .f = function(x) get("epsilons", get(x, scenario1_train))) %>% flatten_dbl()

prices = map(.x = paste0("month", 1:11), 
             .f = function(x) get("prices", get(x, scenario1_train))) %>% flatten_dbl()

target = map(.x = paste0("month", 1:11), 
             .f = function(x) get("target", get(x, scenario1_train))) %>% flatten_dbl()

tic()
new_prices = map(.x = 1:11,
                 .f = get_new_price_monthly,
                 data = data,
                 epsilon_list = epsilon_list,
                 max_quant = max_quant,
                 min_quant = min_quant,
                 x_data = x_train_with_day,
                 train = TRUE) %>% flatten_dbl()
toc()

values = map(.x = 1:11,
             .f = new_values_monthly,
             prices = y_train,
             new_prices = new_prices,
             target = target,
             train = TRUE)


# ---------------------------------------------------- Test


prices = map(.x = paste0("month", 1:11), 
             .f = function(x) get("prices", get(x, scenario1_test))) %>% flatten_dbl()

target = map(.x = paste0("month", 1:11), 
             .f = function(x) get("target", get(x, scenario1_test))) %>% flatten_dbl()

tic()
new_prices = map(.x = 1:11,
                 .f = get_new_price_monthly,
                 data = data,
                 epsilon_list = epsilon_list,
                 max_quant = max_quant,
                 min_quant = min_quant,
                 x_data = x_test_opt,
                 train = FALSE) %>% flatten_dbl()
toc()

values = map(.x = 1:11,
             .f = new_values_monthly,
             prices = y_test_opt,
             new_prices = new_prices,
             target = target,
             train = FALSE)






