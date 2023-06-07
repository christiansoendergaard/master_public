# To get a vector of 1's in the places where quantity_ask is present
ones = function(x_input, max, i){ # x: a matrix with each column corresponding to the inputs of an hour. max: vector containing max_bid and max_ask. i: the column to find ones for 
  not_zero_index = which(x_input[(max$max_bid + 1 + 24 + 7):(max$max_bid + max$max_ask + 24 + 7), i] != 0)
  ones = c(rep(0, 24 + 7 + max$max_bid + min(not_zero_index) - 1),
           rep(1, length(not_zero_index)),
           rep(0, max$max_ask -  max(not_zero_index) + max$max_bid + max$max_ask))
  return(ones)
}

# The objective function
obj = function(eps, x_data, y_data, k, max, day, week){
  hours = map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[8:31, x] == 1))
  days =  map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[1:7, x] == 1))
  
  ones = map(
    .x = 1:ncol(x_data),
    .f = ones,
    max = max, x_input = x_data
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  hour_index = ((days[1] - 1) * 24 + 1):(7*24) %>% c(1:((days[1] - 1)*24))
  hour_index = rep(hour_index, 4)
  hour_index = hour_index[1:ncol(x_data)]
  
  if (day == TRUE & week == FALSE) {
    input = map2(
      .x = 1:ncol(x_data),
      .y = hour_index,
      .f = function(x, y) x_data[-(1:31), x] + ones[-(1:31), x] * eps[y] # -(1:31) since the model is without day/hour
    ) %>% 
      flatten_dbl() %>%
      matrix(ncol = ncol(x_data))
    
    means = map_dbl(.x = seq(1, ncol(x_data), 24), .f = function(x) mean(y_data[x:(x+23)]))
    k_vector = map(.x = 1:length(means), .f = function(x) rep(means[x], 24)) %>% flatten_dbl()
    
    diff = mean( (predict(model, t(input)) - k_vector)^2 )
    return(diff)
    
  } else if (week == TRUE & day == FALSE) {
    input = map2(
      .x = 1:ncol(x_data),
      .y = hour_index,
      .f = function(x, y) x_data[-(1:31), x] + ones[-(1:31), x] * eps[y] # -(1:31) since the model is without day/hour
    ) %>% 
      flatten_dbl() %>%
      matrix(ncol = ncol(x_data))
    
    means = map_dbl(.x = head(seq(1, ncol(x_data), 168), -1), .f = function(x) mean(y_data[x:(x+167)]))
    k_vector = map(.x = 1:length(means), .f = function(x) rep(means[x], 168)) %>% flatten_dbl()
    k_vector = c( k_vector, mean( y_data[(tail(seq(1, ncol(x_data), 168),  1)):ncol(x_data)] ) %>% rep(ncol(x_data) - (tail(seq(1, ncol(x_data), 168),  1)) + 1) )
    
    diff = mean( (predict(model, t(input)) - k_vector)^2 )
    return(diff)
    
  } else {
    input = map(
      .x = 1:ncol(x_data),
      .f = function(x) x_data[-(1:31), x] + ones[-(1:31), x] * eps[x] # -(1:31) since the model is without day/hour
    ) %>% 
      flatten_dbl() %>%
      matrix(ncol = ncol(x_data))
    
    diff = mean( (predict(model, t(input)) - k)^2 )
    return(diff)
  }
}

# The inequality constraints
inequal = function(eps, data, k, max, c, b){
  constr = rbind(sum(eps),
                 -sum(eps))
  return(constr)
}

# The training function for scenario 1. 
opt_monthly_train = function(month, x_data, y_data, max, budget){ browser()
  
  
  train_month_index = get_index_train_test(month)$train_index - 168 * (month - 1) # Right indices from x_train
  
  prices = y_data[train_month_index]
  
  opt = nloptr(x0 = rep(0,168), eval_f = obj, eval_g_ineq = NULL, lb = rep(-budget, 168), ub = rep(budget, 168), 
               opts = list("algorithm" = "NLOPT_LN_COBYLA", "print_level" = 1, "xtol_rel" = 1e-8, "maxeval" = 1),
               x_data = x_data[, train_month_index], y_data = prices, k = 1, max = max, day = TRUE, week = FALSE)
  epsilons = opt$solution
  objective_func = opt$objective
  
  train_x = x_data[, train_month_index]
  
  hours = map_dbl(.x = 1:ncol(train_x), .f = function(x) which(train_x[8:31, x] == 1))
  days =  map_dbl(.x = 1:ncol(train_x), .f = function(x) which(train_x[1:7, x] == 1))
  hour_index = ((days[1] - 1) * 24 + 1):(7*24) %>% c(1:((days[1] - 1) * 24))
  hour_index = rep(hour_index, 4)
  hour_index = hour_index[1:ncol(train_x)]
  
  
  ones = map(
    .x = 1:ncol(train_x),
    .f = ones,
    max = max, 
    x_input = train_x
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(train_x))
  
  input = map2(
    .x = 1:ncol(train_x),
    .y = hour_index,
    .f = function(x, y) train_x[-(1:31), x] + ones[-(1:31), x] * epsilons[y]
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(train_x)) 
  
  p = plot_opt_daily(prices = prices,
                     x_data = train_x,
                     epsilons = epsilons)
  
  colors = c("Unaffected price" = "black", "Target price" = "red", "Stabilized price" = "limegreen")
  
  plot = p$plot + 
    labs(x = "Hours",
         y = "Price", title = paste0("Training result for month ", month)) +
    scale_color_manual(values = colors)
  
  ggsave(filename = paste0("train_month", month,"_budget", budget, ".pdf"), 
         plot = plot, 
         width = 11, # Change these
         height = 5, # Change these
         path = "plots/optimization")
  
  new_prices = predict(model, t(input)) %>% as.vector()
  mae_new = mean(abs(new_prices - p$target))
  mse_new = mean((new_prices - p$target)^2)
  mae = mean(abs(prices - p$target))
  mse = mean((prices - p$target)^2)
  percentage = sum( abs(new_prices - p$target) <=  abs(prices - p$target) ) / length(prices)
  
  return(list(target = p$target,
              objective_func = objective_func,
              prices = prices,
              new_prices = new_prices,
              epsilons = epsilons,
              mae_new = mae_new,
              mse_new = mse_new,
              mae = mae,
              mse = mse,
              percentage = percentage))
}

# The testing function for scenario 1
opt_monthly_test = function(epsilons, month, x_data, y_data, max, budget){
  
  x_data = x_data[, ((month - 1)*168 + 1):((month - 1)*168 + 1 + 167) ]
  prices = y_data[((month - 1)*168 + 1):((month - 1)*168 + 1 + 167)]
  
  hours = map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[8:31, x] == 1))
  days =  map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[1:7, x] == 1))
  hour_index = ((days[1] - 1) * 24 + 1):(7*24) %>% c(1:((days[1] - 1)*24))
  hour_index = rep(hour_index, 4)
  hour_index = hour_index[1:ncol(x_data)]
  
  
  ones = map(
    .x = 1:ncol(x_data),
    .f = ones,
    max = max, x_input = x_data
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  input = map2(
    .x = 1:ncol(x_data),
    .y = hour_index,
    .f = function(x, y) x_data[-(1:31), x] + ones[-(1:31), x] * epsilons[y]
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data)) # Need to cut off first 31 rows for final function
  
  
  means = map_dbl(.x = seq(1, ncol(x_data), 24), .f = function(x) mean(prices[x:(x+23)]))
  k_vector = map(.x = 1:length(means), .f = function(x) rep(means[x], 24)) %>% flatten_dbl()
  
  new_prices = predict(model, t(input)) %>% as.vector()
  
  
  p = plot_opt_daily(prices = prices,
                     x_data = x_data,
                     epsilons = epsilons)
  
  colors <- c("Unaffected price" = "black", "Target price" = "red", "Stabilized price" = "limegreen")
  
  plot = p$plot + 
    labs(x = "Hours",
         y = "Price", title = paste0("Test result for month ", month)) +
    scale_color_manual(values = colors)
  
  ggsave(filename = paste0("test_month", month, "_budget", budget, ".pdf"), 
         plot = plot, 
         width = 11, # Change these
         height = 5, # Change these
         path = "plots/optimization")
  
  mae = mean(abs(prices - p$target))
  mse = mean((prices - p$target)^2)
  mae_new = mean(abs(new_prices - p$target))
  mse_new = mean((new_prices - p$target)^2)
  percentage = sum( abs(new_prices - p$target) <=  abs(prices - p$target) ) / length(prices)
  
  return(list(target = p$target, 
              prices = prices,
              new_prices = new_prices,
              mae_new = mae_new,
              mse_new = mse_new,
              mae = mae,
              mse = mse,
              percentage = percentage))
}

# The function for scenario 1
opt_daily = function(x_train_data, x_test_data, y_data, day, max, budget, perfect_forecast = FALSE){
  
  # Merging x_train and x_test_opt
  x_data = map(.x = 1:11, 
               .f = function(x) cbind(x_train_data[, get_index_train_test(x)$train_index - (x - 1 )*168],
                                      x_test_data[,((x - 1)*168 + 1):(x*168)])
  ) %>%
    flatten_dbl() %>%
    matrix(ncol = length(y_data))
  
  hour_diff = c(which(h == 1) %>% diff(), 24)
  day_next = hour_diff[1:(day - 1)] %>% sum() + 1 # Gets the index which corresponds to the first hour of "day"
  day_previous = hour_diff[1:(day - 2)] %>% sum() + 1 # Gets the index which corresponds to the first hour of "day - 1"
  
  h_next = hour_diff[day] - 1 # The number of hours in "day"
  h_previous = hour_diff[day - 1] - 1 # The number of hours in "day - 1"
  
  x_data_opt = x_data[, day_next:(day_next + h_next)] # Used in the plot to see how the found epsilons compare with the target and real price
  x_data_new = x_data[, day_previous:(day_previous + h_previous)] # Used together with the forecast to get new_input
  
  # The case when you "stand" on a day which changes season time. Below it makes sure that the number of hours in this day is 24 no matter if
  # it is actually 23 or 25 in reality. This is needed since the following day of these days always has 24 hours, and thus the forecast will 
  # be 24 hours. 
  if (h_previous == 22) { # From winter to summer time
    hour3 = hour_diff[1:(day - 2)] %>% sum() - 21 # taking hour 3 from the previous day
    x_data = cbind(x_data_new[,1:2], x_data[, hour3], x_data_new[,3:23])
  } else if (h_previous == 24) { # From summer to winter time
    x_data = x_data_new[, c(1:3, 5:25)] # Simply removing the "fake" hour 3
  } else if (h_next == 22) {# The case when you "stand" on the day before it changes season time. From winter to summer time
    x_data = x_data_new[, -3]
  } else if (h_next == 24) { # From summer to winter time
    x_data = cbind(x_data_new[, 1:3], x_data_new[, 3], x_data_new[, 4:24])
  } else x_data = x_data_new # If not on day which changes season time
  
  
 
  
  prices = y_data[1:(day_next - 1)] # consists of all the prices up to and including "day - 1"
  
  
  # Function which matches the price to the forecast
  if (perfect_forecast == TRUE) {
    
    # Exact prices
    exact_prices = y_data[day_next:(day_next + h_next)]
    
    # Using the network to solve for the new supply which will match the price of the "forecast"
    new_input = match_price(x_data = x_data, price_forecast = exact_prices, max = max)
    
    # Defining the price target as the mean of the "forecast"
    p_star = mean(exact_prices)
    
    # Solving for battery capacity using the new inputs found above
    opt = nloptr(x0 = rep(0, h_next + 1), eval_f = obj, eval_g_ineq = NULL, lb = rep(-budget, h_next + 1), ub = rep(budget, h_next + 1), 
                 opts = list("algorithm" = "NLOPT_LN_COBYLA", "print_level" = 1, "xtol_rel" = 1e-8, "maxeval" = 25000),
                 x_data = new_input, y_data = NULL, k = p_star, max = max, day = FALSE, week = FALSE)
    
    # Saving the solution
    epsilons = opt$solution
    objective_func = opt$objective
    
    # Plotting the solution. First, the plot_opt function is used
    p = plot_opt(prices = exact_prices,
                 x_data = x_data_opt,
                 day = FALSE,
                 week = FALSE,
                 k = p_star,
                 opt = opt) # gets epsilons from the solution
    
    # Defining colors for the different elements in the plot
    colors <- c("Unaffected price" = "black", "Target price" = "red", "Stabilized price" = "limegreen")
    
    # Adding labels and colors to the plot
    plot = p$plot + 
      labs(title = paste0(as.Date(day - 1, origin = "2022-01-01") %>% weekdays(), 
                          " ",
                          as.Date(day - 1, origin = "2022-01-01")), 
           subtitle = "With perfect forecast", x = "Hour") +
      scale_color_manual(values = colors)
    
    #  Saving the plot under name perfect_dailyx_budgety.pdf for day x with budget y
    ggsave(filename = paste0("perfect_daily", day, "_budget_COBYLA", budget, ".pdf"), 
           plot = plot, 
           width = 11, # Change these
           height = 5, # Change these
           path = "plots/optimization")
    
    
    
  } else { 
    # Fitting sarima model
    price_series = ts(prices, frequency = 24)
    sarima_fit = arima(x = price_series,
                       order = c(0, 1, 0), 
                       seasonal = list(order = c(5, 1, 1), period = 24))
    
    
    # In the case where you need to forecast the day which changes season. In this case you need to either forecast 23 or 25 hours
    
    
    # Making a one day-ahead forecast and plotting
    fore = forecast(sarima_fit, h = h_next + 1)
    plot_span = (hour_diff[1:(day - 6)] %>% sum() + 1):(hour_diff[1:(day)] %>% sum()) %>% length()
    forecast_plot = autoplot(fore, include = plot_span) + 
      theme_bw() + 
      labs(title = "One day-ahead forecast with 80% and 95% confidence bounds", 
           x = "Days after January 1st", 
           y = "Price") + geom_line(aes(y = y_data[ (hour_diff[1:(day - 6)] %>% sum() + 1):(hour_diff[1:(day)] %>% sum()) ],
                                        x = seq(day - 5, (day + 1 - 1/h_next), length.out = plot_span)))
    
    ggsave(filename = paste0("forecast", day, "_budget", budget, ".pdf"), 
           plot = forecast_plot, 
           width = 11, # Change these
           height = 5, # Change these
           path = "plots/optimization")
    
    # Defining the prediction
    price_forecast = fore$mean
    price_forecast = price_forecast %>% as.vector()
    
    # Using the network to solve for the new supply which will match the price of the forecast
    new_input = match_price(x_data = x_data, price_forecast = price_forecast, max = max)
    
    # Defining the target price
    p_star = mean(price_forecast)
    
    # Solving for battery capacity using the new inputs found above
    opt = nloptr(x0 = rep(0, h_next + 1), eval_f = obj, eval_g_ineq = NULL, lb = rep(-budget, h_next + 1), ub = rep(budget, h_next + 1), 
                 opts = list("algorithm" = "NLOPT_LN_COBYLA", "print_level" = 1, "xtol_rel" = 1e-8, "maxeval" = 25000),
                 x_data = new_input, y_data = NULL, k = p_star, max = max, day = FALSE, week = FALSE)
    
    # Saving the solution
    epsilons = opt$solution
    objective_func = opt$objective
    
    # Plotting the solution. First, the plot_opt function is used
    p = plot_opt(prices = y_data[day_next:(day_next + h_next)], # Same as exact_prices
                 x_data = x_data_opt,
                 day = FALSE,
                 week = FALSE,
                 k = p_star,
                 opt = opt) # gets epsilons from the solution
    
    # Defining colors for the different elements in the plot
    colors <- c("Unaffected price" = "black", "Target price" = "red", "Stabilized price" = "limegreen")
    
    # Adding labels and colors to the plot
    plot = p$plot + 
      labs(title = paste0(as.Date(day - 1, origin = "2022-01-01") %>% weekdays(), 
                          " ",
                          as.Date(day - 1, origin = "2022-01-01")), 
           subtitle = "Forecast from SARIMA",
           x = "Hour") +
      scale_color_manual(values = colors)
    
    # Saving the plot under name dailyx_budgety.pdf for day x with budget y
    ggsave(filename = paste0("daily", day, "_budget", budget, ".pdf"), 
           plot = plot, 
           width = 11, # Change these
           height = 5, # Change these
           path = "plots/optimization") 
  }
  
  # Extracting the stabilized prices from the plot function and redefining prices to be the price of day + 1
  new_prices = p$new_prices
  prices = y_data[day_next:(day_next + h_next)] # consists of all the prices up to and including "day"
  #prices = y_data[(day * 24 + 1):((day + 1) * 24)]
  
  # Creating the other outputs (apart from the plots which are saved automatically)
  mae_new = mean(abs(new_prices - p$target))
  mse_new = mean((new_prices - p$target)^2)
  mae = mean(abs(prices - p$target))
  mse = mean((prices - p$target)^2)
  times_new_below = sum( abs(new_prices - p$target) <=  abs(prices - p$target) )
  
  
  return(list(target = p$target, 
              objective_func = objective_func,
              prices = prices,
              new_prices = new_prices,
              epsilons = epsilons,
              mae_new = mae_new,
              mse_new = mse_new,
              mae = mae,
              mse = mse,
              times_new_below = times_new_below))
}

# The plotting function used in opt_monthly_train and opt_monthly_test
plot_opt_daily = function(prices, x_data, epsilons){
  
  hours = map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[8:31, x] == 1))
  days =  map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[1:7, x] == 1))
  hour_index = ((days[1] - 1) * 24 + 1):(7*24) %>% c(1:((days[1] - 1)*24))
  hour_index = rep(hour_index, 4)
  hour_index = hour_index[1:ncol(x_data)]
  
  ones = map(
    .x = 1:ncol(x_data),
    .f = ones,
    max = max, x_input = x_data
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  input = map2(
    .x = 1:ncol(x_data),
    .y = hour_index,
    .f = function(x, y) x_data[-(1:31), x] + ones[-(1:31), x] * epsilons[y]
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  
  means = map_dbl(.x = seq(1, ncol(x_data), 24), .f = function(x) mean(prices[x:(x+23)]))
  k_vector = map(.x = 1:length(means), .f = function(x) rep(means[x], 24)) %>% flatten_dbl()
  
  new_prices = predict(model, t(input))
  
  
  
  p = ggplot(mapping = aes(x = 1:ncol(x_data))) +
    geom_line(aes(y = prices, color = "Unaffected price")) +
    #geom_point(col = "gray40", size = 1, aes(y = prices)) +
    geom_line(aes(y = new_prices, color = "Stabilized price")) +
    #geom_point(col = "gray40", size = 1, aes(y = new_prices)) +
    geom_segment(aes(
      x = seq(1, ncol(x_data), 24),
      y = unique(k_vector),
      xend = seq(24, ncol(x_data), 24),
      yend = unique(k_vector), color = "Target price")) +
    theme_bw() + 
    theme(legend.position = c(.89, .87), # Remember to move the legend
          legend.background = element_blank(),
          legend.title = element_blank())
  
  
  return(list(plot = p, target = k_vector))
  
}

# Plotting function used in opt_daily
plot_opt = function(prices, x_data, day, week, k, opt){
  epsilons = opt$solution
  
  n_hours = length(epsilons)
  
  hours = map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[8:31, x] == 1))
  days =  map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[1:7, x] == 1))
  #hour_index = ((days[1] - 1) * n_hours + 1):(7 * n_hours) %>% c(1:((days[1] - 1) * n_hours))
  #hour_index = rep(hour_index, 4)
  #hour_index = hour_index[1:ncol(x_data)]
  hour_index = hours
  
  ones = map(
    .x = 1:ncol(x_data),
    .f = ones,
    max = max, x_input = x_data
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  input = map2(
    .x = 1:ncol(x_data),
    .y = hour_index,
    .f = function(x, y) x_data[, x] + ones[, x] * epsilons[y]
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  
  if (day == TRUE & week == FALSE) {
    #means = map_dbl(.x = seq(1, ncol(x_data), n_hours), .f = function(x) mean(prices[x:(x + n_hours - 1)]))
    #k_vector = map(.x = 1:length(means), .f = function(x) rep(means[x], n_hours)) %>% flatten_dbl()
    means = mean(prices)
    
    new_prices = predict(model, t(input))
    
    p = ggplot(mapping = aes(x = 1:ncol(x_data))) +
      geom_line(aes(y = prices, color = "Unaffected price")) +
      #geom_point(col = "gray40", size = 1, aes(y = prices)) +
      geom_line(aes(y = new_prices, color = "Stabilized price")) +
      #geom_point(col = "gray40", size = 1, aes(y = new_prices)) +
      geom_segment(aes(x = seq(1, ncol(x_data), n_hours), y = means, xend = seq(n_hours, ncol(x_data), n_hours), yend = means, color = "Target price")) +
      ylab("Price") +
      theme_bw()
    
    return(p)
    
  } else if (week == TRUE & day == FALSE) { # don't use
    means = map_dbl(.x = head(seq(1, ncol(x_data), 168), -1), .f = function(x) mean(prices[x:(x+167)]))
    k_vector = map(.x = 1:length(means), .f = function(x) rep(means[x], 168)) %>% flatten_dbl()
    k_vector = c( k_vector, mean( prices[(tail(seq(1, ncol(x_data), 168),  1)):ncol(x_data)] ) %>%
                    rep(ncol(x_data) - (tail(seq(1, ncol(x_data), 168),  1)) + 1) )
    
    new_prices = predict(model, t(input))
    
    p = ggplot(mapping = aes(x = 1:ncol(x_data))) +
      geom_line(col = "paleturquoise3", aes(y = prices)) +
      geom_point(col = "gray40", size = 1, aes(y = prices)) +
      geom_line(col = "blue4", aes(y = new_prices)) +
      geom_point(col = "gray40", size = 1, aes(y = new_prices)) +
      geom_segment(aes(x = seq(1, ncol(x_data), 168), y = unique(k_vector), xend = c(seq(168, ncol(x_data), 168), ncol(x_data)), yend = unique(k_vector), color = "red")) +
      ylab("Price") +
      theme_bw()
    
    return(p)
  }
  
  input = map(
    .x = 1:ncol(x_data),
    .f = function(x) x_data[-(1:31), x] + ones[-(1:31), x] * epsilons[x]
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  new_prices = predict(model, t(input))
  
  
  
  p = ggplot(mapping = aes(x = 1:ncol(x_data))) +
    geom_line(aes(y = prices, color = "Unaffected price")) +
    #geom_point(col = "gray40", size = 1, aes(y = prices)) +
    geom_line(aes(y = new_prices, color = "Stabilized price")) +
    #geom_point(col = "gray40", size = 1, aes(y = new_prices)) +
    geom_hline(aes(yintercept = k, color = "Target price")) +
    ylab("Price") +
    theme_bw() +
    theme(legend.position = c(.89, .87), # Remember to move the legend
          legend.background = element_blank(),
          legend.title = element_blank()) +
    scale_x_continuous(limits = c(1, n_hours), expand = c(0.01, 0.01),
                       name = "", breaks = seq(0, n_hours, 6)) 
  
  return(list(plot = p, target = k, new_prices = new_prices))
}

# Creating objective function to estimate epsilons which will match the current price with the day-ahead forecast
match_price_obj = function(eps, x_data, price_forecast, max){
  hours = map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[8:31, x] == 1))
  
  ones = map(
    .x = 1:ncol(x_data),
    .f = ones,
    max = max, x_input = x_data
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  input = map(
    .x = 1:ncol(x_data),
    .f = function(x, y) x_data[-(1:31), x] + ones[-(1:31), x] * eps[x]
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = ncol(x_data))
  
  diff = mean( (predict(model, t(input)) - price_forecast)^2 )
  return(diff)
}

# The following function will optimize to find the epsilons which will match the price with the forecast.
match_price = function(x_data, price_forecast, max){
  n_hours = length(price_forecast)
  opt = nloptr(x0 = rep(0, n_hours), eval_f = match_price_obj, lb = rep(-0.5, n_hours), ub = rep(0.5, n_hours), 
               opts = list("algorithm" = "NLOPT_LN_SBPLX", "print_level" = 1, "xtol_rel" = 1e-8, "maxeval" = 25000),
               x_data = x_data, price_forecast = price_forecast, max = max)
  
  epsilons = opt$solution
  
  ones = map(
    .x = 1:n_hours,
    .f = ones,
    max = max, x_input = x_data
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = n_hours)
  
  new_input = map(
    .x = 1:n_hours,
    .f = function(x) x_data[, x] + ones[, x] * epsilons[x]
  ) %>% 
    flatten_dbl() %>%
    matrix(ncol = n_hours)
  
  return(new_input)
}

# The following function is used to plot the weekly prices from the daily approach
plot_weekly = function(prices, new_prices, target, month, perfect_forecast = FALSE){
  target = target[((month - 1) * 7 + 1):(month * 7)]
  
  if (month == 1 | month == 2) {
    prices = prices[((month - 1) * 7 * 24 + 1):(month * 7 * 24)]
    new_prices = new_prices[((month - 1) * 7 * 24 + 1):(month * 7 * 24)]
  } else if (month == 3) {
    prices = prices[337:503]
    new_prices = new_prices[337:503]
  } else if (month %in% 4:9) {
    prices = prices[((month - 1) * 7 * 24):(month * 7 * 24 - 1)]
    new_prices = new_prices[((month - 1) * 7 * 24):(month * 7 * 24 - 1)]
  } else if (month == 10) {
    prices = prices[1512:1680]
    new_prices = new_prices[1512:1680]
  } else {
    prices = prices[1681:1848]
    new_prices = new_prices[1681:1848]
  }
  
  if (month == 3) {
    
    p = ggplot(mapping = aes(x = 1:167)) +
      geom_line(aes(y = prices, color = "Unaffected price")) +
      geom_line(aes(y = new_prices, color = "Stabilized price")) +
      geom_segment(aes(
        x = c(seq(1, 72, 24), seq(72, 167, 24)),
        y = target,
        xend = c(seq(24, 71, 24), seq(71, 167, 24)),
        yend = target, color = "Target price")) +
      theme_bw() + 
      theme(legend.position = c(.89, .87), # Remember to move the legend
            legend.background = element_blank(),
            legend.title = element_blank())
    
  } else if (month == 10) {
    p = ggplot(mapping = aes(x = 1:169)) +
      geom_line(aes(y = prices, color = "Unaffected price")) +
      geom_line(aes(y = new_prices, color = "Stabilized price")) +
      geom_segment(aes(
        x = c(seq(1, 144, 24), 146),
        y = target,
        xend = c(seq(24, 143, 24), 145, 169),
        yend = target, color = "Target price")) +
      theme_bw() + 
      theme(legend.position = c(.89, .87), # Remember to move the legend
            legend.background = element_blank(),
            legend.title = element_blank())
  } else {
    p = ggplot(mapping = aes(x = 1:168)) +
      geom_line(aes(y = prices, color = "Unaffected price")) +
      geom_line(aes(y = new_prices, color = "Stabilized price")) +
      geom_segment(aes(
        x = seq(1, 168, 24),
        y = target,
        xend = seq(24, 168, 24),
        yend = target, color = "Target price")) +
      theme_bw() + 
      theme(legend.position = c(.89, .87), # Remember to move the legend
            legend.background = element_blank(),
            legend.title = element_blank())
  }
  
  colors <- c("Unaffected price" = "black", "Target price" = "red", "Stabilized price" = "limegreen")
  
  if (perfect_forecast == FALSE) {
    # Adding labels and colors to the plot
    plot = p + 
      labs(title = paste0("Month ", month, ": Forecast from SARIMA"), 
           x = "Hour") +
      scale_color_manual(values = colors)
    
    ggsave(filename = paste0("daily_month", month, ".pdf"),
           plot = plot,
           path = "plots/optimization",
           width = 11,
           height = 5)
  } else if (perfect_forecast == TRUE) {
    # Adding labels and colors to the plot
    plot = p + 
      labs(title = paste0("Month ", month, ": Perfect forecast"), 
           x = "Hour") +
      scale_color_manual(values = colors)
    
    ggsave(filename = paste0("daily_month_perfect", month, ".pdf"),
           plot = plot,
           path = "plots/optimization",
           width = 11,
           height = 5)
  } else {
    stop("perfect_forecast needs to be specified to either TRUE or FALSE")
  } 
  
  
  if (month == 3) {
    target = map2(.x = 1:7,
                  .y = c(rep(24, 2), 23, rep(24, 4)),
                  .f = function(x, y) rep(target[x], y) 
    ) %>% flatten_dbl()
  } else if (month == 10) {
    target = map2(.x = 1:7,
                  .y = c(rep(24, 5), 25, 24),
                  .f = function(x, y) rep(target[x], y)
    ) %>% flatten_dbl()
  } else {
    target = map(.x = 1:7,
                 .f = function(x) rep(target[x], 24)
    ) %>% flatten_dbl()
  }
  
  mae_new = mean( abs(target - new_prices) )
  mae_old = mean( abs(target - prices) )
  percentage_below = sum( abs(new_prices - target) <=  abs(prices - target) ) / length(prices)
  
  return(list(mae_new = mae_new,
              mae_old = mae_old,
              percentage_below = percentage_below))
  
}

# Function to plot the strategy for the daily approach
plot_epsilons = function(month, epsilon_list, perfect_forecast, min_quant, max_quant, days){
  
  if (month == 1 | month == 2) {
    epsilons = epsilon_list[((month - 1) * 7 * 24 + 1):(month * 7 * 24)]
  } else if (month == 3) {
    epsilons = epsilon_list[337:503]
  } else if (month %in% 4:9) {
    epsilons = epsilon_list[((month - 1) * 7 * 24):(month * 7 * 24 - 1)]
  } else if (month == 10) {
    epsilons = epsilon_list[1512:1680]
  } else {
    epsilons = epsilon_list[1681:1848]
  }
  
  # Unnormalizing the epsilons
  epsilons = epsilons * (max_quant - min_quant)
  
  if (month == 3) {
    p = ggplot(mapping = aes(x = 1:167)) +
      geom_line(aes(y = epsilons), color = "black") +
      theme_bw()
  } else if (month == 10) {
    p = ggplot(mapping = aes(x = 1:169)) +
      geom_line(aes(y = epsilons), color = "black") +
      theme_bw() 
  } else {
    p = ggplot(mapping = aes(x = 1:168)) +
      geom_line(aes(y = epsilons), color = "black") +
      theme_bw()
  }
  
  if (perfect_forecast == FALSE) {
    # Adding labels and titles to the plot
    if (month == 3) {
      plot = p + 
        labs(title = paste0("Strategy for month ", month, ": SARIMA forecast"), 
             x = "Hour", y = "MWh") + 
        geom_vline(xintercept = c(24, 48, 71, 95, 119, 143), col = "maroon", linetype = "solid") +
        scale_x_continuous(limits = c(1, length(epsilons)), expand = c(0.005, 0.005),
                           name = "", breaks = seq(12, 156, 24), labels = days) +
        theme(axis.ticks.x = element_blank())
    } else if (month == 10) {
      plot = p + 
        labs(title = paste0("Strategy for month ", month, ": SARIMA forecast"), 
             x = "Hour", y = "MWh") + 
        geom_vline(xintercept = c(24, 48, 72, 96, 120, 145), col = "maroon", linetype = "solid") +
        scale_x_continuous(limits = c(1, length(epsilons)), expand = c(0.005, 0.005),
                           name = "", breaks = seq(12, 156, 24), labels = days) +
        theme(axis.ticks.x = element_blank())
    } else {
      plot = p + 
        labs(title = paste0("Strategy for month ", month, ": SARIMA forecast"), 
             x = "Hour", y = "MWh") + 
        geom_vline(xintercept = seq(24, 144, 24), col = "maroon", linetype = "solid") +
        scale_x_continuous(limits = c(1, length(epsilons)), expand = c(0.005, 0.005),
                           name = "", breaks = seq(12, 156, 24), labels = days) +
        theme(axis.ticks.x = element_blank())
    }
    ggsave(filename = paste0("epsilons_daily_month", month, ".pdf"),
           plot = plot,
           path = "plots/optimization",
           width = 11,
           height = 5)
    
  } else if (perfect_forecast == TRUE) {
    # Adding labels and colors to the plot
    if (month == 3) {
      plot = p + 
        labs(title = paste0("Strategy for month ", month, ": Perfect forecast"), 
             x = "Hour", y = "MWh") + 
        geom_vline(xintercept = c(24, 48, 71, 95, 119, 143), col = "maroon", linetype = "solid") +
        scale_x_continuous(limits = c(1, length(epsilons)), expand = c(0.005, 0.005),
                           name = "", breaks = seq(12, 156, 24), labels = days) +
        theme(axis.ticks.x = element_blank())
    } else if (month == 10) {
      plot = p + 
        labs(title = paste0("Strategy for month ", month, ": Perfect forecast"), 
             x = "Hour", y = "MWh") + 
        geom_vline(xintercept = c(24, 48, 72, 96, 120, 145), col = "maroon", linetype = "solid") +
        scale_x_continuous(limits = c(1, length(epsilons)), expand = c(0.005, 0.005),
                           name = "", breaks = seq(12, 156, 24), labels = days) +
        theme(axis.ticks.x = element_blank())
    } else {
      plot = p + 
        labs(title = paste0("Strategy for month ", month, ": Perfect forecast"), 
             x = "Hour", y = "MWh") + 
        geom_vline(xintercept = seq(24, 144, 24), col = "maroon", linetype = "solid") +
        scale_x_continuous(limits = c(1, length(epsilons)), expand = c(0.005, 0.005),
                           name = "", breaks = seq(12, 156, 24), labels = days) +
        theme(axis.ticks.x = element_blank())
    }
    
      
      ggsave(filename = paste0("epsilons_daily_month_perfect", month, ".pdf"),
             plot = plot,
             path = "plots/optimization",
             width = 11,
             height = 5)
  } else {
    stop("perfect_forecast needs to be specified to either TRUE or FALSE")
  } 
}

# Function to plot the strategy for the montlhy approach
plot_epsilons_monthly = function(month, epsilon_list, min_quant, max_quant, days, x_data){
  
  # This is needed to find out which day is the "first"
  x_data = x_data[, ((month - 1)*168 + 1):((month - 1)*168 + 1 + 167) ]
  days_number =  map_dbl(.x = 1:ncol(x_data), .f = function(x) which(x_data[1:7, x] == 1))
  
  if (days_number[1] == 1) {
    hour_index = ((days_number[1] - 1) * 24 + 1):(7*24)
  } else {
    hour_index = ((days_number[1] - 1) * 24 + 1):(7*24) %>% c(1:((days_number[1] - 1)*24))
  }
  
  
  epsilons = epsilon_list[((month - 1) * 168 + 1):(month * 168)]
  
  # Unnormalizing the epsilons
  epsilons = epsilons * (max_quant - min_quant)
  
  # Getting the right hours in the right spots
  epsilons = map_dbl(.x = hour_index, 
                 .f = function(x) epsilons[x])
  
  p = ggplot(mapping = aes(x = 1:168)) +
    geom_line(aes(y = epsilons), color = "black") +
    theme_bw()
  
  plot = p + 
    labs(title = paste0("Strategy for month ", month, ": Monthly approach"), 
         x = "Hour", y = "MWh") + 
    geom_vline(xintercept = seq(24, 144, 24), col = "maroon", linetype = "solid") +
    scale_x_continuous(limits = c(1, 168), expand = c(0.005, 0.005),
                       name = "", breaks = seq(12, 156, 24), labels = days) +
    theme(axis.ticks.x = element_blank()) 
  
  ggsave(filename = paste0("epsilons_monthly", month, ".pdf"),
         plot = plot,
         path = "plots/optimization",
         width = 11,
         height = 5)
}

# Function to get the correct new prices by using the found strategy directly on the S and D curves
get_new_price_daily = function(data, month, days, epsilon_list, max_quant, min_quant){
  
  
  if (month == 1 | month == 2) {
    epsilons = epsilon_list[((month - 1) * 7 * 24 + 1):(month * 7 * 24)]
  } else if (month == 3) {
    epsilons = epsilon_list[337:503]
  } else if (month %in% 4:9) {
    epsilons = epsilon_list[((month - 1) * 7 * 24):(month * 7 * 24 - 1)]
  } else if (month == 10) {
    epsilons = epsilon_list[1512:1680]
  } else {
    epsilons = epsilon_list[1681:1848]
  }
  
  epsilons = epsilons * (max_quant - min_quant)
  
  
  if (month == 3) {
    days = c(rep(days[1:2], 24), rep(days[3], 23), rep(days[4:7], 24)) %>% sort()
    hours = c(rep(1:24, 2), c(1:2, 4:24), rep(1:24, 4))
    
    # List of data frames for side Sell
    data_sell = map2(.x = days, 
                     .y = hours, 
                     .f = get_daily_data, 
                     month = month, 
                     side = "Sell",
                     df = data)
    
    # Adding the found epsilon value to the observation with the lowest price
    data_sell = map(.x = 1:length(data_sell), 
                    .f = function(x){ 
                      data_sell[[x]][1, "Quantity"] = data_sell[[x]][1, "Quantity"] + epsilons[x]
                      return(data_sell[[x]])
                    }
    )
    
    # List of data frames for side Buy
    data_buy = map2(.x = days, 
                    .y = hours, 
                    .f = get_daily_data, 
                    month = month, 
                    side = "Buy",
                    df = data)
    
  } else if (month == 10) {
    days = c(rep(days[1:5], 24), rep(days[6], 25), rep(days[7], 24)) %>% sort()
    hours = c(rep(1:24, 5), c(1:3, 25, 4:24), 1:24)
    
    # List of data frames for side Sell
    data_sell = map2(.x = days, 
                     .y = hours, 
                     .f = get_daily_data, 
                     month = month, 
                     side = "Sell",
                     df = data)
    
    # Adding the found epsilon value to the observation with the lowest price
    data_sell = map(.x = 1:length(data_sell), 
                    .f = function(x){ 
                      data_sell[[x]][1, "Quantity"] = data_sell[[x]][1, "Quantity"] + epsilons[x]
                      return(data_sell[[x]])
                    }
    )
    
    # List of data frames for side Buy
    data_buy = map2(.x = days, 
                    .y = hours, 
                    .f = get_daily_data, 
                    month = month, 
                    side = "Buy",
                    df = data)
  } else {
    days = rep(days, 24) %>% sort()
    hours = rep(1:24, 7)
    
    # List of data frames for side Sell
    data_sell = map2(.x = days, 
                     .y = hours, 
                     .f = get_daily_data, 
                     month = month, 
                     side = "Sell",
                     df = data)
    
    # Adding the found epsilon value to the observation with the lowest price
    data_sell = map(.x = 1:length(data_sell), 
                    .f = function(x){ 
                      data_sell[[x]][1, "Quantity"] = data_sell[[x]][1, "Quantity"] + epsilons[x]
                      return(data_sell[[x]])
                    }
    )
    
    # List of data frames for side Buy
    data_buy = map2(.x = days, 
                    .y = hours, 
                    .f = get_daily_data, 
                    month = month, 
                    side = "Buy",
                    df = data)
    
  }
  
  new_prices = map_dbl(.x = 1:length(data_sell), 
                       .f = function(x) {
                         find_equilibrium(price_supply = data_sell[[x]]$Price,
                                          quant_supply = data_sell[[x]]$Quantity,
                                          price_demand = data_buy[[x]]$Price,
                                          quant_demand = data_buy[[x]]$Quantity)$price
                       }
                      )
  return(new_prices)
}

# Function to get the correct new prices by using a found strategy directly on the S and D curves
# In the monthly approach. Applies both for training and test
get_new_price_monthly = function(month, epsilon_list, min_quant, max_quant, x_data, train, data){
  
  # Choosing the correct epsilons for the month considered
  epsilons = epsilon_list[((month - 1) * 168 + 1):(month * 168)]
  
  # Unnormalizing the epsilons
  epsilons = epsilons * (max_quant - min_quant)
  
  if (train == TRUE) {
    
    # Defining "new_days" to be the such that we can get the correct hour index in the epsilon vector 
    train_month_index = get_index_train_test(month)$train_index - 168 * (month - 1)
    train_x = x_data[, train_month_index]
    weekdays =  map_dbl(.x = 1:ncol(train_x), .f = function(x) which(train_x[1:7, x] == 1))
    
    # The hour indices used in the map2 function to get epsilon added to the correct hours.
    if (weekdays[1] == 1) {
      hour_index = ((weekdays[1] - 1) * 24 + 1):(7 * 24)
      hour_index = rep(hour_index, 4)
      hour_index = hour_index[1:ncol(train_x)]
    } else {
      hour_index = ((weekdays[1] - 1) * 24 + 1):(7 * 24) %>% c(1:((weekdays[1] - 1) * 24))
      hour_index = rep(hour_index, 4)
      hour_index = hour_index[1:ncol(train_x)]
    }

    
    if (month %in% 4:10) {
      if (month %in% c(5, 7, 10)) {
        days = rep(1:24, 24) %>% sort()
        hours = rep(1:24, 24)
        
        days = c(30, head(days, -1))
        hours = c(24, head(hours, -1))
      } else if (month == 8) {
        days = rep(1:24, 24) %>% sort()
        hours = rep(1:24, 24)
        
        days = c(31, head(days, -1))
        hours = c(24, head(hours, -1))
      } else if (month %in% c(4, 6, 9)) {
        days = rep(1:23, 24) %>% sort()
        hours = rep(1:24, 23)
        
        days = c(31, head(days, -1))
        hours = c(24, head(hours, -1))
      }
        
    } else if (month %in% c(1,3)) {
        days = rep(1:24, 24) %>% sort()
        hours = rep(1:24, 24)
      } else if (month == 2) {
        days = rep(1:21, 24) %>% sort()
        hours = rep(1:24, 21)
      } else if (month == 11) {
        days = c(rep(1, 24), rep(3:13, 24), rep(15:16, 24)) %>% sort()
        hours = rep(1:24, 14)
      }
  } else {
    # Defining "new_days" to be the such that we can get the correct hour index in the epsilon vector 
    test_index = ((month - 1) * 168 + 1):(month  * 168)
    test_x = x_data[, test_index]
    weekdays =  map_dbl(.x = 1:ncol(test_x), .f = function(x) which(test_x[1:7, x] == 1))
    
    # The hour indices used in the map2 function to get epsilon added to the correct hours.
    if (weekdays[1] == 1) {
      hour_index = ((weekdays[1] - 1) * 24 + 1):(7 * 24)
      hour_index = rep(hour_index, 4)
      hour_index = hour_index[1:ncol(test_x)]
    } else {
      hour_index = ((weekdays[1] - 1) * 24 + 1):(7 * 24) %>% c(1:((weekdays[1] - 1) * 24))
      hour_index = rep(hour_index, 4)
      hour_index = hour_index[1:ncol(test_x)]
    }
    
    if (month %in% 4:10) {
      if (month %in% c(5, 7, 10)) {
        days = rep(25:31, 24) %>% sort()
        hours = rep(1:24, 7)
        
        days = c(30, head(days, -1))
        hours = c(24, head(hours, -1))
      } else if (month == 8) {
        days = rep(25:31, 24) %>% sort()
        hours = rep(1:24, 7)
        
        days = c(31, head(days, -1))
        hours = c(24, head(hours, -1))
      } else if (month %in% c(4, 6, 9)) {
        days = rep(24:30, 24) %>% sort()
        hours = rep(1:24, 7)
        
        days = c(31, head(days, -1))
        hours = c(24, head(hours, -1))
      }
      
    } else if (month == 1) {
      days = rep(25:31, 24) %>% sort()
      hours = rep(1:24, 7)
    } else if (month == 2) {
      days = rep(22:28, 24) %>% sort()
      hours = rep(1:24, 7)
    } else if (month == 11) {
      days = rep(17:23, 24) %>% sort()
      hours = rep(1:24, 7)
    } else if (month == 3) {
      days = rep(25:31, 24) %>% sort()
      hours = rep(1:24, 7)
      
      hours[51] = 2
    }
  }
  
  
  if (days[1] == 31 | days[1] == 30) {
    # List of data frames for side Sell
    data_sell = pmap(.l = list(c(month - 1, rep(month, length(days[-1]))), days, hours),
                     .f = get_daily_data, 
                     side = "Sell",
                     df = data)
    
    # List of data frames for side Buy
    data_buy = pmap(.l = list(c(month - 1, rep(month, length(days[-1]))), days, hours), 
                    .f = get_daily_data, 
                    side = "Buy",
                    df = data)
  } else {
    # List of data frames for side Sell
    data_sell = map2(.x = days, 
                     .y = hours, 
                     .f = get_daily_data, 
                     month = month, 
                     side = "Sell",
                     df = data)
    
    # List of data frames for side Buy
    data_buy = map2(.x = days, 
                    .y = hours, 
                    .f = get_daily_data, 
                    month = month, 
                    side = "Buy",
                    df = data)
  }
  
  
  
  # Adding the found epsilon value to the observation with the lowest price
  data_sell = map2(.x = 1:length(data_sell), 
                   .y = hour_index,
                   .f = function(x, y){ 
                     data_sell[[x]][1, "Quantity"] = data_sell[[x]][1, "Quantity"] + epsilons[y]
                     return(data_sell[[x]])
                   }
                  )
  
  # Determining new prices from the new supply curve
  new_prices = map_dbl(.x = 1:length(data_sell), 
                       .f = function(x) {
                         find_equilibrium(price_supply = data_sell[[x]]$Price,
                                          quant_supply = data_sell[[x]]$Quantity,
                                          price_demand = data_buy[[x]]$Price,
                                          quant_demand = data_buy[[x]]$Quantity)$price
                       }
  )
  
  return(new_prices)
  
}

# Get new values and plots for the monthly approach
new_values_monthly = function(prices, new_prices, target, month, train){
  
  if (train == TRUE) {

    train_month_index = get_index_train_test(month)$train_index - 168 * (month - 1) # Right indices from x_train 
   
    prices = prices[train_month_index]
    new_prices = new_prices[train_month_index]
    target = target[train_month_index]
  } else {
    
    prices = prices[((month - 1) * 168 + 1):(month * 168)]
    new_prices = new_prices[((month - 1) * 168 + 1):(month * 168)]
    target = target[((month - 1) * 168 + 1):(month * 168)]
  }
  
  if (month %in% c(4:10)) {
    target = target[-1]
    target = c(target, tail(target, 1))
  } else target = target
  
  # Defining the colors
  colors = c("Unaffected price" = "black", "Target price" = "red", "Stabilized price" = "limegreen")
  
  # Creating the ggplot
  p = ggplot(mapping = aes(x = 1:length(prices))) +
    geom_line(aes(y = prices, color = "Unaffected price")) +
    geom_line(aes(y = new_prices, color = "Stabilized price")) +
    geom_segment(aes(
      x = seq(1, length(prices), 24),
      y = unique(target),
      xend = seq(24, length(prices), 24),
      yend = unique(target), color = "Target price")) +
    theme_bw() + 
    theme(legend.position = c(.89, .87), # Remember to move the legend
          legend.background = element_blank(),
          legend.title = element_blank()) +
    labs(x = "Hours",
         y = "Price", title = paste0("Test result for month ", month)) +
    scale_color_manual(values = colors)
  
  ggsave(filename = paste0("test_month", month, ".pdf"), 
         plot = p, 
         width = 11, 
         height = 5, 
         path = "plots/optimization")
  
  
  mae_new = mean(abs(new_prices - target))
  mae_old = mean(abs(prices - target))
  percentage = sum( abs(new_prices - target) <=  abs(prices - target) ) / length(prices)
  
  return(list(mae_new = mae_new,
              mae_old = mae_old,
              percentage = percentage))
}

