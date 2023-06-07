# For loading and saving the data as an RDS object ------------------------

#file_names = dir(path = "Data_DAM", pattern = ".csv", full.names = TRUE)                                  # where you have your files
#bid_ask_data = do.call(rbind, lapply(file_names, read.csv))                                               # collect all data into a data frame
#bid_ask_data = select(bid_ask_data, -Curve)                                                               # remove column "Curve"
#bid_ask_data[bid_ask_data$Hour == "3B",] = bid_ask_data %>% filter(Hour == "3B") %>% mutate(Hour = 25)    # two Hour 3 at date "2022-10-20" due to standard time, the are marked with Hour "3" and "3B", we change "3B" to "25"
#bid_ask_data = filter(bid_ask_data, Quantity != 0)
#saveRDS(object = bid_ask_data, file = "RDS/bid_ask_data")                                                 


# data for a specific hour ------------------------------------------------

# get data for a specific hour (given month, day, hour and side (optional)).
get_daily_data = function(df, month, day, hour, side = c("Sell", "Buy")) {
  if (nchar(day) == 1 & nchar(month) == 1) {
    data = df %>% filter(Date == paste0("2022-0", month, "-0", day) &
                           Hour == hour & Side %in% side)
  } else if (nchar(day) == 2 & nchar(month) == 1) {
    data = df %>% filter(Date == paste0("2022-0", month, "-", day) &
                           Hour == hour & Side %in% side)
  } else if (nchar(day) == 1 & nchar(month) == 2) {
    data = df %>% filter(Date == paste0("2022-", month, "-0", day) &
                           Hour == hour & Side %in% side)
  } else if (nchar(day) == 2 & nchar(month) == 2) {
    data = df %>% filter(Date == paste0("2022-", month, "-", day) &
                           Hour == hour & Side %in% side)
  }
  return(data)
}


# input vector for a specific hour ----------------------------------------

# function creating a single input vector, given input date and hour. It
# consists of bid prices, bid quantities, sell prices and sell quantities
# in that order
get_input_vector = function(data, month, day, hour) {
  daily_data = get_daily_data(
    df = data,
    month = month,
    day = day,
    hour = hour
  )
  
  price_buy = filter(daily_data, Side == "Buy")$Price
  quan_buy = filter(daily_data, Side == "Buy")$Quantity
  price_sell = filter(daily_data, Side == "Sell")$Price
  quan_sell = filter(daily_data, Side == "Sell")$Quantity
  
  input_vector = c(price_buy, quan_buy, price_sell, quan_sell)
  
  return(input_vector)
  
}

# mutate input vectors such that aggregates quantities are included instead
# of just quantities (function beased on the output of the function get_input_vector)
aggregate_quantities = function(data, input_vectors, month_indexes, day_indexes, hour_indexes){ 
  agg_quan = pmap(
    .l = list(input = input_vectors, month = month_indexes, day = day_indexes, hour = hour_indexes),
    .f = function(data, input, month, day, hour) { 
      n = count_bid(data = data, month = month, day = day, hour = hour)
      
      bids = input[(n$n_bids + 1):(2 * n$n_bids)] %>%
        aggregate(list(input[1:n$n_bids]), sum) %>%
        arrange(desc(Group.1)) %>%
        mutate(x = cumsum(x)) %>%
        unlist() %>%
        as.numeric
      
      asks = input[(2 * n$n_bids + n$n_asks + 1):(2 * n$n_bids + 2 * n$n_asks)] %>%
        aggregate(list(input[(2 * n$n_bids + 1):(2 * n$n_bids + n$n_asks)]), sum) %>%
        mutate(x = cumsum(x)) %>%
        unlist() %>%
        as.numeric
      
      return(c(bids, asks))
    },
    data = data)
  
  return(input_vectors = agg_quan)
}


# output vector for a specific hour ---------------------------------------

# Find the point where the curves cross each other
find_equilibrium <- function(price_supply, quant_supply, price_demand, quant_demand) {
  
  # aggregate quantities by price for supply and demand bids
  supply <- aggregate(quant_supply, list(price_supply), sum)
  demand <- aggregate(quant_demand, list(price_demand), sum)
  
  # rename columns to "price" and "quantity"
  colnames(supply) <- c("price", "quantity")
  colnames(demand) <- c("price", "quantity")
  
  supply_price = map(.x = 1:length(supply$price), 
                     .f = function(x) rep(supply$price[x], 2)) %>% flatten_dbl()
  supply_price = supply_price[-length(supply_price)]
  supply_quantity = map(.x = 1:length(supply$quantity), 
                        .f = function(x) c(supply$quantity[x], 0)) %>% flatten_dbl()
  supply_quantity = supply_quantity[-1]
  
  demand_price = map(.x = 1:length(demand$price), 
                     .f = function(x) rep(demand$price[x], 2)) %>% flatten_dbl() %>% rev()
  demand_price = demand_price[-length(demand_price)]
  demand_quantity = map(.x = 1:length(demand$quantity), 
                        .f = function(x) c(rev(demand$quantity)[x], 0)) %>% flatten_dbl()
  demand_quantity = demand_quantity[-1]
  
  # set initial values for price and quantity
  pS = c(min(supply$price))
  pD = c(max(demand$price))
  qS = c(sum(supply$quantity[supply$price == pS[1]]))
  qD = c(sum(demand$quantity[demand$price == pD[1]]))
  
  # iterate until equilibrium is found or no more possible prices exist
  while ( tail(pS, 1) < tail(pD, 1) ) {
    if ( tail(qS, 1) < tail(qD,1) ) {
      pS = append(pS, supply_price[length(pS) + 1])
      qS = append(qS, tail(qS, 1) + supply_quantity[length(qS) + 1])
    } else {
      pD = append(pD, demand_price[length(pD) + 1])
      qD = append(qD, tail(qD, 1) + demand_quantity[length(qD) + 1])
    }
  }
  qS_extra = tail(qS, 1) + supply_quantity[length(qS) + 1]
  qD_extra = tail(qD, 1) + demand_quantity[length(qD) + 1]
  qS = tail(qS, 2)
  pS = tail(pS, 2)
  qD = tail(qD, 2)
  pD = tail(pD, 2)
  
  
  if (all(sapply(list(qS[2], qD[2], qD[1]), function(x) x == qS[2])) == TRUE) {
    return(list(price = pS[2], quantity = qS[2]))
  } else if (qS[2] == qD[1] & qD[2] > qS[2]) {
    return(list(price = pD[1], quantity = qD[1]))
  } else if (qS[2] == qD[2] & pS[2] == pD[2]) {
    return(list(price = pS[2], quantity = qS[2]))
  } else if (pS[2] == pD[2]) {
    return(list(price = pS[2], quantity = min(qD_extra, qS_extra) ))
  } else if (qS[2] > qD[2]) {
    return(list(price = pS[2], quantity = qD[2]))
  } else if (qS[2] < qD[2]) {
    return(list(price = pD[2], quantity = qS[2]))
  } else {return(c(NA,NA))}
}

# return the output vector consisting of MCP and MCQ in that order
get_output_vector = function(df, month, day, hour){ 
  data_sell = get_daily_data(df, month, day, hour, side = "Sell") 
  data_buy = get_daily_data(df, month, day, hour, side = "Buy") 
  
  output = find_equilibrium(price_supply = data_sell$Price,
                            quant_supply = data_sell$Quantity,
                            price_demand = data_buy$Price,
                            quant_demand = data_buy$Quantity)
  
  output = output %>% unlist() %>% as.vector()
  return(output)
}


# number of bids and asks -------------------------------------------------

# function counting the number of bids and asks for a given month, day and hour
count_bid = function(data, month, day, hour, distinct = FALSE) {
  daily_data = get_daily_data(
    df = data,
    month = month,
    day = day,
    hour = hour
  )
  if (distinct == FALSE) {
    n_bids = daily_data %>% filter(Side == "Buy") %>% nrow()
    n_asks = daily_data %>% filter(Side == "Sell") %>% nrow()
  } else {
    n_bids = daily_data %>% filter(Side == "Buy") %>% distinct(Price) %>% nrow()
    n_asks = daily_data %>% filter(Side == "Sell") %>% distinct(Price) %>% nrow()
  }
  
  return(list(n_bids = n_bids, n_asks = n_asks))
}


# number of hours and days in a specific month ----------------------------

# function computing the number of hours and days in a given month
count_hours_days_in_month = function(data, month) {
  month = sprintf('%0.2d', month)   # possibly insert leading zeros, e.g. 1 -> 01
  
  hours = map(
    .x = sprintf('%0.2d', 1:31),
    .f = function(month, x) {
      data[data$Date == paste0("2022-", month, "-", x), ] %>% distinct(Hour) %>% count()
    },
    month = month
  ) %>% unlist() %>% sum()
  
  days = hours / 24   # not integer: due to change from standard time to summer time and opposite
  
  return(list(days_in_month = days, hours_in_month = hours))
}


# number of days in a specific month --------------------------------------

# function computing the indexes for all hours and days in a given month (day indexes are repeated the number of Hours on that day)
get_hour_day_indexes = function(data, month) {
  month = sprintf('%0.2d', month)   # possibly insert leading zeros, e.g. 1 -> 01
  
  hour_index = map(   # making an index list of all hours in the chosen month
    .x = sprintf('%0.2d', 1:31),
    .f = function(x){
      if (data[data$Date == paste0("2022-", month, "-", x), ] %>% distinct(Date) %>% count() == 1) {   # checking whether the day is present in the data
        if (data[data$Date == paste0("2022-", month, "-", x), ] %>% distinct(Hour) %>% count() < 25) {   # runs if loop for all days where time is not changed from summer time to standard time
          data[data$Date == paste0("2022-", month, "-", x), ] %>% distinct(Hour) %>% unlist() %>% as.numeric() %>% sort()   # c(1,2,4,...,24) on days changing from standard time to summer time, c(1,...,24) other days
        } else {   # days changing from summer time to standard time (one more hour)
          c(1:3, 25, 4:24)   # 25 is the extra hour 3
        }
      }
    }
  ) %>% unlist()
  
  day_index = map(   # making an index list of all days in the chosen month (each day is repeated the number of hours on the given day)
    .x = sprintf('%0.2d', 1:31),
    .f = function(x){
      if (data[data$Date == paste0("2022-", month, "-", x), ] %>% distinct(Date) %>% count() == 1) {
        rep(x, data[data$Date == paste0("2022-", month, "-", x), ] %>% distinct(Hour) %>% count()) %>% as.numeric   # repeats the day index the number of hours on the day
      }
    }
  ) %>% unlist()
  
  return(list(hour_index_list = hour_index, day_index_list = day_index))
}


# training and test data --------------------------------------------------

# function separating the inputs into training and test set
# input_vectors -> "list", p -> "numeric"
get_training_test_data = function(input_vectors, output_vectors, p){   
  
  index =
    sample(
      x = 1:2,                        # 1 for training, 2 for test
      size = length(input_vectors),   # index vector of size equal to number of inputs in total 
      replace = T,                    # the indexes 1,2 are replaced after being choosen
      prob = c(p,1-p)                 # p is probability of index 1, 1-p probabibility of index 2
    )
  
  training_input = input_vectors[index == 1]     # creating training inputs
  test_input = input_vectors[index == 2]         # creating test inputs
  training_output = output_vectors[index == 1]   # creating training outputs
  test_output = output_vectors[index == 2]
  
  return(list(
    index_set = index,
    training_input = training_input,
    test_input = test_input,
    training_output = training_output,
    test_output = test_output
  ))
}


# normalizing / standardizing vector --------------------------------------

# normalize into the range 0 to 1, used in the stan_norm function
normalize = function(x, min_vec, max_vec){
  
  (x - min_vec)/(max_vec - min_vec) 
  
}

# stan_norm can standardize (mean 0, sd 1) and normalize (range -1 to 1)
# with default standardize. use type = "standard" to standardize and 
# type = "min.max" to normalize into the range -1 to 1

# feature_list is of class "list" with all elements of same length or "matrix",  
# with features in columns 

# method "across" (default) is standardizing/normalizing the first corresponding feature in all features
# this is the usual way of standardizing/normalizing and the method mentioned in Aggarwal p. 127 
# and Hagan et. al. p 22.5 to 22.6. method "individual" standardize/normalize each feature separately

# type = "standard" uses eq. (22.2) in Hagan et. al.
# type = "min.max" uses eq. (22.1) in Hagan et. al. 
stan_norm = function(feature_list, type = "standard", method = "across") {
  if (length(unique(sapply(feature_list, length))) != 1) {   # the elements of feature_list should be of the same length
    
    return(warning("The elements of feature_list differ in length"))
    
  }
  
  if (class(feature_list)[1] == "list") {
    features = feature_list %>% unlist() %>% matrix(ncol = length(feature_list))   # convert to matrix if class(feature_list) = "list"
    
  } else {
    
    features = feature_list
    
  }
  
  if (type == "standard" & method == "across") {   # rows: the j'th entry in each feature vector is standardized together
    
    mean_vec = rowMeans(features)                                                  # mean vector
    sd_vec = apply(X = features,                                                   # vector of standard deviations, uses denominator n-1
                   MARGIN = 1,
                   FUN = sd)                                                       
    stan_features = scale(t(features), center = mean_vec, scale = sd_vec) %>% t()  # (x - mean) / sd for all features x
    
    return(stan_features)
    
  }
  
  if (type == "standard" & method == "individual") {   # columns: each feature vector is standardized separately
    
    mean_vec = colMeans(features)                                                  # mean vector
    sd_vec = apply(X = features,                                                   # vector of standard deviations, uses denominator n-1
                   MARGIN = 2,
                   FUN = sd)                                                       
    stan_features = scale(features, center = mean_vec, scale = sd_vec)             # (x - mean) / sd for all features x
    
    return(stan_features)
    
  }
  
  if (type == "min.max" & method == "across") {   # scale feature values into the range -1 to 1
    
    min_vec = map_dbl(   # minimum value of each row
      .x = 1:nrow(features),
      .f = function(x) {
        features[x, ] %>% min()
      }
    )
    max_vec = map_dbl(   # maximum value of each row
      .x = 1:nrow(features),
      .f = function(x) {
        features[x, ] %>% max()
      }
    )
    
    norm_features = apply(   # normalizing into the range -1 to 1 using the normalize function defined above
      X = features,
      MARGIN = 2,
      FUN = normalize,
      min_vec = min_vec,
      max_vec = max_vec
    )
    
    return(norm_features)
    
  }
  
  if (type == "min.max" & method == "individual"){   # scale feature values into the range -1 to 1
    
    min_vec = map_dbl(   # minimum value of each row
      .x = 1:ncol(features),
      .f = function(x) {
        features[,x] %>% min()
      }
    )
    max_vec = map_dbl(   # maximum value of each row
      .x = 1:ncol(features),
      .f = function(x) {
        features[,x] %>% max()
      }
    )
    
    norm_features = apply(   # normalizing into the range -1 to 1 using the normalize function defined above
      X = features,
      MARGIN = 1,
      FUN = normalize,
      min_vec = min_vec,
      max_vec = max_vec
    ) %>% t()
    
    return(norm_features)
    
  }
}


# market clearing price (MCP) ---------------------------------------------

agg_quantity = function(data,i){
  
  distinct = data$Price == unique(data$Price)[i]
  subset = data[distinct,]
  
  if (nrow(subset) > 1){
    data[which(distinct),] = subset %>% mutate(Quantity = sum(subset$Quantity))
  }
  
  return(data)
  
}

remove_duplicated_prices = function(supply){ 
  
  for (i in 1:length(unique(supply$Price))) {
    supply = agg_quantity(supply, i)
  }
  
  supply = supply %>% distinct()
  
  return(supply)
  
}



get_output = function(data, month, day, hour){
  
  daily_data = get_daily_data(
    df = data,
    month = month,
    day = day,
    hour = hour
  )
  
  supply = daily_data %>% filter(Side == "Sell") %>% remove_duplicated_prices()
  demand = daily_data %>% filter(Side == "Buy") %>% remove_duplicated_prices()
  
  supply = supply %>% mutate(Agg_quantity = cumsum(Quantity))
  demand = demand %>% mutate(Agg_quantity = cumsum(Quantity))
  
  index_supply = 1
  index_demand = 1
  point_supply = supply[index_supply,] %>% select(Agg_quantity,Price)
  point_demand = demand[index_demand,] %>% select(Agg_quantity,Price)
  
  while (point_demand$Price > point_supply$Price) {
    
    while (point_supply$Agg_quantity >= point_demand$Agg_quantity & point_supply$Price < point_demand$Price) {
      index_demand = index_demand + 1
      point_demand = demand[index_demand,] %>% select(Agg_quantity,Price)
    }
    
    while (point_supply$Agg_quantity <= point_demand$Agg_quantity & point_supply$Price < point_demand$Price) {
      index_supply = index_supply + 1
      point_supply = supply[index_supply,] %>% select(Agg_quantity,Price)
    }
  } 
  return(list(point_demand = point_demand, point_supply = point_supply))
}



# Normalized inputs and outputs -------------------------------------------

# The following function will give a list of three elements. These elements are:
# a list containing the normalized output, a list containing four elements which
# the normalized inputs (prices and quants for bid and ask), and lastly the the 
# prices and quants for bid and ask (non-normalized)
get_prices_quantities = function(data, month, day, hour){
  date = paste0(
    "2022-",
    sprintf('%0.2d', month),
    "-",
    sprintf('%0.2d', day),
    " hour ",
    sprintf('%0.2d', hour)
  )
  
  n = count_bid(data = data, month = month, day = day, hour = hour, distinct = TRUE)
  n_bid = n$n_bids
  n_ask = n$n_asks
  
  price_bid = get(date, input_vectors)[1:n_bid]
  price_ask = get(date, input_vectors)[(2*n_bid + 1 ):(2*n_bid + n_ask)]
  quantity_bid = get(date, input_vectors)[(n_bid + 1):(2*n_bid)]
  quantity_ask = get(date, input_vectors)[(2*n_bid + n_ask + 1):(2*n_bid + 2*n_ask)]
  
  prices = price_bid %>% append(price_ask) %>% append(get(date, output_vectors)[1])
  quantities = quantity_bid %>% append(quantity_ask) 
  
  stan_prices = stan_norm(feature_list = prices %>% as.matrix(), 
                          type = "min.max", 
                          method = "individual")
  stan_quantities = stan_norm(feature_list = quantities %>% as.matrix(), 
                              type = "min.max", 
                              method = "individual")
  
  stan_prices_bid = stan_prices[1:n_bid]
  stan_prices_ask = stan_prices[(n_bid + 1):(n_bid + n_ask)]
  stan_output = stan_prices %>% as.vector() %>% last()
  stan_quantities_bid = stan_quantities[1:n_bid]
  stan_quantities_ask = stan_quantities[(n_bid + 1):(n_bid + n_ask)]
  
  return(list(
    stan_output = stan_output,
    stan_prices_bid = stan_prices_bid, stan_prices_ask = stan_prices_ask,
         stan_quantities_bid = stan_quantities_bid, stan_quantities_ask = stan_quantities_ask,
    price_bid = price_bid, price_ask = price_ask, quantity_bid = quantity_bid, quantity_ask = quantity_ask
  ))
}

# plot_supply_demand(df = data,month = 1,day = 1,hour = 1)
# # 50 and 30785.2
# 
# plot(x = supply$Agg_quantity, y = supply$Price, xlim = c(30500,31200), ylim = c(45,55), col = "red")
# points(x = demand$Agg_quantity, y = demand$Price)
# points(x = get_output(data,1,1,1)$point_supply[1], y = get_output(data,1,1,1)$point_supply[2], pch = "4", col = "red")
# points(x = get_output(data,1,1,1)$point_demand[1], y = get_output(data,1,1,1)$point_demand[2], pch = "4")
# 
# get_output(data,1,1,1)$point_demand
# 
# daily_data = get_daily_data(data,1,1,1)
# 
# supply = daily_data %>% filter(Side == "Sell") %>% remove_duplicated_prices()
# demand = daily_data %>% filter(Side == "Buy") %>% remove_duplicated_prices()
# 
# supply = supply %>% mutate(Agg_quantity = cumsum(Quantity))
# demand = demand %>% mutate(Agg_quantity = cumsum(Quantity))










