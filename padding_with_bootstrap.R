# WHAT DO WE DO: 
# - use aggregated quantities (not quantities)
# - construct each input in the following way: $0,...,0, bid_P_1, bid_P_2,..., bid_P_n, 0,...,0, 0,...,0, bid_Q_1,..., bid_Q_n, 0,..., 0, 0,..., 0, bid_P_1, ask_P_2,..., ask_P_m, 0,..., 0, 0,..., 0, ask_Q_1,..., ask_Q_m, 0,..., 0$ 
# - the number of entries in the neural network allocated for buy bid prices is the maximum number of buy bids across all hours (the same allocation is performed for buy bid quantities, supply bid price and supply bid quantities)
# - if new data has a larger dimension (more asks or bids than the maximum i training data), we remove independently until we reach the same dimension as the training data
# - "individual" standardization/normalization (all prices together (both bids and asks) and all aggregated quantities together (include both input and output in this standardization/normalization))
# - do it with both prices and agg quantities, only prices and only agg quantities 

# packages, sourcing functions and load data ------------------------------

# packages
library(tidyverse)
library(R.utils)
library(tictoc)
library(tensorflow)
use_python("/srv/scratch/csan18/r-miniconda/bin/python")
library(keras)

# load data
data = readRDS(file = "RDS/data_input_output")
output_w_quant = readRDS(file = "RDS/output_vectors")
m = readRDS(file = "RDS/month_indexes") 
d = readRDS(file = "RDS/day_indexes")
h = readRDS(file = "RDS/hour_indexes")

# prepare data ------------------------------------------------------------

# function computing the maximum number of bids and asks, respectively, in an hour
get_max_bid_ask = function(data){
  quantity_bid = map(.x = data,
                     .f = `[`,
                     c("quantity_bid")) 
  quantity_ask = map(.x = data,
                     .f = `[`,
                     c("quantity_ask"))
  
  n_bid = quantity_bid %>%
    map_dbl(
      .f = function(x) {
        x %>% unlist() %>% length()
      }
    )
  n_ask = quantity_ask %>%
    map_dbl(
      .f = function(x) {
        x %>% unlist() %>% length()
      }
    )
  
  return(list(max_bid = max(n_bid), max_ask = max(n_ask)))
}

# c(maximum number of bids, maximum number of asks)
max = get_max_bid_ask(data)

get_zeros_input_boot = function(data, month, day, hour, max_bid, max_ask, price.quantity = "Quantity_and_price"){ 
  date = paste0(
    "2022-",
    sprintf('%0.2d', month),
    "-",
    sprintf('%0.2d', day),
    " hour ",
    sprintf('%0.2d', hour)
  )
  
  q_b = get("quantity_bid" , get(date, data))
  p_b = get("price_bid" , get(date, data))
  index_b = sample(x = 1:length(q_b), size = length(q_b), replace = T)
  q_b = q_b[index_b] %>% sort()
  p_b = p_b[index_b] %>% sort(decreasing = T)
  
  q_a = get("quantity_ask" , get(date, data))
  p_a = get("price_ask" , get(date, data))
  index_a = sample(x = 1:length(q_b), size = length(q_b), replace = T)
  q_a = q_a[index_a] %>% sort()
  p_a = p_a[index_a] %>% sort()
  
  quantity_bid = c(rep(0, ceiling((max_bid - length(q_b))/2)),
                   q_b,
                   rep(0, floor((max_bid - length(q_b))/2)))
  quantity_ask = c(rep(0, ceiling((max_ask - length(q_a))/2)),
                   q_a,
                   rep(0, floor((max_ask - length(q_a))/2)))
  price_bid = c(rep(0, ceiling((max_bid - length(p_b))/2)),
                p_b,
                rep(0, floor((max_bid - length(p_b))/2)))
  price_ask = c(rep(0, ceiling((max_ask - length(p_a))/2)),
                p_a,
                rep(0, floor((max_ask - length(p_a))/2)))
  
  if (price.quantity == "Quantity") {
    return(c(quantity_bid, quantity_ask))
  } 
  
  if (price.quantity == "Quantity_and_price"){
    return(c(quantity_bid, quantity_ask, price_bid, price_ask))
  }
}

# function an input vector, given a specific day (input vector using padding)
get_zeros_input = function(data, month, day, hour, max_bid, max_ask, price.quantity = "Quantity_and_price"){ 
  date = paste0(
    "2022-",
    sprintf('%0.2d', month),
    "-",
    sprintf('%0.2d', day),
    " hour ",
    sprintf('%0.2d', hour)
  )
  
  q_b = get("quantity_bid" , get(date, data))
  p_b = get("price_bid" , get(date, data))
  q_a = get("quantity_ask" , get(date, data))
  p_a = get("price_ask" , get(date, data))
  
  quantity_bid = c(rep(0, ceiling((max_bid - length(q_b))/2)),
                   q_b,
                   rep(0, floor((max_bid - length(q_b))/2)))
  quantity_ask = c(rep(0, ceiling((max_ask - length(q_a))/2)),
                   q_a,
                   rep(0, floor((max_ask - length(q_a))/2)))
  price_bid = c(rep(0, ceiling((max_bid - length(p_b))/2)),
                p_b,
                rep(0, floor((max_bid - length(p_b))/2)))
  price_ask = c(rep(0, ceiling((max_ask - length(p_a))/2)),
                p_a,
                rep(0, floor((max_ask - length(p_a))/2)))
  
  if (price.quantity == "Quantity") {
    return(c(quantity_bid, quantity_ask))
  } 
  
  if (price.quantity == "Quantity_and_price"){
    return(c(quantity_bid, quantity_ask, price_bid, price_ask))
  }
}

# extract indexes for training and validation/test data for one month (obtained by extracting the last seven days from the month)
get_index_train_test = function(month){ 
  n_days = (length(m[m == month]) / 24) %>% round()
  
  if (month %in% 2:11){
    start_index = length(m[m %in% 1:(month - 1)]) 
  } else {
    start_index = 0
  }
  
  train_index = 1:(which(h[m == month] == 1)[n_days - 6] - 1) + start_index
  test_index = which(h[m == month] == 1)[n_days - 6]:length(m[m == month]) + start_index
  
  return(list(train_index = train_index, test_index = test_index))
}


# inputs using padding ----------------------------------------------------

# input without hour and day
input = pmap(
  .l = list(month = m, day = d, hour = h),
  .f = get_zeros_input,
  data = data,
  max_bid = max$max_bid,
  max_ask = max$max_ask,
  price.quantity = "Quantity_and_price"
) %>%
  flatten_dbl() %>%
  matrix(ncol = length(data))

# input with only hour
input_with_hour = map2(
  .x = h,
  .y = 1:7800,
  .f = function(x,y) {
    if (x %in% 1:24 == TRUE) {
      c(rep(0, x-1), 1, rep(0, 24-x), input[, y])
    } else {
      c(rep(0, 2), 1, rep(0, 21), input[, y])
    }
    
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = length(data))

# input with hour and day (7 days)
input_with_day = map2(
  .x = map2(
    .x = c(6:7, rep(1:7, 43), 1:2, 4:7, 1:7, 2:7, 1:3),
    .y = c(diff(which(h == 1)),24),
    .f = function(x,y) {rep(x,y)}) %>% unlist(),
  .y = 1:7800,
  .f = function(x,y) {c(rep(0,x-1), 1, rep(0,7-x), input_with_hour[,y])}
) %>%
  flatten_dbl() %>%
  matrix(ncol = 7800)

# all targets/outputs -----------------------------------------------------

output = map_dbl(.x = 1:7800,
                 .f = function(x){output_w_quant[[x]][1]})


# indexes used for training and validation/test ---------------------------

train_index = map(
  .x = 1:11,
  .f = function(x) {
    get_index_train_test(x)$train_index
  }
) %>%
  unlist()

test_index = map(
  .x = 1:11,
  .f = function(x) {
    get_index_train_test(x)$test_index
  }
) %>%
  unlist()

# # index used for training and test
# set.seed(1234)
# index = sample(3,
#                size = 7800,
#                replace = T,
#                prob = c(0.9, 0.05, 0.05)) # 80% train, 10% validation, 10% test

# random sampling to create validation and test set
set.seed(1234)
index = sample(2,
               size = length(test_index),
               replace = T, 
               prob = c(0.5,0.5))   # 50% for validation and 50% for test of the last 7 days in each month


# -------------------------------------------------------------------------

x_train = pmap(
  .l = list(month = rep(m[train_index],10), 
            day = rep(d[train_index],10), 
            hour = rep(h[train_index],10)),
  .f = get_zeros_input_boot,
  data = data,
  max_bid = max$max_bid,
  max_ask = max$max_ask,
  price.quantity = "Quantity_and_price"
) %>%
  flatten_dbl() %>%
  matrix(ncol = 10*length(train_index))

x_val = input[, test_index[index == 1]]

x_test = input[, test_index[index == 2]]

# x_val = pmap(
#   .l = list(month = rep(m[test_index[index == 1]],10), 
#             day = rep(d[test_index[index == 1]],10), 
#             hour = rep(h[test_index[index == 1]],10)),
#   .f = get_zeros_input,
#   data = data,
#   max_bid = max$max_bid,
#   max_ask = max$max_ask,
#   price.quantity = "Quantity_and_price"
# ) %>%
#   flatten_dbl() %>%
#   matrix(ncol = 10*length(test_index[index == 1]))
# 
# x_test = pmap(
#   .l = list(month = rep(m[test_index[index == 2]],10), 
#             day = rep(d[test_index[index == 2]],10), 
#             hour = rep(h[test_index[index == 2]],10)),
#   .f = get_zeros_input,
#   data = data,
#   max_bid = max$max_bid,
#   max_ask = max$max_ask,
#   price.quantity = "Quantity_and_price"
# ) %>%
#   flatten_dbl() %>%
#   matrix(ncol = 10*length(test_index[index == 2]))

# -------------------------------------------------------------------------

x_train_with_hour = map2(
  .x = rep(h[train_index],10),
  .y = 1:ncol(x_train),
  .f = function(x,y) {
    if (x %in% 1:24 == TRUE) {
      c(rep(0, x-1), 1, rep(0, 24-x), x_train[, y])
    } else {
      c(rep(0, 2), 1, rep(0, 21), x_train[, y])
    }
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_train))

# x_val_with_hour = map2(
#   .x = rep(h[test_index[index == 1]],10),
#   .y = 1:ncol(x_val),
#   .f = function(x,y) {
#     if (x %in% 1:24 == TRUE) {
#       c(rep(0, x-1), 1, rep(0, 24-x), x_val[, y])
#     } else {
#       c(rep(0, 2), 1, rep(0, 21), x_val[, y])
#     }
#   }
# ) %>%
#   flatten_dbl() %>%
#   matrix(ncol = ncol(x_val))
# 
# x_test_with_hour = map2(
#   .x = rep(h[test_index[index == 2]],10),
#   .y = 1:ncol(x_test),
#   .f = function(x,y) {
#     if (x %in% 1:24 == TRUE) {
#       c(rep(0, x-1), 1, rep(0, 24-x), x_test[, y])
#     } else {
#       c(rep(0, 2), 1, rep(0, 21), x_test[, y])
#     }
#   }
# ) %>%
#   flatten_dbl() %>%
#   matrix(ncol = ncol(x_test))


# -------------------------------------------------------------------------

# day of the week indicated by one-hot encodding
day_index = map2(
  .x = c(6:7, rep(1:7, 43), 1:2, 4:7, 1:7, 2:7, 1:3),
  .y = c(diff(which(h == 1)),24),
  .f = function(x,y) {rep(x,y)}
) %>% 
  unlist()

x_train_with_day = map2(
  .x = day_index[train_index] %>% rep(10), 
  .y = 1:ncol(x_train),
  .f = function(x,y) {c(rep(0,x-1), 1, rep(0,7-x), x_train_with_hour[,y])}
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_train))

x_val_with_day = input_with_day[, test_index[index == 1]]

x_test_with_day = input_with_day[, test_index[index == 2]]

# x_val_with_day = map2(
#   .x = day_index[test_index[index == 1]], 
#   .y = 1:length(test_index[index == 1]),
#   .f = function(x,y) {c(rep(0,x-1), 1, rep(0,7-x), x_val_with_hour[,y])}
# )
# 
# x_test_with_day = map2(
#   .x = day_index[test_index[index == 2]], 
#   .y = 1:length(test_index[index == 2]),
#   .f = function(x,y) {c(rep(0,x-1), 1, rep(0,7-x), x_test_with_hour[,y])}
# )

# -------------------------------------------------------------------------

y_train = output[train_index] %>% rep(10)
y_val = output[test_index[index == 1]]
y_test = output[test_index[index == 2]]


# -------------------------------------------------------------------------

# Dividing the data into train and test
# index = readRDS(file = "RDS/index")   # the same index used to divide into training and test set for the other input vectors
# index = index[1:7800]   # we only need the first 7800 entries

# set.seed(1234)
# index = sample(2,
#                size = 7800,
#                replace = T,
#                prob = c(0.9, 0.1)) # 80% train, 10% validation, 10% test
# 
# # train
# x_train = input[, index == 1]
# y_train = output[index == 1]
# 
# x_train_with_hour = input_with_hour[, index == 1]
# y_train_with_hour = output[index == 1]
# 
# # test
# x_test = input[, index == 2]
# y_test = output[index == 2]
# 
# x_test_with_hour = input_with_hour[, index == 2]   
# y_test_with_hour = output[index == 2]

# -------------------------------------------------------------------------

# Normalize
min_quant = min(x_train[1:(max$max_bid + max$max_ask),][x_train[1:(max$max_bid + max$max_ask),] != 0])
min_price = min(x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),][x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),] != 0])
max_quant = max(x_train[1:(max$max_bid + max$max_ask),])
max_price = max(x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),])

norm = function(input, col, min_quant, min_price, max_quant, max_price){ 
  n = nrow(input) - 2*(max$max_bid + max$max_ask)
  quant = input[(1 + n):(max$max_bid + max$max_ask + n),col] 
  price = input[((max$max_bid + max$max_ask) + 1 + n):(2*(max$max_bid + max$max_ask) + n),col]
  
  quant = quant %>% replace(quant == 0, min_quant)
  price = price %>%  replace(price == 0, min_price)
  
  norm_quant = (quant - min_quant) / (max_quant - min_quant)
  norm_price = (price - min_price) / (max_price - min_price)
  
  if (n == 0){
    return(c(norm_quant, norm_price))
  } else {
    return(c(input[1:n,col], norm_quant, norm_price))
  }
}

# -------------------------------------------------------------------------

x_train = map(
  .x = 1:ncol(x_train),
  .f = function(x) {
    norm(
      input = x_train,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_train))

x_val = map(
  .x = 1:ncol(x_val),
  .f = function(x) {
    norm(
      input = x_val,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_val))

x_test = map(
  .x = 1:ncol(x_test),
  .f = function(x) {
    norm(
      input = x_test,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_test))

# -------------------------------------------------------------------------

x_train_with_day = map(
  .x = 1:ncol(x_train_with_day),
  .f = function(x) {
    norm(
      input = x_train_with_day,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_train_with_day))

x_val_with_day = map(
  .x = 1:ncol(x_val_with_day),
  .f = function(x) {
    norm(
      input = x_val_with_day,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_val_with_day))

x_test_with_day = map(
  .x = 1:ncol(x_test_with_day),
  .f = function(x) {
    norm(
      input = x_test_with_day,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_test_with_day))

# save NN_sets ------------------------------------------------------------

# saveRDS(object = x_train, file = "NN_sets/bootstrap/x_train")
# saveRDS(object = y_train, file = "NN_sets/targets/bootstrap/y_train")


# NN ----------------------------------------------------------------------

# Creating a neural network
nr = nrow(x_train)
tensorflow::set_random_seed(1234)
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 2*nr, activation = "elu", input_shape = c(nr)) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1*nr, activation = "elu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 0.25*nr, activation = "elu") %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)


model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = 'mae')

history <- model %>% fit(
  x = t(x_train),
  y = y_train,
  batch_size = 128,
  epochs = 400,
  validation_data = list(t(x_val),y_val)
)

history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

model %>% evaluate(t(x_test), y_test)
pred <- model %>% predict(t(x_test))
mean((y_test - pred)^2) # another way to compute the loss
plot(y_test, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test), col = "red")



# Creating a neural network (with hour)
nr_hour = nrow(x_train_with_hour)
tensorflow::set_random_seed(1234)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 1*nr_hour, activation = "relu", input_shape = c(nr_hour)) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 0.5*nr_hour, activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 0.25*nr_hour, activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 0.1*nr_hour, activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 50, activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 20, activation = "relu") %>%
  layer_dense(units = 1)


model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = 'mae')

mymodel <- model %>% fit(
  x = t(x_train_with_hour),
  y = y_train_with_hour,
  batch_size = 128,
  epochs = 500,
  validation_data = list(t(x_val_with_hour),y_val_with_hour)
)


model %>% evaluate(t(x_test_with_hour), y_test_with_hour)
pred <- model %>% predict(t(x_test_with_hour))
mean((y_test_with_hour - pred)^2) # another way to compute the loss
plot(y_test_with_hour, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test_with_hour), col = "red")
