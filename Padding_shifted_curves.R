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
library(keras)

# load data
shifted_data = readRDS(file = "RDS/shifted_data") 
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
max = get_max_bid_ask(shifted_data)

# create padded input vectors from the original supply curve and shifted supply curves
get_zeros_input_shifted = function(data, max_bid, max_ask, price.quantity = "Quantity_and_price", i, j){
  
  q_b = get("quantity_bid" , data[[i]])   
  p_b = get("price_bid" , data[[i]])
  p_a = get("price_ask" , data[[i]])
  q_a = data[[i]][[j]]   # i can be chosen from {1,..., 7800} and j from {4, 6,..., 15}
  
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

# inputs (create all input vectors) ---------------------------------------

# input without hour and day (only original data)
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

# input with only hour (only original data)
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

# input with hour and day (7 days) (only original data)
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

# (only original data)
output = map_dbl(.x = 1:7800,
                 .f = function(x){output_w_quant[[x]][1]})


# training, validation, test (consecutive data) ---------------------------

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

# indexes of input/outputs to be used in the training data
train_index = map(
  .x = 1:11,
  .f = function(x) {
    get_index_train_test(x)$train_index
  }
) %>%
  unlist()

# indexes of input/outputs to be used in the validation/test data
test_index = map(
  .x = 1:11,
  .f = function(x) {
    get_index_train_test(x)$test_index
  }
) %>%
  unlist()

# training input for shifted curves (without day and hour)
x_train = map2(.x = map2(.x = train_index, .y = rep(11,length(train_index)), .f = rep) %>% unlist(),
               .y = rep(c(4,6:15), length(train_index)),
               .f = get_zeros_input_shifted,
               data = shifted_data,
               max_bid = max$max_bid,
               max_ask = max$max_ask,
               price.quantity = "Quantity_and_price") %>%
  flatten_dbl() %>%
  matrix(ncol = length(train_index) * 11)

# training outputs for shifted curves
y_train = map2_dbl(.x = map2(.x = train_index, .y = rep(11,length(train_index)), .f = rep) %>% unlist(),
                   .y = rep(c(5,16:25),length(train_index)),
                   .f = function(x,y){shifted_data[[x]][[y]][1]})

# random sampling to create validation and test set
set.seed(1234)
index = sample(2,
               size = length(test_index),
               replace = T, 
               prob = c(0.5,0.5))   # 50% for validation and 50% for test of the last 7 days in each month

# unnormalized validation inputs  
x_val = input[, test_index[index == 1]]
x_val_with_day = input_with_day[, test_index[index == 1]]
y_val = output[test_index[index == 1]]

# unnormalized test inputs
x_test = input[, test_index[index == 2]]
x_test_with_day = input_with_day[, test_index[index == 2]]
y_test = output[test_index[index == 2]]

# shifted test data
x_test = map2(.x = map2(.x = test_index[index == 2], .y = rep(11,length(test_index[index == 2])), .f = rep) %>% unlist(),
               .y = rep(c(4,6:15), length(test_index[index == 2])),
               .f = get_zeros_input_shifted,
               data = shifted_data,
               max_bid = max$max_bid,
               max_ask = max$max_ask,
               price.quantity = "Quantity_and_price") %>%
  flatten_dbl() %>%
  matrix(ncol = length(test_index[index == 2]) * 11)

y_test = map2_dbl(.x = map2(.x = test_index[index == 2], .y = rep(11,length(test_index[index == 2])), .f = rep) %>% unlist(),
                   .y = rep(c(5,16:25),length(test_index[index == 2])),
                   .f = function(x,y){shifted_data[[x]][[y]][1]})


# normalizing the data ----------------------------------------------------

# maximum and minimum values of the prices and aggregated quantities for both curves
min_quant = min(x_train[1:(max$max_bid + max$max_ask),][x_train[1:(max$max_bid + max$max_ask),] != 0])
min_price = min(x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),][x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),] != 0])
max_quant = max(x_train[1:(max$max_bid + max$max_ask),])
max_price = max(x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),])

# function normalizing the data using min-max normalization
norm = function(input, col, min_quant, min_price, max_quant, max_price){
  quant = input[1:(max$max_bid + max$max_ask),col] 
  price = input[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),col] 
  
  quant = quant %>% replace(quant == 0, min_quant)
  price = price %>%  replace(price == 0, min_price)
  
  norm_quant = (quant - min_quant) / (max_quant - min_quant)
  norm_price = (price - min_price) / (max_price - min_price)
  
  return(c(norm_quant, norm_price))
}

# train
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

# validation
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

# test
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

# save NN_sets ------------------------------------------------------------

# saveRDS(object = x_train, file = "NN_sets/shifted/x_train")
# 
# saveRDS(object = x_test, file = "NN_sets/shifted/x_test")
# 
# saveRDS(object = y_train, file = "NN_sets/targets/shifted/y_train")
# 
# saveRDS(object = y_test, file = "NN_sets/targets/shifted/y_test")


# -------------------------------------------------------------------------

# Creating a neural network
nr = nrow(x_train)
tensorflow::set_random_seed(123)
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 2*nr, activation = "elu", input_shape = c(nr)) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1*nr, activation = "elu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 0.25*nr, activation = "elu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 500, activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 250, activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 50, activation = "relu") %>%
  layer_dense(units = 1)

opt = optimizer_adam(learning_rate = 0.001)
model %>% compile(loss = 'mse',
                  optimizer = opt,
                  metrics = 'mae')

tic()
history <- model %>% fit(
  x = t(x_train),
  y = y_train,
  batch_size = 128,
  epochs = 100,
  validation_data = list(t(x_val),y_val)
)
toc()

history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

model %>% evaluate(t(x_test), y_test)
pred <- model %>% predict(t(x_test))
mean((y_test - pred)^2) # another way to compute the loss
plot(y_test, pred)
lines(x = c(1,1500), y = c(1,1500), type = 'l')
abline(lm(pred ~ y_test), col = "red")
