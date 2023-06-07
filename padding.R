# packages, sourcing functions and load data ------------------------------

# packages
library(tidyverse)
library(R.utils)
library(tictoc)
library(keras)

# load data
data = readRDS(file = "RDS/data_input_output")
output_w_quant = readRDS(file = "RDS/output_vectors")
m = readRDS(file = "RDS/month_indexes") 
d = readRDS(file = "RDS/day_indexes")
h = readRDS(file = "RDS/hour_indexes")

# dir.create("/srv/scratch/csan18")
# keras::install_keras(envname = "/srv/scratch/csan18/r-miniconda")

library(tensorflow)
use_python("/srv/scratch/csan18/r-miniconda/bin/python")
library(keras)

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

# inputs (create all input vectors) ---------------------------------------

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
  matrix(ncol = length(data))

input_with_month = map2(
  .x = m,
  .y = 1:7800,
  .f = function(x,y) {c(rep(0,x-1), 1, rep(0,11-x), input_with_day[,y])}
) %>% flatten_dbl() %>% 
  matrix(ncol = length(data))

# # input with hour and day (2 days)
# input_with_day = map(
#   .x = 1:7800,
#   .f = function(x){
#     if (sum(input_with_day[1:5,x]) == 1){
#       c(1,0,input_with_hour[,x])
#     } else {
#       c(0,1,input_with_hour[,x])
#     }
#   }
# ) %>%
#   flatten_dbl() %>%
#   matrix(ncol = 7800)


# all targets/outputs -----------------------------------------------------

output = map_dbl(.x = 1:7800,
                 .f = function(x){output_w_quant[[x]][1]})


# training, validation and test set ---------------------------------------

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

# unnormalized training inputs 
x_train = input[, train_index]
x_train_with_hour = input_with_hour[, train_index]
x_train_with_day = input_with_day[, train_index]
x_train_with_month = input_with_month[, train_index]

# # shifted curves
# x_train = map2(.x = map2(.x = train_index, .y = rep(11,length(train_index)), .f = rep) %>% unlist(),
#                .y = rep(c(4,6:15), length(train_index)),
#                .f = get_zeros_input_shifted,
#                data = shifted_data,
#                max_bid = max$max_bid,
#                max_ask = max$max_ask, 
#                price.quantity = "Quantity_and_price") %>% 
#   flatten_dbl() %>% 
#   matrix(ncol = length(train_index) * 11)


# random sampling to create validation and test set
set.seed(1234)
index = sample(2,
               size = length(test_index),
               replace = T, 
               prob = c(0.5,0.5))   # 50% for validation and 50% for test of the last 7 days in each month

# unnormalized validation inputs  
x_val = input[, test_index[index == 1]]
x_val_with_hour = input_with_hour[, test_index[index == 1]]
x_val_with_day = input_with_day[, test_index[index == 1]]
x_val_with_month = input_with_month[, test_index[index == 1]]

# unnormalized test inputs
x_test = input[, test_index[index == 2]]
x_test_with_hour = input_with_hour[, test_index[index == 2]]
x_test_with_day = input_with_day[, test_index[index == 2]]
x_test_with_month = input_with_month[, test_index[index == 2]]

# outputs
y_train = output[train_index]

# # shifted curves 
# y_train = map2_dbl(.x = map2(.x = train_index, .y = rep(11,length(train_index)), .f = rep) %>% unlist(),
#                    .y = rep(c(5,16:25),length(train_index)),
#                    .f = function(x,y){shifted_data[[x]][[y]][1]})

y_val = output[test_index[index == 1]]

y_test = output[test_index[index == 2]]


# normalizing the data ----------------------------------------------------

# maximum and minimum values of the prices and aggregated quantities for both curves
min_quant = min(x_train[1:(max$max_bid + max$max_ask),][x_train[1:(max$max_bid + max$max_ask),] != 0])
min_price = min(x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),][x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),] != 0])
max_quant = max(x_train[1:(max$max_bid + max$max_ask),])
max_price = max(x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),])

# function normalizing the data using min-max normalization
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

# ------------------------

# train without hour
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

# validation without hour
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

# test without hour
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

# ------------------------

# train with hour
x_train_with_hour = map(
  .x = 1:ncol(x_train_with_hour),
  .f = function(x) {
    norm(
      input = x_train_with_hour,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_train_with_hour))

# validation with hour
x_val_with_hour = map(
  .x = 1:ncol(x_val_with_hour),
  .f = function(x) {
    norm(
      input = x_val_with_hour,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_val_with_hour))

# test with hout
x_test_with_hour = map(
  .x = 1:ncol(x_test_with_hour),
  .f = function(x) {
    norm(
      input = x_test_with_hour,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_test_with_hour))

# ------------------------

# train with day
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

# validation with day
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

# test with day
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

# ------------------------

# train with month
x_train_with_month = map(
  .x = 1:ncol(x_train_with_month),
  .f = function(x) {
    norm(
      input = x_train_with_month,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_train_with_month))

# validation with month
x_val_with_month = map(
  .x = 1:ncol(x_val_with_month),
  .f = function(x) {
    norm(
      input = x_val_with_month,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_val_with_month))

# test with month
x_test_with_month = map(
  .x = 1:ncol(x_test_with_month),
  .f = function(x) {
    norm(
      input = x_test_with_month,
      col = x,
      min_quant = min_quant,
      min_price = min_price,
      max_quant = max_quant,
      max_price = max_price
    )
  }
) %>%
  flatten_dbl() %>%
  matrix(ncol = ncol(x_test_with_month))


# save NN_sets ------------------------------------------------------------

saveRDS(object = x_train, file = "NN_sets/padding_train/x_train")
saveRDS(object = x_val, file = "NN_sets/validation/x_val")
saveRDS(object = x_test, file = "NN_sets/test/x_test")

saveRDS(object = x_train_with_hour, file = "NN_sets/padding_train/x_train_with_hour")
saveRDS(object = x_val_with_hour, file = "NN_sets/validation/x_val_with_hour")
saveRDS(object = x_test_with_hour, file = "NN_sets/test/x_test_with_hour")

saveRDS(object = x_train_with_day, file = "NN_sets/padding_train/x_train_with_day")
saveRDS(object = x_val_with_day, file = "NN_sets/validation/x_val_with_day")
saveRDS(object = x_test_with_day, file = "NN_sets/test/x_test_with_day")

saveRDS(object = x_train_with_month, file = "NN_sets/padding_train/x_train_with_month")
saveRDS(object = x_val_with_month, file = "NN_sets/validation/x_val_with_month")
saveRDS(object = x_test_with_month, file = "NN_sets/test/x_test_with_month")

saveRDS(object = y_train, file = "NN_sets/targets/original/y_train")
saveRDS(object = y_val, file = "NN_sets/targets/original/y_val")
saveRDS(object = y_test, file = "NN_sets/targets/original/y_test")


# NN without month, day and hour ------------------------------------------

# Creating a neural network
nr = nrow(x_train)
tensorflow::set_random_seed(1234)
# tensorflow::set_random_seed(123)
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 2*nr, activation = "elu", input_shape = c(nr)) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1*nr, activation = "elu") %>%
  # layer_dropout(rate = 0.4) %>%
  # layer_dense(units = 0.5*nr, activation = "elu") %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

#compiler
opt = optimizer_adam(learning_rate = 0.001)
model %>% compile(loss = 'mse',
                  optimizer = opt,
                  metrics = 'mae')

# running the network
tic()
history <- model %>% fit(
  x = t(x_train),
  y = y_train,
  batch_size = 128,
  epochs = 500,
  validation_data = list(t(x_val),y_val)
)
toc()

# extract minimum values
history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

# predict/test
model %>% evaluate(t(x_test), y_test)
pred <- model %>% predict(t(x_test))
mean((y_test - pred)^2) # another way to compute the loss
plot(y_test, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test), col = "red")

saveRDS(object = history, file = "models/history_padding_original")
save_model_tf(model, "models/model_padding_original")
# new_model = load_model_tf('models/model_padding_1')


# NN with hour ------------------------------------------------------------

# Creating a neural network (with hour)
nr_hour = nrow(input_with_hour)
tensorflow::set_random_seed(1234)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 1*nr_hour, activation = "elu", input_shape = c(nr_hour)) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = floor(0.5*nr_hour), activation = "elu") %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = floor(0.25*nr_hour), activation = "elu") %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# compiler
opt = optimizer_adam(learning_rate = 0.001)
model %>% compile(loss = 'mse',
                  optimizer = opt,
                  metrics = 'mae')

# running the network
tic()
history <- model %>% fit(
  x = t(x_train_with_hour),
  y = y_train,
  batch_size = 128,
  epochs = 1,
  validation_data = list(t(x_val_with_hour),y_val)
)
toc()

# extract minimum values
history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

# predict/test
model %>% evaluate(t(x_test_with_hour), y_test_with_hour)
pred <- model %>% predict(t(x_test_with_hour))
mean((y_test_with_hour - pred)^2) # another way to compute the loss
plot(y_test_with_hour, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test_with_hour), col = "red")


# NN with day and hour -------------------------------------------------------------

# Creating a neural network (with day)
nr_day = nrow(input_with_day)
tensorflow::set_random_seed(1234)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 2*nr_day, activation = "relu", input_shape = c(nr_day)) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = floor(1*nr_day), activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = floor(0.5*nr_day), activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# compiler
opt = optimizer_adam(learning_rate = 0.001)
model %>% compile(loss = 'mse',
                  optimizer = opt,
                  metrics = 'mae')

# running the network 
tic()
history <- model %>% fit(
  x = t(x_train_with_day),
  y = y_train,
  batch_size = 128,
  epochs = 500,
  validation_data = list(t(x_val_with_day),y_val)
)
toc()

# extract minimum values
history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

# predict/test
model %>% evaluate(t(x_test_with_day), y_test_with_day)
pred <- model %>% predict(t(x_test_with_day))
mean((y_test_with_day - pred)^2) # another way to compute the loss
plot(y_test_with_day, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test_with_day), col = "red")


# NN with month, day and hour -----------------------------------------------------------

# Creating a neural network (with day)
nr_month = nrow(input_with_month)
tensorflow::set_random_seed(1234)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 2*nr_month, activation = "elu", input_shape = c(nr_month)) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = floor(1*nr_month), activation = "elu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = floor(0.5*nr_day), activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# compiler
opt = optimizer_adam(learning_rate = 0.001)
model %>% compile(loss = 'mse',
                  optimizer = opt,
                  metrics = 'mae')

# running the network 
tic()
history <- model %>% fit(
  x = t(x_train_with_month),
  y = y_train,
  batch_size = 128,
  epochs = 1,
  validation_data = list(t(x_val_with_month),y_val)
)
toc()

# extract minimum values
history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

# predict/test
model %>% evaluate(t(x_test_with_day), y_test_with_day)
pred <- model %>% predict(t(x_test_with_day))
mean((y_test_with_day - pred)^2) # another way to compute the loss
plot(y_test_with_day, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test_with_day), col = "red")
