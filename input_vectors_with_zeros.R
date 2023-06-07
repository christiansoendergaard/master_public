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

# source functions
sourceDirectory("functions", modifiedOnly = FALSE) 

# load data
data = readRDS(file = "RDS/data_input_output")
output_w_quant = readRDS(file = "RDS/output_vectors")
m = readRDS(file = "RDS/month_indexes") 
d = readRDS(file = "RDS/day_indexes")
h = readRDS(file = "RDS/hour_indexes")

# prepare data ------------------------------------------------------------

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

max = get_max_bid_ask(data)

get_zeros_input = function(data, month, day, hour, max_bid, max_ask, price.quantity = "Quantity"){ 
  date = paste0(
    "2022-",
    sprintf('%0.2d', month),
    "-",
    sprintf('%0.2d', day),
    " hour ",
    sprintf('%0.2d', hour)
  )
  
  q_b = get("quantity_bid" , get(date, data))
  q_a = get("quantity_ask" , get(date, data))
  p_b = get("price_bid" , get(date, data))
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

input = pmap(.l = list(month = m, day = d, hour = h),
             .f = get_zeros_input,
             data = data,
             max_bid = max$max_bid,
             max_ask = max$max_ask, 
             price.quantity = "Quantity_and_price") %>% 
  flatten_dbl() %>% 
  matrix(ncol = length(data))

output = map_dbl(.x = 1:7800,
                 .f = function(x){output_w_quant[[x]][1]})


# Dividing the data into train and test
index = sample(2, 
                size = length(output),  
                replace = T, 
                prob = c(0.8,0.2)) # 80% train, 20% test

# train
x_train = input[, index == 1]
y_train = output[index == 1]

# test
x_test = input[, index == 2]
y_test = output[index == 2]

# Normalize
min_quant = min(x_train[1:(max$max_bid + max$max_ask),][x_train[1:(max$max_bid + max$max_ask),] != 0])
min_price = min(x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),][x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),] != 0])
max_quant = max(x_train[1:(max$max_bid + max$max_ask),])
max_price = max(x_train[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),])

norm = function(input, col, min_quant, min_price, max_quant, max_price){
  quant = input[1:(max$max_bid + max$max_ask),col] 
  price = input[((max$max_bid + max$max_ask) + 1):(2*(max$max_bid + max$max_ask)),col] 
  
  quant = quant %>% replace(quant == 0, min_quant)
  price = price %>%  replace(price == 0, min_price)
  
  norm_quant = (quant - min_quant) / (max_quant - min_quant)
  norm_price = (price - min_price) / (max_price - min_price)
  
  return(c(norm_quant, norm_price))
}

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



# Creating a neural network
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 500, activation = "elu", input_shape = c(2*max$max_bid + 2*max$max_ask)) %>%
  layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 500, activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 300, activation = "relu") %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 250, activation = "elu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50, activation = "elu") %>%
  layer_dropout(rate = 0.2) %>%
  # layer_dense(units = 20, activation = "relu") %>%
  layer_dense(units = 1)


model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = 'mae')

mymodel <- model %>% fit(
  x = t(x_train),
  y = y_train,
  batch_size = 128,
  epochs = 100,
  validation_split = 0.2
)


model %>% evaluate(t(x_test), y_test)
pred <- model %>% predict(t(x_test))
mean((y_test - pred)^2) # another way to compute the loss
plot(y_test, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
