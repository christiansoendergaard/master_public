
# training runs: activation function, number of layers and neurons --------

# neural network without day and hour information 
NN_without = function(activation, hidden1, hidden2, hidden3, epochs){ 
  nr = nrow(x_train)
  
  n_layers = c(hidden1, hidden2, hidden3) %>% na.omit %>% length()

  tensorflow::set_random_seed(1234)

  if (n_layers == 3) {
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = floor(hidden1*nr), activation = activation, input_shape = c(nr)) %>%
      layer_dense(units = floor(hidden2*nr), activation = activation) %>%
      layer_dense(units = floor(hidden3*nr), activation = activation) %>%
      layer_dense(units = 1)
  }

  if (n_layers == 2) {
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = floor(hidden1*nr), activation = activation, input_shape = c(nr)) %>%
      layer_dense(units = floor(hidden2*nr), activation = activation) %>%
      layer_dense(units = 1)
  }


  # compiler
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
    epochs = epochs,
    validation_data = list(t(x_val),y_val)
  )
  toc(log = TRUE, quiet = TRUE)
  log.txt = tic.log(format = TRUE) %>% unlist()
  tic.clearlog()
  
  

  train_loss = history$metrics$loss %>% tail(1)
  train_mae = history$metrics$mae %>% tail(1)
  val_loss = history$metrics$val_loss %>% tail(1)
  val_mae = history$metrics$val_mae %>% tail(1)

  # extract minimum values
  train_loss_min = history$metrics$loss %>% min()
  train_mae_min = history$metrics$mae %>% min()
  val_loss_min = history$metrics$val_loss %>% min()
  val_mae_min = history$metrics$val_mae %>% min()

  NN_val = c(train_loss, train_mae, val_loss, val_mae,
             train_loss_min, train_mae_min, val_loss_min, val_mae_min)

  list(NN_values = NN_val, time_epoch = log.txt)
}

NN_with = function(activation, hidden1, hidden2, hidden3, epochs){
  
  # nr = nrow(input_with_day)   # with day and hour information
  nr = nrow(input_with_hour)   # with hour information
  
  n_layers = c(hidden1, hidden2, hidden3) %>% na.omit %>% length()
  
  tensorflow::set_random_seed(1234)
  
  if (n_layers == 3) {
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = floor(hidden1*nr), activation = activation, input_shape = c(nr)) %>%
      layer_dense(units = floor(hidden2*nr), activation = activation) %>%
      layer_dense(units = floor(hidden3*nr), activation = activation) %>%
      layer_dense(units = 1)
  }
  
  if (n_layers == 2) {
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = floor(hidden1*nr), activation = activation, input_shape = c(nr)) %>%
      layer_dense(units = floor(hidden2*nr), activation = activation) %>%
      layer_dense(units = 1)
  }
  
  # compiler
  opt = optimizer_adam(learning_rate = 0.001)
  model %>% compile(loss = 'mse',
                    optimizer = opt,
                    metrics = 'mae')
  
  # running the network 
  tic()
  history <- model %>% fit(
    # x = t(x_train_with_day),   # with day and hour information
    x = t(x_train_with_hour),
    y = y_train,
    batch_size = 128,
    epochs = epochs,
    # validation_data = list(t(x_val_with_day),y_val)   # with day and hour information
    validation_data = list(t(x_val_with_hour),y_val)   # with huor information
  )
  toc(log = TRUE, quiet = TRUE)
  log.txt = tic.log(format = TRUE) %>% unlist()
  tic.clearlog()
  
  train_loss = history$metrics$loss %>% tail(1)
  train_mae = history$metrics$mae %>% tail(1)
  val_loss = history$metrics$val_loss %>% tail(1)
  val_mae = history$metrics$val_mae %>% tail(1)
  
  # extract minimum values
  train_loss_min = history$metrics$loss %>% min()
  train_mae_min = history$metrics$mae %>% min()
  val_loss_min = history$metrics$val_loss %>% min()
  val_mae_min = history$metrics$val_mae %>% min()
  
  NN_val = c(train_loss, train_mae, val_loss, val_mae,
             train_loss_min, train_mae_min, val_loss_min, val_mae_min)
  
  return(list(NN_values = NN_val, time_epoch = log.txt))
}

NN_embedding = function(activation, hidden1, hidden2, hidden3, epochs){
  
  # nr = nrow(input_with_day)   # with day and hour information
  nr = nrow(input_with_hour)   # with hour information
  
  max_bid = max$max_bid
  max_ask = max$max_ask
  
  n_layers = c(hidden1, hidden2, hidden3) %>% na.omit %>% length()
  
  tensorflow::set_random_seed(1234)
  
  # # input giving the day of the week
  # day_input = layer_input(shape = c(1), name = "day")
  # 
  # day_features = day_input %>% 
  #   layer_embedding(input_dim = 7, output_dim = 2, input_length = 1) %>%
  #   layer_flatten()
  
  # input giving the hour of the day
  hour_input = layer_input(shape = c(1), name = "hours")
  
  hour_features = hour_input %>% 
    layer_embedding(input_dim = 24, output_dim = 2, input_length = 1) %>%
    layer_flatten()
  
  # input giving the curves
  sd_input = layer_input(shape = c(2*max_bid + 2*max_ask), name = "supply.demand")
  
  # concatinating all layers
  # merged = layer_concatenate(list(day_features, hour_features, sd_input))   # with day and hour information
  merged = layer_concatenate(list(hour_features, sd_input))   # with hour information
  
  if (n_layers == 3) {
  output = merged %>% 
    layer_dense(units = floor(hidden1*nr), activation = activation) %>%
    layer_dense(units = floor(hidden2*nr), activation = activation) %>%
    layer_dense(units = floor(hidden3*nr), activation = activation) %>%
    layer_dense(units = 1)
  }
  
  if (n_layers == 2){
    output = merged %>% 
      layer_dense(units = floor(hidden1*nr), activation = activation) %>%
      layer_dense(units = floor(hidden2*nr), activation = activation) %>%
      layer_dense(units = 1)
  }
  
  # model = keras_model(inputs = list(day_input ,hour_input, sd_input), outputs = output)   # with day and hour information
  model = keras_model(inputs = list(hour_input, sd_input), outputs = output)   # with hour information
  
  model %>% compile(loss = 'mse',
                    optimizer = 'adam',
                    metrics = 'mae')
  
  # # train, val, test (7 days), with day and hour information
  # day_train = apply(x_train_with_day[1:7,], 2, which.max) - 1 
  # hours_train = apply(x_train_with_day[8:31,], 2, which.max) - 1
  # sd_train = t(x_train_with_day[32:(2*max_bid + 2*max_ask + 24 + 7),])
  # 
  # day_val = apply(x_val_with_day[1:7,], 2, which.max) - 1
  # hours_val = apply(x_val_with_day[8:31,], 2, which.max) - 1
  # sd_val = t(x_val_with_day[32:(2*max_bid + 2*max_ask + 24 + 7),])
  # 
  # day_test = apply(x_test_with_day[1:7,], 2, which.max) - 1
  # hours_test = apply(x_test_with_day[8:31,], 2, which.max) - 1
  # sd_test = t(x_test_with_day[32:(2*max_bid + 2*max_ask + 24 + 7),])
  
  # train, val, test, with hour information
  hours_train = apply(x_train_with_hour[1:24,], 2, which.max) - 1
  sd_train = t(x_train_with_hour[25:(2*max_bid + 2*max_ask + 24),])
  
  hours_val = apply(x_val_with_hour[1:24,], 2, which.max) - 1
  sd_val = t(x_val_with_hour[25:(2*max_bid + 2*max_ask + 24),])
  
  hours_test = apply(x_test_with_hour[1:24,], 2, which.max) - 1
  sd_test = t(x_test_with_hour[25:(2*max_bid + 2*max_ask + 24),])
  
  # running the NN
  tic()
  history <- model %>% fit(
    # x = list(day = day_train, hours = hours_train, supply.demand = sd_train),   # with day and hour information
    x = list(hours = hours_train, supply.demand = sd_train),   # with hour information
    y = y_train,
    batch_size = 128,
    epochs = epochs,
    # validation_data = list(list(day = day_val, hours = hours_val, supply.demand = sd_val), y_val)   # with day and hour information
    validation_data = list(list(hours = hours_val, supply.demand = sd_val), y_val)   # with hour information
  )
  toc(log = TRUE, quiet = TRUE)
  log.txt = tic.log(format = TRUE) %>% unlist()
  tic.clearlog()
  
  train_loss = history$metrics$loss %>% tail(1)
  train_mae = history$metrics$mae %>% tail(1)
  val_loss = history$metrics$val_loss %>% tail(1)
  val_mae = history$metrics$val_mae %>% tail(1)
  
  # extract minimum values
  train_loss_min = history$metrics$loss %>% min()
  train_mae_min = history$metrics$mae %>% min()
  val_loss_min = history$metrics$val_loss %>% min()
  val_mae_min = history$metrics$val_mae %>% min()
  
  NN_val = c(train_loss, train_mae, val_loss, val_mae,
             train_loss_min, train_mae_min, val_loss_min, val_mae_min)
  
  return(list(NN_values = NN_val, time_epoch = log.txt))
  

}

runs_activation_layers_neurons = function(epochs,
                                          x_train, y_train, x_train_with_day,
                                          x_val, y_val, x_val_with_day){ 
  
  method = c(
    # rep("without", 10), 
    rep("with", 10), 
    rep("embedding", 10))
  activation = c(rep("relu", 5), rep("elu",5)) %>% rep(2)
  hidden1 = c(2,4,1,2,4) %>% rep(4)
  hidden2 = c(1,2,0.5,1,2) %>% rep(4)
  hidden3 = c(NA,NA,0.25,0.5,1) %>% rep(4)
  
  l = pmap(
    .l = list(method, activation, hidden1, hidden2, hidden3), 
    .f = function(method, activation, hidden1, hidden2, hidden3){
      
      if (method == "without"){
        NN_without(activation, hidden1, hidden2, hidden3, epochs) 
      } else if (method == "with"){
        NN_with(activation, hidden1, hidden2, hidden3,epochs) 
      } else if (method == "embedding"){
        NN_embedding(activation, hidden1, hidden2, hidden3, epochs) 
      }
    }
  )
    
  return(l)  
  
} 

method = c(
  rep("without", 10),
  rep("with", 10), 
  rep("embedding", 10))
activation = c(rep("relu", 5), rep("elu",5)) %>% rep(3)
hidden1 = c(2,4,1,2,4) %>% rep(6)
hidden2 = c(1,2,0.5,1,2) %>% rep(6)
hidden3 = c(NA,NA,0.25,0.5,1) %>% rep(6)

runs_act_lay_neu = runs_activation_layers_neurons(epochs = 500,
                               x_train = x_train,
                               y_train = y_train,
                               x_train_with_day = x_train_with_hour,
                               x_val = x_val,
                               y_val = y_val,
                               x_val_with_day = x_val_with_hour) %>% 
  set_names(paste0(method,",", activation,",", hidden1,",", hidden2,",", hidden3))

# saveRDS(object = runs_act_lay_neu, file = "RDS/runs_act_lay_neu_hour")
runs_act_lay_neu_hour = readRDS(file = "RDS/runs_act_lay_neu_hour")

# saveRDS(object = runs_act_lay_neu, file = "RDS/runs_act_lay_neu_hourday")
runs_act_lay_neu_hourday = readRDS(file = "RDS/runs_act_lay_neu_hourday")

# saveRDS(object = runs_act_lay_neu, file = "RDS/runs_act_lay_neu_hourdaymonth")
runs_act_lay_neu_hourdaymonth = readRDS(file = "RDS/runs_act_lay_neu_hourdaymonth")

# data_frame with hour 
df_runs_act_lay_neu_hour = map(.x = runs_act_lay_neu_hour, 
    .f = 1
) %>% 
  unlist() %>% 
  matrix(nrow = 20, ncol = 8, byrow = TRUE) %>%
  as.data.frame()
  
row.names(df_runs_act_lay_neu_hour) = set_names(paste0(method,",", activation,",", hidden1,",", hidden2,",", hidden3))
colnames(df_runs_act_lay_neu_hour) = c("Loss", "MAE", "Val_loss", "Val_MAE", "min_loss", "min_mae", "min_val_loss", "min_val_MAE")

# dataframe with day and hour
df_runs_act_lay_neu_hourday = map(.x = runs_act_lay_neu_hourday, 
                         .f = 1
) %>% 
  unlist() %>% 
  matrix(nrow = 30, ncol = 8, byrow = TRUE) %>%
  as.data.frame()

row.names(df_runs_act_lay_neu_hourday) = set_names(paste0(method,",", activation,",", hidden1,",", hidden2,",", hidden3))
colnames(df_runs_act_lay_neu_hourday) = c("Loss", "MAE", "Val_loss", "Val_MAE", "min_loss", "min_mae", "min_val_loss", "min_val_MAE")

# dataframe with month, day and hour
df_runs_act_lay_neu_hourdaymonth = map(.x = runs_act_lay_neu_hourdaymonth, 
                         .f = 1
) %>% 
  unlist() %>% 
  matrix(nrow = 20, ncol = 8, byrow = TRUE) %>%
  as.data.frame()

row.names(df_runs_act_lay_neu_hourdaymonth) = set_names(paste0(method,",", activation,",", hidden1,",", hidden2,",", hidden3))
colnames(df_runs_act_lay_neu_hourdaymonth) = c("Loss", "MAE", "Val_loss", "Val_MAE", "min_loss", "min_mae", "min_val_loss", "min_val_MAE")
