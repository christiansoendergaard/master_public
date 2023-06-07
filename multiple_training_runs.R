
# Multiple training runs (last phase of finding the NN) -------------------

# packages
library(tidyverse)
library(R.utils)
library(tictoc)
library(keras)

# neural network without day and hour information 
NN_without_mult = function(activation, hidden1, hidden2, hidden3, epochs, input_type, run){ 
  nr = nrow(x_train)
  
  n_layers = c(hidden1, hidden2, hidden3) %>% na.omit %>% length()
  
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
  
  saveRDS(object = history, file =  paste0("models/history_", input_type, "_", run))
  save_model_tf(model, paste0("models/model_", input_type, "_", run))
  
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


runs_multiple = function(epochs, n_runs, input_type, 
                         x_train, y_train,
                         x_val, y_val){ 
  
  # method = rep("without", n_runs)
  activation = rep("elu", n_runs)
  hidden1 = rep(2, n_runs)
  hidden2 = rep(1, n_runs) 
  hidden3 = rep(NA, n_runs)
  run = seq(1,5)
  
  tensorflow::set_random_seed(123)
  
  l = pmap(
    .l = list(activation, hidden1, hidden2, hidden3, run), 
    .f = NN_without_mult,
    epochs = epochs,
    input_type = input_type
  )
  
  return(l)  
  
} 

method = rep("without", 5)
activation = rep("elu",5)
hidden1 = rep(2,5)
hidden2 = rep(1,5) 
hidden3 = rep(NA,5)
run = seq(1,5)

runs_mult_training = runs_multiple(
  epochs = 500,
  n_runs = 5,
  input_type = "bootstrap",
  x_train = x_train,
  y_train = y_train,
  x_val = x_val,
  y_val = y_val
) %>%
  set_names(paste0(method, ",", activation, ",", hidden1, ",", hidden2, ",", hidden3))

# saveRDS(object = runs_mult_training, file = "RDS/runs_mult_training_shifted")
runs_mult_training_shifted = readRDS(file = "RDS/runs_mult_training_shifted")

# saveRDS(object = runs_mult_training, file = "RDS/runs_mult_training_bootstrap")
runs_mult_training_bootstrap = readRDS(file = "RDS/runs_mult_training_bootstrap")

# saveRDS(object = runs_mult_training, file = "RDS/runs_mult_training_padding")
runs_mult_training_padding = readRDS(file = "RDS/runs_mult_training_padding")


# data_frame with shifted
df_runs_multiple_shifted = map(.x = runs_mult_training_shifted, 
                               .f = 1
) %>% 
  unlist() %>% 
  matrix(nrow = 5, ncol = 8, byrow = TRUE) %>%
  as.data.frame()

row.names(df_runs_multiple_shifted) = set_names(paste0(method,",", activation,",", hidden1,",", hidden2,",", hidden3, ",", run))
colnames(df_runs_multiple_shifted) = c("Loss", "MAE", "Val_loss", "Val_MAE", "min_loss", "min_mae", "min_val_loss", "min_val_MAE")

# data_frame with bootstrap
df_runs_multiple_bootstrap = map(.x = runs_mult_training_bootstrap, 
                               .f = 1
) %>% 
  unlist() %>% 
  matrix(nrow = 5, ncol = 8, byrow = TRUE) %>%
  as.data.frame()

row.names(df_runs_multiple_bootstrap) = set_names(paste0(method,",", activation,",", hidden1,",", hidden2,",", hidden3, ",", run))
colnames(df_runs_multiple_bootstrap) = c("Loss", "MAE", "Val_loss", "Val_MAE", "min_loss", "min_mae", "min_val_loss", "min_val_MAE")

# data_frame with padding
df_runs_multiple_padding = map(.x = runs_mult_training_padding, 
                                 .f = 1
) %>% 
  unlist() %>% 
  matrix(nrow = 5, ncol = 8, byrow = TRUE) %>%
  as.data.frame()

row.names(df_runs_multiple_padding) = set_names(paste0(method,",", activation,",", hidden1,",", hidden2,",", hidden3, ",", run))
colnames(df_runs_multiple_padding) = c("Loss", "MAE", "Val_loss", "Val_MAE", "min_loss", "min_mae", "min_val_loss", "min_val_MAE")


# testing -----------------------------------------------------------------

get_test_results = function(input_type) {
  method = rep("without", 5)
  activation = rep("elu",5)
  hidden1 = rep(2,5)
  hidden2 = rep(1,5) 
  hidden3 = rep(NA,5)
  run = seq(1,5)
  
  df_test = map(
    .x = 1:5,
    .f = function(x) { 
      new_model = load_model_tf(paste0("models/model_", input_type, "_", x)) 
      new_model %>% evaluate(t(x_test), y_test)
    }
  ) %>% 
    unlist() %>% 
    matrix(nrow = 5, ncol = 2, byrow = TRUE) %>%
    as.data.frame()
  
  row.names(df_test) = set_names(paste0(method,",", activation,",", hidden1,",", hidden2,",", hidden3, ",", run))
  colnames(df_test) = c("test_loss", "test_MAE")
  
  return(df_test)
}

test_padding = get_test_results("padding")
test_shifted = get_test_results("shifted")
test_bootstrap = get_test_results("bootstrap")

# collecting all results --------------------------------------------------

# data frame with original NN (set.seed(1234))
original = runs_act_lay_neu_hourday$`without,elu,2,1,NA`$NN_values %>% 
  matrix(nrow = 1, ncol = 8, byrow = TRUE) %>% 
  as.data.frame()
  
row.names(original) = set_names("padding_original")
colnames(original) = c("Loss", "MAE", "Val_loss", "Val_MAE", "min_loss", "min_mae", "min_val_loss", "min_val_MAE")

# test result original curve 
original_model = load_model_tf("models/model_padding_original")
original_test = original_model %>% evaluate(t(x_test), y_test) %>% 
  matrix(nrow = 1, ncol = 2, byrow = TRUE) %>%
  as.data.frame()

row.names(original_test) = set_names("padding_original_test")
colnames(original_test) = c("test_loss", "test_MAE")

# collecting everything padding
results_multiple_runs_padding = list(
  runs = df_runs_multiple_padding,
  runs_test = test_padding,
  orginal = original,
  original_test = original_test
)

# collecting everything shifted 
results_multiple_runs_shifted = list(
  runs = df_runs_multiple_shifted,
  runs_test = test_shifted
)

# collecting everything bootstrap 
results_multiple_runs_bootstrap = list(
  runs = df_runs_multiple_bootstrap,
  runs_test = test_bootstrap
)

# saveRDS(object = results_multiple_runs_padding, file = "RDS/results_multiple_runs_padding")
results_multiple_runs_padding = readRDS("RDS/results_multiple_runs_padding")

# saveRDS(object = results_multiple_runs_shifted, file = "RDS/results_multiple_runs_shifted")
results_multiple_runs_shifted = readRDS("RDS/results_multiple_runs_shifted")

# saveRDS(object = results_multiple_runs_bootstrap, file = "RDS/results_multiple_runs_bootstrap")
results_multiple_runs_bootstrap = readRDS("RDS/results_multiple_runs_bootstrap")
