FLAGS <- flags(
  flag_numeric("dropout1", 0.2),
  flag_numeric("dropout2", 0.2),
  flag_numeric("lr", 0.001),
  flag_numeric("batch_size", 128)
)

tensorflow::set_random_seed(1234)
model <- keras_model_sequential()
model %>% layer_dense(units = 2*nr, activation = "elu", input_shape = c(nr)) %>% 
  layer_dropout(rate = FLAGS$dropout1) %>% 
  layer_dense(units = 1*nr, activation = "elu") %>% 
  layer_dropout(rate = FLAGS$dropout2) %>% 
  layer_dense(units = 1)


opt = optimizer_adam(learning_rate = FLAGS$lr)
model %>% keras::compile(loss = 'mse',
                         optimizer = opt,
                         metrics = 'mae')

history <- model %>% fit(
  x = t(x_train),
  y = y_train,
  batch_size = FLAGS$batch_size,
  epochs = 500, 
  validation_data = list(t(x_val), y_val)
)


