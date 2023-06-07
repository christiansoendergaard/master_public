
# NN embedding with hour --------------------------------------------------

nr_hour = nrow(input_with_hour)

tensorflow::set_random_seed(1234)

# input giving the hour of the day
hour_input = layer_input(shape = c(1), name = "hours")

hour_features = hour_input %>%
  layer_embedding(input_dim = 24, output_dim = 2, input_length = 1) %>%
  layer_flatten()

# input giving the curves
sd_input = layer_input(shape = c(2*max$max_bid + 2*max$max_ask), name = "supply.demand")

# concatinating all layers
merged = layer_concatenate(list(hour_features, sd_input))
output = merged %>%
  layer_dense(units = 1*nr_hour, activation = "elu") %>%
  layer_dense(units = floor(0.5*nr_hour), activation = "elu") %>%
  layer_dense(units = floor(0.25*nr_hour), activation = "elu") %>%
  layer_dense(units = 1)

model = keras_model(inputs = list(hour_input, sd_input), outputs = output)

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = 'mae')

# train, val, test
hours_train = apply(x_train_with_hour[1:24,], 2, which.max) - 1
sd_train = t(x_train_with_hour[25:(2*max$max_bid + 2*max$max_ask + 24),])

hours_val = apply(x_val_with_hour[1:24,], 2, which.max) - 1
sd_val = t(x_val_with_hour[25:(2*max$max_bid + 2*max$max_ask + 24),])

hours_test = apply(x_test_with_hour[1:24,], 2, which.max) - 1
sd_test = t(x_test_with_hour[25:(2*max$max_bid + 2*max$max_ask + 24),])

# running the NN
tic()
history <- model %>% fit(
  x = list(hours = hours_train, supply.demand = sd_train),
  y = y_train,
  batch_size = 128,
  epochs = 1,
  validation_data = list(list(hours = hours_val, supply.demand = sd_val), y_val)
)
toc()

# extract minimum values
history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

# test the NN
model %>% evaluate(list(hours_test, sd_test), y_test)
pred <- model %>% predict(list(hours_test, sd_test))
mean((y_test - pred)^2) # another way to compute the loss
plot(y_test_with_hour, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test), col = "red")


# NN embedding with day and hour ---------------------------------------------------

nr_day = nrow(input_with_day)

tensorflow::set_random_seed(1234)

# input giving the day of the week
day_input = layer_input(shape = c(1), name = "day")

day_features = day_input %>% 
  layer_embedding(input_dim = 7, output_dim = 2, input_length = 1) %>%
  layer_flatten()

# input giving the hour of the day
hour_input = layer_input(shape = c(1), name = "hours")

hour_features = hour_input %>% 
  layer_embedding(input_dim = 24, output_dim = 2, input_length = 1) %>%
  layer_flatten()

# input giving the curves
sd_input = layer_input(shape = c(2*max$max_bid + 2*max$max_ask), name = "supply.demand")

# concatinating all layers
merged = layer_concatenate(list(day_features, hour_features, sd_input))
output = merged %>% 
  layer_dense(units = 4*nr_day, activation = "elu") %>%
  layer_dense(units = floor(2*nr_day), activation = "elu") %>%
  # layer_dense(units = floor(0.25*nr_day), activation = "elu") %>%
  layer_dense(units = 1)

model = keras_model(inputs = list(day_input ,hour_input, sd_input), outputs = output)

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = 'mae')

# train, val, test (7 days)
day_train = apply(x_train_with_day[1:7,], 2, which.max) - 1 
hours_train = apply(x_train_with_day[8:31,], 2, which.max) - 1
sd_train = t(x_train_with_day[32:(2*max$max_bid + 2*max$max_ask + 24 + 7),])

day_val = apply(x_val_with_day[1:7,], 2, which.max) - 1
hours_val = apply(x_val_with_day[8:31,], 2, which.max) - 1
sd_val = t(x_val_with_day[32:(2*max$max_bid + 2*max$max_ask + 24 + 7),])

day_test = apply(x_test_with_day[1:7,], 2, which.max) - 1
hours_test = apply(x_test_with_day[8:31,], 2, which.max) - 1
sd_test = t(x_test_with_day[32:(2*max$max_bid + 2*max$max_ask + 24 + 7),])

# # train, val, test (weekdays and weekend)
# day_train = apply(x_train_with_day[1:2,], 2, which.max) - 1 
# hours_train = apply(x_train_with_day[3:26,], 2, which.max) - 1
# sd_train = t(x_train_with_day[27:(2*max$max_bid + 2*max$max_ask + 24 + 2),])
# 
# day_val = apply(x_val_with_day[1:2,], 2, which.max) - 1
# hours_val = apply(x_val_with_day[3:26,], 2, which.max) - 1
# sd_val = t(x_val_with_day[27:(2*max$max_bid + 2*max$max_ask + 24 + 2),])
# 
# day_test = apply(x_test_with_day[1:2,], 2, which.max) - 1
# hours_test = apply(x_test_with_day[3:26,], 2, which.max) - 1
# sd_test = t(x_test_with_day[27:(2*max$max_bid + 2*max$max_ask + 24 + 2),])


# running the NN
tic()
history <- model %>% fit(
  x = list(day = day_train, hours = hours_train, supply.demand = sd_train),
  y = y_train,
  batch_size = 128,
  epochs = 500,
  validation_data = list(list(day = day_val, hours = hours_val, supply.demand = sd_val), y_val)
)
toc()

# extract minimum values
history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

# test the NN
model %>% evaluate(list(day_test, hours_test, sd_test), y_test)
pred <- model %>% predict(list(day_test, hours_test, sd_test))
mean((y_test - pred)^2) # another way to compute the loss
plot(y_test, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test), col = "red")




# NN embedding with month, day and hour -----------------------------------

nr_month = nrow(input_with_month)

tensorflow::set_random_seed(1234)

# input giving the day of the week
month_input = layer_input(shape = c(1), name = "month")

month_features = month_input %>% 
  layer_embedding(input_dim = 11, output_dim = 2, input_length = 1) %>%
  layer_flatten()

# input giving the day of the week
day_input = layer_input(shape = c(1), name = "day")

day_features = day_input %>% 
  layer_embedding(input_dim = 7, output_dim = 2, input_length = 1) %>%
  layer_flatten()

# input giving the hour of the day
hour_input = layer_input(shape = c(1), name = "hours")

hour_features = hour_input %>% 
  layer_embedding(input_dim = 24, output_dim = 2, input_length = 1) %>%
  layer_flatten()

# input giving the curves
sd_input = layer_input(shape = c(2*max$max_bid + 2*max$max_ask), name = "supply.demand")

# concatinating all layers
merged = layer_concatenate(list(month_features, day_features, hour_features, sd_input))
output = merged %>% 
  layer_dense(units = 4*nr_month, activation = "relu") %>%
  layer_dense(units = floor(2*nr_month), activation = "relu") %>%
  # layer_dense(units = floor(0.25*nr_day), activation = "relu") %>%
  layer_dense(units = 1)

model = keras_model(inputs = list(month_input, day_input ,hour_input, sd_input), outputs = output)

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = 'mae')

# train, val, test (7 days)
month_train = apply(x_train_with_month[1:11,], 2, which.max) - 1
day_train = apply(x_train_with_month[12:18,], 2, which.max) - 1 
hours_train = apply(x_train_with_month[19:42,], 2, which.max) - 1
sd_train = t(x_train_with_month[43:(2*max$max_bid + 2*max$max_ask + 24 + 7 + 11),])

month_val = apply(x_val_with_month[1:11,], 2, which.max) - 1
day_val = apply(x_val_with_month[12:18,], 2, which.max) - 1 
hours_val = apply(x_val_with_month[19:42,], 2, which.max) - 1
sd_val = t(x_val_with_month[43:(2*max$max_bid + 2*max$max_ask + 24 + 7 + 11),])

month_test = apply(x_test_with_month[1:11,], 2, which.max) - 1
day_test = apply(x_test_with_month[12:18,], 2, which.max) - 1 
hours_test = apply(x_test_with_month[19:42,], 2, which.max) - 1
sd_test = t(x_test_with_month[43:(2*max$max_bid + 2*max$max_ask + 24 + 7 + 11),])

# # train, val, test (weekdays and weekend)
# day_train = apply(x_train_with_day[1:2,], 2, which.max) - 1 
# hours_train = apply(x_train_with_day[3:26,], 2, which.max) - 1
# sd_train = t(x_train_with_day[27:(2*max$max_bid + 2*max$max_ask + 24 + 2),])
# 
# day_val = apply(x_val_with_day[1:2,], 2, which.max) - 1
# hours_val = apply(x_val_with_day[3:26,], 2, which.max) - 1
# sd_val = t(x_val_with_day[27:(2*max$max_bid + 2*max$max_ask + 24 + 2),])
# 
# day_test = apply(x_test_with_day[1:2,], 2, which.max) - 1
# hours_test = apply(x_test_with_day[3:26,], 2, which.max) - 1
# sd_test = t(x_test_with_day[27:(2*max$max_bid + 2*max$max_ask + 24 + 2),])


# running the NN
tic()
history <- model %>% fit(
  x = list(month = month_train, day = day_train, hours = hours_train, supply.demand = sd_train),
  y = y_train,
  batch_size = 128,
  epochs = 1,
  validation_data = list(list(month = month_val, day = day_val, hours = hours_val, supply.demand = sd_val), y_val)
)
toc()

# extract minimum values
history$metrics$loss %>% min()
history$metrics$mae %>% min()
history$metrics$val_loss %>% min()
history$metrics$val_mae %>% min()

# test the NN
model %>% evaluate(list(day_test, hours_test, sd_test), y_test)
pred <- model %>% predict(list(day_test, hours_test, sd_test))
mean((y_test - pred)^2) # another way to compute the loss
plot(y_test, pred)
lines(x = c(1,1000), y = c(1,1000), type = 'l')
abline(lm(pred ~ y_test), col = "red")
