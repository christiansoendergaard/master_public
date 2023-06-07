
# shifted test set --------------------------------------------------------

y_test_shifted = readRDS("NN_sets/targets/shifted/y_test")

outlier = which.max(y_test_shifted)

x_test_shifted = readRDS("NN_sets/shifted/x_test")[,-outlier]
y_test_shifted = readRDS("NN_sets/targets/shifted/y_test")[-outlier]

padding_model = load_model_tf("models/model_padding_3") 
test_padding = padding_model %>% evaluate(t(x_test_shifted), y_test_shifted)

bootstrap_model = load_model_tf("models/model_bootstrap_3")
test_bootstrap = bootstrap_model %>% evaluate(t(x_test_shifted), y_test_shifted)

shifted_model = load_model_tf("models/model_shifted_5")
test_shifted = shifted_model %>% evaluate(t(x_test_shifted), y_test_shifted)

df_test_shifted = matrix(
  c(test_padding, test_bootstrap, test_shifted),
  nrow = 3, 
  ncol = 2,
  byrow = TRUE
) %>% 
  as.data.frame()

row.names(df_test_shifted) = set_names(c("padding", "bootstrap", "shifted"))
colnames(df_test_shifted) = c("test_loss", "test_MAE")


# lm models ---------------------------------------------------------------

x_test = readRDS("NN_sets/test/x_test")
y_test = readRDS("NN_sets/targets/original/y_test")

pred_padding_shifted = padding_model %>% predict(t(x_test_shifted))
pred_bootstrap_shifted = bootstrap_model %>% predict(t(x_test_shifted))
pred_shifted_shifted = shifted_model %>% predict(t(x_test_shifted))

pred_padding = padding_model %>% predict(t(x_test))
pred_bootstrap = bootstrap_model %>% predict(t(x_test))
pred_shifted = shifted_model %>% predict(t(x_test))

lm_padding_shifted = lm(y_test_shifted ~ pred_padding_shifted) %>% summary()
lm_bootstrap_shifted = lm(y_test_shifted ~ pred_bootstrap_shifted) %>% summary()
lm_shifted_shifted = lm(y_test_shifted ~ pred_shifted_shifted) %>% summary()

lm_padding = lm(y_test ~ pred_padding) %>%  summary()
lm_bootstrap = lm(y_test ~ pred_bootstrap) %>% summary()
lm_shifted = lm(y_test ~ pred_shifted) %>% summary()




