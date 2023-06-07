library(tidyverse)
library(R.utils)
library(keras)
library(tensorflow)
library(tictoc)
library(purrr)
library(cowplot)



x_test_shifted = readRDS("NN_sets/shifted/x_test")
y_test_shifted = readRDS("NN_sets/targets/shifted/y_test")
x_test = readRDS("NN_sets/test/x_test")
y_test = readRDS("NN_sets/targets/original/y_test")

# Padding model -----------------------------------------------------------

# On the test without shifted curves
model = load_model_tf("models/model_padding_3")
model %>% evaluate(t(x_test), y_test)

pred <- model %>% predict(t(x_test))
pred = pred %>% as.vector()

df = tibble("Prediction" = pred, "True value" = y_test)
plot_padding = ggplot(data = df, aes(x = Prediction, y = `True value`)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "lm", se = FALSE,
              data = tibble(x = c(-5,885), y = c(-5,885)),
              mapping = aes(x = x, y = y), lwd = 1, color = "red") +
  theme_bw() + 
  labs(title = "Predictions vs true values on original curves",
       subtitle = "Network trained on original curves (padding)")
lm_model = lm(formula = y_test ~ pred)
lm_sum = summary(lm_model)
lm_sum$r.squared

# On the test with shifted curves
model %>% evaluate(t(x_test_shifted[,-8032]), y_test_shifted[-8032])

pred <- model %>% predict(t(x_test_shifted[,-8032]))
pred = pred %>% as.vector()

df = tibble("Prediction" = pred, "True value" = y_test_shifted[-8032])
plot_padding_shifted = ggplot(data = df, aes(x = Prediction, y = `True value`)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "lm", se = FALSE,
              data = tibble(x = c(-110,1200), y = c(-110,1200)),
              mapping = aes(x = x, y = y), lwd = 1, color = "red") +
  theme_bw() + 
  labs(title = "Predictions versus true values on shifted curves",
       subtitle = "Network trained on original curves (padding)")
lm_model = lm(formula = y_test_shifted[-8032] ~ pred)
sum = summary(lm_model)
sum$r.squared

# bootstrap model -----------------------------------------------------------

# On the test without shifted curves
model = load_model_tf("models/model_bootstrap_3")
model %>% evaluate(t(x_test), y_test)

pred <- model %>% predict(t(x_test))
pred = pred %>% as.vector()

df = tibble("Prediction" = pred, "True value" = y_test)
plot_bootstrap = ggplot(data = df, aes(x = Prediction, y = `True value`)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "lm", se = FALSE,
              data = tibble(x = c(-5,885), y = c(-5,885)),
              mapping = aes(x = x, y = y), lwd = 1, color = "red") +
  theme_bw() + 
  labs(title = "Predictions vs true values on original curves",
       subtitle = "Network trained on original curves (bootstrap)")
lm_model = lm(formula = y_test ~ pred)
lm_sum = summary(lm_model)
lm_sum$r.squared

# On the test with shifted curves
model %>% evaluate(t(x_test_shifted[,-8032]), y_test_shifted[-8032])

pred <- model %>% predict(t(x_test_shifted[,-8032]))
pred = pred %>% as.vector()

df = tibble("Prediction" = pred, "True value" = y_test_shifted[-8032])
plot_bootstrap_shifted = ggplot(data = df, aes(x = Prediction, y = `True value`)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "lm", se = FALSE,
              data = tibble(x = c(-110,1200), y = c(-110,1200)),
              mapping = aes(x = x, y = y), lwd = 1, color = "red") +
  theme_bw() + 
  labs(title = "Predictions versus true values on shifted curves",
       subtitle = "Network trained on original curves (bootstrap)")
lm_model = lm(formula = y_test_shifted[-8032] ~ pred)
sum = summary(lm_model)
sum$r.squared



# Shifted model -----------------------------------------------------------

# On the test without shifted curves
model_shifted = load_model_tf("models/model_shifted_5")
model_shifted %>% evaluate(t(x_test), y_test)

pred <- model_shifted %>% predict(t(x_test))
pred = pred %>% as.vector()

df = tibble("Prediction" = pred, "True value" = y_test)

plot_shifted = ggplot(data = df, aes(x = Prediction, y = `True value`)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "lm", se = FALSE,
              data = tibble(x = c(-5,885), y = c(-5,885)),
              mapping = aes(x = x, y = y), lwd = 1, color = "red") +
  theme_bw() + 
  labs(title = "Predictions versus true values on original curves",
       subtitle = "Network trained on shifted curves")
lm_model = lm(formula = y_test ~ pred)
lm_sum = summary(lm_model)
lm_sum$r.squared

# On the test with shifted curves
model_shifted %>% evaluate(t(x_test_shifted[,-8032]), y_test_shifted[-8032])

pred <- model_shifted %>% predict(t(x_test_shifted[,-8032]))
pred = pred %>% as.vector()

df = tibble("Prediction" = pred, "True value" = y_test_shifted[-8032])
plot_shifted_shifted = ggplot(data = df, aes(x = Prediction, y = `True value`)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "lm", se = FALSE,
              data = tibble(x = c(-110,2000), y = c(-110,2000)),
              mapping = aes(x = x, y = y), lwd = 1, color = "red") +
  theme_bw() + 
  labs(title = "Predictions vs true values on shifted curves",
       subtitle = "Network trained on shifted curves")
lm_model = lm(formula = y_test_shifted[-8032] ~ pred)
sum = summary(lm_model)
sum$r.squared


plot_grid(plot_padding, plot_padding_shifted, plot_bootstrap, plot_bootstrap_shifted, plot_shifted, plot_shifted_shifted, nrow = 3)






