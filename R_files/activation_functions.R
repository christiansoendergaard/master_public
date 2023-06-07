
# packages ----------------------------------------------------------------

library(ggplot2)
library(ggpubr)
library(Unicode)
library(cowplot)

# activation function -----------------------------------

# Identity activation funtion
identity_act_fun <- ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(title = "Identity activation function", x = "u", y = "\U03F1(u)") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0), limits = c(-1,1)) +
  scale_y_continuous(expand = c(0,0), limits = c(-1,1), labels = scales::label_number(accuracy = 0.01)) +
  stat_function(fun = function(u) u , size = 1) + 
  theme(plot.title = element_text(size = 10))
  
  
# Sign activation function
sign_act_fun <- ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), limits = c(-1,1)) +
  scale_y_continuous(expand = c(0.05,0), limits = c(-1,1), labels = scales::label_number(accuracy = 0.01)) + 
  labs(title = "Sign activation function", x = "u", y = "\U03F1(u)") +
  theme_bw() +
  geom_segment(aes(x = -1, y = -1, xend = 0, yend = -1), size = 1) +
  geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), size = 1) +
  geom_point(aes(x = 0,y = -1), size = 2) +
  geom_point(aes(x = 0,y = 1), size = 2, shape = 21, fill = "white") + 
  theme(plot.title = element_text(size = 10))

# Sigmoid activation function
sigmoid_act_fun <- ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), limits = c(-5,5)) +
  scale_y_continuous(expand = c(0.05,0), limits = c(0,1), labels = scales::label_number(accuracy = 0.01)) +
  labs(title = "Sigmoid activation function", x = "u", y = "\U03F1(u)") +
  theme_bw() +
  stat_function(fun = function(u) {1 / (1 + exp(-u))} , size = 1) + 
  theme(plot.title = element_text(size = 10))
  
# TanH activation function
tanh_act_fun <- ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), limits = c(-5,5)) +
  scale_y_continuous(expand = c(0.05,0), limits = c(-1,1), labels = scales::label_number(accuracy = 0.01)) + 
  labs(title = "Tanh activation function", x = "u", y = "\U03F1(u)") +
  theme_bw() +
  stat_function(fun = function(u) {(exp(2*u) - 1) / (exp(2*u) + 1)} , size = 1) + 
  theme(plot.title = element_text(size = 10))

# ReLU activation function
relu_act_fun <- ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), limits = c(-5,5)) +
  scale_y_continuous(expand = c(0.05,0), limits = c(0,5), labels = scales::label_number(accuracy = 0.01)) + 
  labs(title = "ReLU activation function", x = "u", y = "\U03F1(u)") +
  theme_bw() +
  stat_function(fun = function(u) {pmax(0,u)} , size = 1) + 
  theme(plot.title = element_text(size = 10))

# Hard TanH activation function
htanh_act_fun <- ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), limits = c(-2,2)) +
  scale_y_continuous(expand = c(0.05,0), limits = c(-1,1), labels = scales::label_number(accuracy = 0.01)) + 
  labs(title = "Hard tanh activation function", x = "u", y = "\U03F1(u)") +
  theme_bw() +
  stat_function(fun = function(u) {pmax(pmin(u,1), -1)} , size = 1) + 
  theme(plot.title = element_text(size = 10))

# ELU activation function
elu_act_fun <- ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), limits = c(-5,5)) +
  scale_y_continuous(expand = c(0.05,0), limits = c(-1,5), labels = scales::label_number(accuracy = 0.01)) +
  labs(title = "ELU activation function", x = "u", y = "\U03F1(u)") +
  theme_bw() +
  stat_function(fun = function(u) {ifelse(u < 0, exp(u) -1, u)} , size = 1) + 
  theme(plot.title = element_text(size = 10))



# Arranging all plots into one figure -------------------------------------

all_act_fun <- plot_grid(
  identity_act_fun, sign_act_fun, sigmoid_act_fun,
  tanh_act_fun, relu_act_fun, htanh_act_fun,
  ncol = 2,
  nrow = 3
)

bottom_row = plot_grid(NULL, elu_act_fun, NULL, ncol=3, rel_widths=c(0.25,0.5,0.25))

collect_all = plot_grid(all_act_fun, bottom_row, ncol = 1, rel_heights = c(3,1))

# save it manually with height = width = 10 inches






