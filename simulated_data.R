
# packages ----------------------------------------------------------------

library(purrr)
library(Rcpp)
library(tidyverse)
library(tictoc)

# Input data -----------------------------------------------------------

# function simulating Input data
sim_sup_dem = function(lambda, n) { # set lambda = n
  
  lambda_1 <- lambda 
  lambda_2 <- lambda_1 * 100
  lambda_3 <- lambda_1
  n_1 <- n 
  n_2 <- n_1 * 100
  n_3 <- n_1 * 5
  n_4 <- n * 5
  n_5 <- n_4 * 20
  n_6 <- n 
  
  # simulate arrival times (supply)
  arrival_times_1 <- c(0, rexp(rate = lambda_1, n = n_1))
  arrival_times_2 <- c(rexp(rate = lambda_2, n = n_2))
  arrival_times_3 <- c(rexp(rate = lambda_3, n = n_3))
  arrival_times_supply = c(arrival_times_1, arrival_times_2, arrival_times_3)
  
  # simulate arrival times (demand)
  arrival_times_4 <- c(0, rexp(rate = lambda_1, n = n_4))
  arrival_times_5 <- c(rexp(rate = lambda_2, n = n_5))
  arrival_times_6 <- c(rexp(rate = lambda_3, n = n_6))
  arrival_times_demand = c(arrival_times_4, arrival_times_5, arrival_times_6)
  
  path_supply <- cumsum(arrival_times_supply)
  path_demand <- cumsum(arrival_times_demand)
  path_demand <- -path_demand + last(path_supply)
  
  return(list("supply" = path_supply,"demand" = path_demand))
  
}

# simulating input data
set.seed(123)
input_data <- map(.x = rep(10, 8000),
                     .f = sim_sup_dem,
                     lambda = 1)

# normalizing the data (eq 22.1 in NN design and eq 3.35 in Aggarwal)
normalize <- function(input_matrix) {

  min_vec = apply() 
  max_vec =
  
  2 * (x - min(x)) / (max(x) - min(x)) - 1
  
}

# standardization (alternative form of normalization)
input_matrix <-     # transforming from list to matrix
  input_data %>% 
  flatten() %>% 
  unlist() %>% 
  as.vector() %>% 
  matrix(ncol = 8000, byrow = F)

mean_vec <- colMeans(input_matrix)   # mean vector 
sd_vec <- apply(X = input_matrix, MARGIN = 2, FUN = sd) # margin = 1 means over rows
stan_input_data <- scale(input_matrix, center = mean_vec, scale = sd_vec)   # standardizing

# training and test data
index <-
  sample(
    x = 1:2,
    size = ncol(stan_input_data),
    replace = T,
    prob = c(0.9,0.1)
  )

training_input <- stan_input_data[,index == 1]
test_input <- stan_input_data[,index == 2]

  
# plotting simulated demand and supply curves -------------------------------------------------------------------


curves = sim_sup_dem(1,10)
  
ggplot() +
  geom_point(mapping = aes(x = seq(0, length(curves$supply) - 1, 1),
                           y = curves$supply), color = "red") +
  geom_point(mapping = aes(x = seq(0, length(curves$demand) - 1, 1),
  y = curves$demand), color = "blue")

ggplot() +
  geom_point(mapping = aes(x = seq(0, length(input_data_norm[[1]]$supply) - 1, 1),
                           y = input_data_norm[[1]]$supply), color = "red") +
  geom_point(mapping = aes(x = seq(0, length(input_data_norm[[1]]$demand) - 1, 1),
                           y = input_data_norm[[1]]$demand), color = "blue")


# Output data -------------------------------------------------------------

sourceCpp("find_optimum.cpp")

get_mcp <- function(data_matrix,j){
  find_optimum(
    price_supply = data_matrix[1:(length(data_matrix[,j])/2),j],
    quant_supply = rnorm(1061, 1, 0.1),
    price_demand = data_matrix[(length(data_matrix[,j])/2 + 1):length(data_matrix[,j]),j],
    quant_demand = rnorm(1061, 1, 0.1)
  )
}

# set.seed(12345678)
# demand <- tibble(p = runif(1061, 0, 200), q = rnorm(1061, 1, 0.1))
# supply <- tibble(p = runif(1061, 0, 200), q = rnorm(1061, 1, 0.1))
# 
# opt <- find_optimum(supply$p, supply$q, demand$p, demand$q)
# opt
# 
# plot_supply_demand <- function(supply, demand) {
#   supply_df <- supply %>% 
#     bind_rows(data_frame(p = -Inf, q = 0)) %>% 
#     arrange(p) %>% 
#     mutate(agg_q = cumsum(q), side = "supply") %>% 
#     bind_rows(data_frame(p = Inf, q = 0, agg_q = sum(supply$q), side = "supply"))
#   
#   demand_df <- demand %>% 
#     bind_rows(data_frame(p = Inf, q = 0)) %>% 
#     arrange(desc(p)) %>% 
#     mutate(agg_q = cumsum(q), side = "demand") %>% 
#     bind_rows(data_frame(p = -Inf, q = 0, agg_q = sum(demand$q), side = "demand"))
#   
#   ggplot(mapping = aes(x = p, y = agg_q, color = side)) + 
#     geom_step(data = demand_df, direction = "vh") +
#     geom_step(data = supply_df)
# }
# 
# plot_supply_demand(supply, demand) +
#   geom_point(aes(x = opt$price, y = opt$quantity), color = "red", size = 2)

get_mcp <- function(data_matrix,j){
  find_optimum(
    price_supply = data_matrix[1:(length(data_matrix[,j])/2),j],
    quant_supply = rnorm(1061, 1, 0.1),
    price_demand = data_matrix[(length(data_matrix[,j])/2 + 1):length(data_matrix[,j]),j],
    quant_demand = rnorm(1061, 1, 0.1)
  )
}

tic()
stan_output_data <- map(
  .x = 1:6, 
  .f = get_mcp, 
  data_matrix = stan_input_data) %>% unlist() %>% as.vector() %>%  matrix(ncol = 10, byrow = F)
toc()




