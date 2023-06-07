# packages
library(tidyverse)
library(R.utils)
library(tictoc)
library(dplyr)

df = readRDS(file = "RDS/bid_ask_data")
shifted_data = readRDS(file = "RDS/shifted_data")

sourceDirectory("functions", modifiedOnly = FALSE)

# Function for plotting the supply/demand curves
plot_supply_demand_shifted = function(df, shifted_data, month, day, hour, a){ 
  
  # --- original demand curve ---
  
  df_demand = get_daily_data(df, month, day, hour, side = "Buy") %>%
    arrange(desc(Price)) %>%
    mutate(agg_q = cumsum(Quantity), Side = "Buy")
  
  # Dataframe to create the end lines
  ends_demand = data.frame(
    agg_q = c(0, max(df_demand$agg_q)),
    agg_q_end = c(min(df_demand$agg_q), 2 * max(df_demand$agg_q)),
    Price = c(max(df_demand$Price), min(df_demand$Price)),
    Price_end = c(max(df_demand$Price), min(df_demand$Price)),
    Side = c("Buy", "Buy")
  )
  
  # --- original supply curve ---
  
  df_supply =  get_daily_data(df, month, day, hour, side = "Sell") %>%
    arrange(Price) %>%
    mutate(agg_q = cumsum(Quantity), Side = "Sell")
  
  # Dataframe to create the end lines
  ends_supply = data.frame(
    agg_q = c(0, max(df_supply$agg_q)),
    agg_q_end = c(min(df_supply$agg_q), 2 * max(df_supply$agg_q)),
    Price = c(min(df_supply$Price), max(df_supply$Price)),
    Price_end = c(min(df_supply$Price), max(df_supply$Price)),
    Side = c("Sell", "Sell")
  )
  
  # --- shifted curves ---

  date = paste0(
    "2022-",
    sprintf('%0.2d', month),
    "-",
    sprintf('%0.2d', day),
    " hour ",
    sprintf('%0.2d', hour)
  )

  df_shifted = map(
    .x = 1:10,
    .f = function(x){
      tibble(
        agg_q = get(paste0("quantity_ask_shift", x) , get(date, shifted_data)),
        Price = get("price_ask" , get(date, shifted_data)),
        Side = rep("Shifted", length(get("price_ask" , get(date, shifted_data))))
      )
    }
  )

  ends_shifted = map(
    .x = 1:10,
    .f = function(x){
      tibble(
        agg_q = c(0, max(df_shifted[[x]]$agg_q)),                                    # agg_q
        agg_q_end = c(min(df_shifted[[x]]$agg_q), 2 * max(df_shifted[[x]]$agg_q)),   # agg_q_end
        Price = c(min(df_supply$Price), max(df_supply$Price)),                       # Price
        Price_end = c(min(df_supply$Price), max(df_supply$Price)),                   # Price_end
        Side = c("Shifted", "Shifted")
      )
    }
  )
  
  p = ggplot(mapping = aes(x = agg_q, y = Price, color = Side)) +
    geom_point(data = df_shifted[[1]], alpha = a) +
    geom_segment(data = df_shifted[[1]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[1]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[1]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[2]], alpha = a) +
    geom_segment(data = df_shifted[[2]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[2]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[2]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[3]], alpha = a) +
    geom_segment(data = df_shifted[[3]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[3]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[3]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[4]], alpha = a) +
    geom_segment(data = df_shifted[[4]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[4]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[4]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[5]], alpha = a) +
    geom_segment(data = df_shifted[[5]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[5]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[5]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[6]], alpha = a) +
    geom_segment(data = df_shifted[[6]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[6]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[6]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[7]], alpha = a) +
    geom_segment(data = df_shifted[[7]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[7]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[7]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[8]], alpha = a) +
    geom_segment(data = df_shifted[[8]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[8]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[8]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[9]], alpha = a) +
    geom_segment(data = df_shifted[[9]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[9]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[9]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_shifted[[10]], alpha = a) +
    geom_segment(data = df_shifted[[10]], 
                 aes(xend = c(agg_q[2:nrow(df_shifted[[10]])], NA), 
                     yend = c(Price)), alpha = a
    ) +
    geom_segment(data = ends_shifted[[10]],
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end), alpha = a
    ) +
    geom_point(data = df_demand) +
    geom_point(data = df_supply) +
    geom_segment(data = df_supply, 
                 aes(xend = c(agg_q[2:nrow(df_supply)], NA), 
                     yend = c(Price))
    ) +
    geom_segment(data = ends_supply,
                 aes(x = agg_q,
                     y = Price,
                     xend = agg_q_end,
                     yend = Price_end)
    ) +
    geom_segment(data = df_demand, 
                 aes(xend = c(agg_q[2:nrow(df_demand)], last(agg_q)), 
                     yend = Price)
    ) +
    geom_segment(data = ends_demand,
                 aes(x = agg_q,
                     y = Price, 
                     xend = agg_q_end,
                     yend = Price_end)
    ) +
    coord_cartesian(xlim =
                      c(min(c(min(df_supply$agg_q), min(df_demand$agg_q))),
                        max(c(max(df_shifted[[10]]$agg_q), max(df_demand$agg_q))))
    ) +
    theme_bw() +
    theme(legend.position = c(.89, .85),
          legend.background = element_blank(),
          legend.title = element_blank()) +
    scale_color_manual(labels = c("Demand", "Supply", "Shifted supply"), values = c("maroon", "steelblue4", "palegreen3")) +
    xlab("Quantity") + 
    labs(title = paste0("Supply and demand curves for 2022-", sprintf('%0.2d', month), "-", sprintf('%0.2d', day), " hour ", hour))
  
  return(p)
}

plot_supply_demand_shifted(df = df,
                           shifted_data = shifted_data,
                           month = 3, 
                           day = 5,
                           hour = 7, 
                           a = 0.5)
