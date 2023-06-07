# Functions for blotting the bids/asks and the supply/demand curves -------

# Function for plotting the bids/asks
plot_bids_asks = function(df, month, day, hour, side = c("Sell", "Buy")){
  
  if (length(side) == 1) {
    if (side == "Sell") {
      lab = "Supply bids"
      val = "paleturquoise3"
      title = "Supply bids for 2022-"
    } else if (side == "Buy") {
      lab = "Demand bids"
      val = "maroon"
      title = "Demand bids for 2022-"
    }
  } else {lab = c("Demand bids", "Supply bids")
  val = c("maroon", "paleturquoise3")
  title = "Supply and demand bids for 2022-"}
  
  data = get_daily_data(df, month, day, hour, side)
  
  p = ggplot(data = data, mapping = aes(x = Quantity, y = Price, color = Side)) +
    geom_point() + 
    theme_bw() +
    theme(legend.position = c(.88, .87),
         legend.background = element_blank(),
         legend.title = element_blank()) +
    scale_color_manual(labels = lab, values = val) +
    labs(title = paste0(title, sprintf("%0.2d", month), "-", sprintf("%0.2d", day), " hour ", hour))
  return(p)
}

# Fucntion for plotting the supply/demand curves
plot_supply_demand = function(df, month, day, hour){
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
  
  p = ggplot(mapping = aes(x = agg_q, y = Price, color = Side)) +
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
    coord_cartesian(#xlim = c(30200,30600), ylim = c(40,47)
                      c(min(c(min(df_supply$agg_q), min(df_demand$agg_q))),
                        max(c(max(df_supply$agg_q), max(df_demand$agg_q))))
                    ) +
    theme_bw() +
    theme(legend.position = c(.89, .87),
          legend.background = element_blank(),
          legend.title = element_blank()) +
    scale_color_manual(labels = c("Demand", "Supply"), values = c("maroon", "paleturquoise3")) +
    xlab("Quantity") + 
    labs(title = paste0("Supply and demand curves for 2022-", sprintf('%0.2d', month), "-", sprintf('%0.2d', day), " hour ", hour))
  
  return(p)
}

# Function for plotting supply curve
plot_supply = function(df, month, day, hour){
  
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
  
  p = ggplot(mapping = aes(x = agg_q, y = Price, color = Side)) +
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
    coord_cartesian(xlim = c(min(df_supply$agg_q), max(df_supply$agg_q)+5000)) +
    theme_bw() +
    theme(legend.position = c(.89, .87),
          legend.background = element_blank(),
          legend.title = element_blank()) +
    scale_color_manual(labels = "Supply", values = "paleturquoise3") +
    xlab("Quantity") + 
    labs(title = paste0("Supply curve for 2022-", sprintf('%0.2d', month), "-", sprintf('%0.2d', day), " hour ", hour))
  
  return(p)
}

# Function for plotting demand curve
plot_demand = function(df, month, day, hour){
  
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
  
  p = ggplot(mapping = aes(x = agg_q, y = Price, color = Side)) +
    geom_point(data = df_demand) +
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
    coord_cartesian(xlim = c(min(df_demand$agg_q), max(df_demand$agg_q))) +
    theme_bw() +
    theme(legend.position = c(.89, .87),
          legend.background = element_blank(),
          legend.title = element_blank()) +
    scale_color_manual(labels = c("Demand"), values = c("maroon")) +
    xlab("Quantity") + 
    labs(title = paste0("Demand curve for 2022-", sprintf('%0.2d', month), "-", sprintf('%0.2d', day), " hour ", hour))
  
 
  return(p)
}




