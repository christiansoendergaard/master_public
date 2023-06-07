# What I want: 
# 1. A function for scenario 1. This function should take month, x_data, y_data as input. As output I would like 
# plot comparing with the true prices and the targeted price, as well as the epsilons, MAE, % of stabilized closer to the target
# price than the true price.
# 2. A function for scenario 2. This function should take day (after jan 1st), x_data, y_data as input. Then, in this function
# the 2-step optimization is performed and a SARIMA is fitted and a one day-ahead forecast is performed. The output should be 
# plot of the forecast compared to the true series, plot of the price stabilization, MAE and % of stabilized closer to the target
# price than the true price. 

# Fitting a SARIMA model to forecast the prices
library(forecast)
library(rugarch)
library(fGarch)
library(cowplot)

ggAcf(diff(diff(output), 24), lag.max = 100) + theme_bw()
ggPacf(diff(diff(output), 24), lag.max = 100) + theme_bw()
ggAcf(output2, lag.max = 100)
# The ACF tails off and the PACF cuts off at lag two. This indicates an AR(2) model
# Seasonal component: ACF cuts off at lag 24 and PACF tails off. This indicates MA(1) with seasonal differencing of 24.
output2 = output + 100
output2 = log(output2)
tic()
fit1 = arima(x = output, order = c(4,0,5), seasonal = list(order = c(5, 1, 1), period = 24))
toc()
forecast::checkresiduals(fit1)

tic()
sarima = forecast::auto.arima(y = ts(output, frequency = 24), stepwise = FALSE, trace = TRUE, seasonal = TRUE,
                              nmodels = 1000, ic = "bic", max.P = 5, max.Q = 5, max.p = 5, max.q = 5, 
                              max.order = 20, d = 0, D = 1)
toc()
#(0,1,0)(5,1,1)
forecast(sarima, h = 24, level = 95) %>% autoplot(xlim = c(300,330))

#
autoplot(ts(output, frequency = 24), xlab = "Days after January 1st", ylab = "Price", main = "Hourly MCPs") + theme_bw()
acf = ggAcf(ts(output, frequency = 24)) + 
  theme_bw() + 
  labs(title = "ACF of the series")
pacf = ggPacf(ts(output, frequency = 24)) + 
  theme_bw() +
  labs(title = "PACF of the series")
plot_grid(acf, pacf)

# The plots in the appendix -----------------------------------------------

acf_diff = ggAcf(ts(output, frequency = 24) %>% diff()) + 
  theme_bw() + 
  labs(title = "ACF of the first differenced series")
pacf_diff = ggPacf(ts(output, frequency = 24) %>% diff()) +
  theme_bw() +
  labs(title = "PACF of the first differenced series")
plot_grid(acf_diff, pacf_diff)

acf_diff24 = ggAcf((ts(output, frequency = 24) %>% diff()) %>% diff(24)) +
  theme_bw() + 
  labs(title = "ACF of the first and seasonal differenced series")
pacf_diff24 = ggPacf((ts(output, frequency = 24) %>% diff())) +
  theme_bw() +
  labs(title = "PACF of the first and seasonal differenced series")
plot_grid(acf_diff24, pacf_diff24)





# residuals = fit1$residuals
# std_res = residuals / sd(residuals)
# 
# spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
#            mean.model = list(armaOrder = c(0,0)),
#            distribution.model = "std")
# garch_fit = ugarchfit(spec = spec, data = residuals)
# garch_fit@fit$z
# 
# ugarchforecast(fitORspec = garch_fit, n.ahead = 24)

