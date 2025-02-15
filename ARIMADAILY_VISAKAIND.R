VISAKAIND

if (!require("quantmod")) install.packages("quantmod")
library(quantmod)

if (!require("tseries")) install.packages("tseries")
library(tseries)

if (!require("forecast")) install.packages("forecast")
library(forecast)

if (ncol(VIPIND) == 0) {
  stop("No data fetched. Check symbol and internet connection.")
}


getSymbols("VISAKAIND.NS", src = "yahoo", from = "2020-04-01", to = "2024-03-31")
VISAKAIND_data <- get("VISAKAIND.NS")
returns_VISAKAIND <- as.xts(tail(data.frame(VISAKAIND_data$VISAKAIND.NS.Close), -1) / head(data.frame(VISAKAIND_data$VISAKAIND.NS.Close), -1) - 1, order.by=index(VISAKAIND_data)[-1])

colnames(returns_VISAKAIND) <- "returns_VISAKAIND"
returns_VISAKAIND <- na.omit(returns_VISAKAIND)
returns_VISAKAIND


plot(VISAKAIND_data$VISAKAIND.NS.Close, main = "VISAKAIND Closing Prices", xlab = "Date", ylab = "Close Price")
plot(returns_VISAKAIND, main = "VISAKAIND Daily Returns", xlab = "Date", ylab = "Returns")


adf.test(returns_VISAKAIND, alternative = "stationary")


plot(acf(returns_VISAKAIND, lag.max = 10), main = "ACF for VISAKAIND Returns")
plot(pacf(returns_VISAKAIND, lag.max = 10), main = "PACF for VISAKAIND Returns")

auto.arima(returns_VISAKAIND, ic="bic")

arima_final_VISAKAIND <- arima(returns_VISAKAIND, order = c(3,1,0))
summary(arima_final_VISAKAIND)


predicted_VISAKAIND <- forecast(arima_final_VISAKAIND, h = 10)
predicted_VISAKAIND

tsdiag(arima_final_VISAKAIND)


