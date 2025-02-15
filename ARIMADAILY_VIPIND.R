if (!require("quantmod")) install.packages("quantmod")
library(quantmod)

if (!require("tseries")) install.packages("tseries")
library(tseries)

if (!require("forecast")) install.packages("forecast")
library(forecast)

if (ncol(VIPIND) == 0) {
  stop("No data fetched. Check symbol and internet connection.")
}


getSymbols("VIPIND.NS", src = "yahoo", from = "2020-04-01", to = "2024-03-31")
vipind_data <- get("VIPIND.NS")
returns_vipind <- as.xts(tail(data.frame(vipind_data$VIPIND.NS.Close), -1) / head(data.frame(vipind_data$VIPIND.NS.Close), -1) - 1, order.by=index(vipind_data)[-1])

colnames(returns_vipind) <- "returns_vipind"
returns_vipind <- na.omit(returns_vipind)
returns_vipind


plot(vipind_data$VIPIND.NS.Close, main = "VIPIND Closing Prices", xlab = "Date", ylab = "Close Price")
plot(returns_vipind, main = "VIPIND Daily Returns", xlab = "Date", ylab = "Returns")


adf.test(returns_vipind, alternative = "stationary")


plot(acf(returns_vipind, lag.max = 10), main = "ACF for VIPIND Returns")
plot(pacf(returns_vipind, lag.max = 10), main = "PACF for VIPIND Returns")

auto.arima(returns_vipind, ic="bic")

arima_final_vipind <- arima(returns_vipind, order = c(0,0,0))
summary(arima_final_vipind)


predicted_vipind <- forecast(arima_final_vipind, h = 10)
predicted_vipind

tsdiag(arima_final_vipind)
