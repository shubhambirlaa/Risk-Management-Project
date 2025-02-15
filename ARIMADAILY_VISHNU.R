VISHNU

if (!require("quantmod")) install.packages("quantmod")
library(quantmod)

if (!require("tseries")) install.packages("tseries")
library(tseries)

if (!require("forecast")) install.packages("forecast")
library(forecast)

if (ncol(VIPIND) == 0) {
  stop("No data fetched. Check symbol and internet connection.")
}


getSymbols("VISHNU.NS", src = "yahoo", from = "2020-04-01", to = "2024-03-31")
VISHNU_data <- get("VISHNU.NS")
returns_VISHNU <- as.xts(tail(data.frame(VISHNU_data$VISHNU.NS.Close), -1) / head(data.frame(VISHNU_data$VISHNU.NS.Close), -1) - 1, order.by=index(VISHNU_data)[-1])

colnames(returns_VISHNU) <- "returns_VISHNU"
returns_VISHNU <- na.omit(returns_VISHNU)
returns_VISHNU


plot(VISHNU_data$VISHNU.NS.Close, main = "VISHNU Closing Prices", xlab = "Date", ylab = "Close Price")
plot(returns_VISHNU, main = "VISHNU Daily Returns", xlab = "Date", ylab = "Returns")


adf.test(returns_VISHNU, alternative = "stationary")


plot(acf(returns_VISHNU, lag.max = 10), main = "ACF for VISHNU Returns")
plot(pacf(returns_VISHNU, lag.max = 10), main = "PACF for VISHNU Returns")

auto.arima(returns_VISHNU, ic="bic")

arima_final_VISHNU <- arima(returns_VISHNU, order = c(1,1,0))
summary(arima_final_VISHNU)


predicted_VISHNU <- forecast(arima_final_VISHNU, h = 10)
predicted_VISHNU

tsdiag(arima_final_VISHNU)


