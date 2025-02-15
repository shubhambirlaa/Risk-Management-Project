VIPIND <- getSymbols.yahoo("VIPIND.NS", from = "2020-04-01", to = "2024-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
VIPIND<- na.omit(VIPIND)
returns_VIPIND <- as.xts(tail(data.frame(VIPIND$VIPIND.NS.Close),-1)/head(data.frame(VIPIND$VIPIND.NS.Close),-1)-1, frequency = 12)


colnames(returns_VIPIND) <- "returns_VIPIND"
returns_VIPIND <- na.omit(returns_VIPIND)
returns_VIPIND


plot(VIPIND$VIPIND.NS.Close, main = "VIPIND Monthly Closing Prices", xlab = "Date", ylab = "Close Price")
plot(returns_VIPIND, main = "VIPIND Monthly Returns", xlab = "Date", ylab = "Returns")


adf.test(returns_VIPIND, alternative = "stationary")


plot(acf(returns_VIPIND, lag.max = 10), main = "ACF for VIPIND Returns")
plot(pacf(returns_VIPIND, lag.max = 10), main = "PACF for VIPIND Returns")

auto.arima(returns_VIPIND, ic="bic")

arima_final_VIPIND <- arima(returns_VIPIND, order = c(0,1,1))
summary(arima_final_VIPIND)


predicted_VIPIND <- forecast(arima_final_VIPIND, h = 10)
predicted_VIPIND

tsdiag(arima_final_VIPIND)


