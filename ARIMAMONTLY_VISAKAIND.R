
VISAKAIND <- getSymbols.yahoo("VISAKAIND.NS", from = "2020-04-01", to = "2024-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
VISAKAIND<- na.omit(VISAKAIND)
returns_VISAKAIND <- as.xts(tail(data.frame(VISAKAIND$VISAKAIND.NS.Close),-1)/head(data.frame(VISAKAIND$VISAKAIND.NS.Close),-1)-1, frequency = 12)


colnames(returns_VISAKAIND) <- "returns_VISAKAIND"
returns_VISAKAIND <- na.omit(returns_VISAKAIND)
returns_VISAKAIND


plot(VISAKAIND$VISAKAIND.NS.Close, main = "VISAKAIND Monthly Closing Prices", xlab = "Date", ylab = "Close Price")
plot(returns_VISAKAIND, main = "VISAKAIND Monthly Returns", xlab = "Date", ylab = "Returns")


adf.test(returns_VISAKAIND, alternative = "stationary")


plot(acf(returns_VISAKAIND, lag.max = 10), main = "ACF for VISAKAIND Returns")
plot(pacf(returns_VISAKAIND, lag.max = 10), main = "PACF for VISAKAIND Returns")

auto.arima(returns_VISAKAIND, ic="bic")

arima_final_VISAKAIND <- arima(returns_VISAKAIND, order = c(0,0,0))
summary(arima_final_VISAKAIND)


predicted_VISAKAIND <- forecast(arima_final_VISAKAIND, h = 10)
predicted_VISAKAIND

tsdiag(arima_final_VISAKAIND)


