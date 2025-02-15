VISHNU <- getSymbols.yahoo("VISHNU.NS", from = "2020-04-01", to = "2024-03-31", verbose = F, auto.assign = F, periodicity = "weekly")

returns_VISHNU <- as.xts(tail(data.frame(VISHNU$VISHNU.NS.Close),-1)/head(data.frame(VISHNU$VISHNU.NS.Close),-1)-1, frequency = 52)


colnames(returns_VISHNU) <- "returns_VISHNU"
returns_VISHNU <- na.omit(returns_VISHNU)
returns_VISHNU


plot(VISHNU_data$VISHNU.NS.Close, main = "VISHNU weekly Closing Prices", xlab = "Date", ylab = "Close Price")
plot(returns_VISHNU, main = "VISHNU Weekly Returns", xlab = "Date", ylab = "Returns")


adf.test(returns_VISHNU, alternative = "stationary")


plot(acf(returns_VISHNU, lag.max = 10), main = "ACF for VISHNU Returns")
plot(pacf(returns_VISHNU, lag.max = 10), main = "PACF for VISHNU Returns")

auto.arima(returns_VISHNU, ic="bic")

arima_final_VISHNU <- arima(returns_VISHNU, order = c(1,1,0))
summary(arima_final_VISHNU)


predicted_VISHNU <- forecast(arima_final_VISHNU, h = 10)
predicted_VISHNU

tsdiag(arima_final_VISHNU)


