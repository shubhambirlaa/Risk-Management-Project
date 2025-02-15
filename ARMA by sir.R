# Install and load necessary packages
if (!require("quantmod")) install.packages("quantmod")
library(quantmod)

if (!require("tseries")) install.packages("tseries")
library(tseries)

if (!require("forecast")) install.packages("forecast")
library(forecast)

if (ncol(VIPIND) == 0) {
  stop("No data fetched. Check symbol and internet connection.")
}

# Calculating the returns
# Extract the Adjusted column. If Adjusted prices are not available, use the Close prices.
price_col <- ifelse("VIPIND.NS.Adjusted" %in% colnames(VIPIND), "VIPIND.NS.Adjusted", "VIPIND.NS.Close")
Returns_VIPIND <- na.omit(as.xts(tail(VIPIND[, price_col], -1) / head(VIPIND[, price_col], -1) - 1, order.by=index(VIPIND)[-1]))

# Set the column name for returns
colnames(Returns_VIPIND) <- "Returns"

# Setting options to not convert retrieved data to xts object automatically
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

# Getting data for VIPIND from Yahoo Finance
VIPIND <- getSymbols("VIPIND.NS", src = "yahoo", from = "2020-04-01", to = "2024-03-31", auto.assign = TRUE, warnings = FALSE)

# Converting the object to a data frame and viewing the first few rows
VIPIND_DF <- data.frame(Date=index(VIPIND), coredata(VIPIND))
head(VIPIND_DF)

# Calculating the Returns for VIPIND
Returns_VIPIND <- na.omit(as.xts(tail(VIPIND[, "VIPIND.NS.Adjusted"], -1) / 
                                   head(VIPIND[, "VIPIND.NS.Adjusted"], -1) - 1,
                                 order.by=index(VIPIND)[-1]))

colnames(Returns_VIPIND) <- "Returns"

# Visualizing the adjusted closing prices and returns
chartSeries(VIPIND[, "VIPIND.NS.Adjusted"], name = "VIPIND Adjusted Closing Prices")
chartSeries(Returns_VIPIND, name = "VIPIND Daily Returns")

# Identifying the model with ADF test
adf.test(Returns_VIPIND, alternative = "stationary")

# ACF and PACF plots for determining order of AR and MA terms
acf(Returns_VIPIND, lag.max = 10, main = "ACF for VIPIND Returns")
pacf(Returns_VIPIND, lag.max = 10, main = "PACF for VIPIND Returns")

# Fitting an ARIMA model based on ACF and PACF plots
# For this example, I will use an ARIMA(0,0,1) as per your previous indication,
# but you should adjust it based on actual ACF and PACF plots
arima_final_VIPIND <- arima(Returns_VIPIND, order = c(0,0,1))

# Predicting using this model
predicted_VIPIND <- predict(arima_final_VIPIND, n.ahead = 10)
predicted_VIPIND

# Diagnosis of the model
tsdiag(arima_final_VIPIND)
