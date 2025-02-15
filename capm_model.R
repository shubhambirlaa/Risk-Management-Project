# Load the necessary library
require(quantmod)

# Set start and end dates
start_date <- "2020-04-01"
end_date <- "2024-03-31"

# Get the data of Nifty 50 and VIPIND Equity
NSE <- getSymbols.yahoo("^NSEI", from = start_date, to = end_date, verbose = FALSE, auto.assign = FALSE)
VIPIND <- getSymbols.yahoo("VIPIND.NS", from = start_date, to = end_date, verbose = FALSE, auto.assign = FALSE)

# Making a data frame of the closing prices
Close <- cbind(NSE$NSEI.Close, VIPIND$VIPIND.NS.Close)

# Calculating the returns
# Ensure that Close is a data.frame object and then calculate returns
Close <- as.data.frame(Close)
Returns <- na.omit(as.xts(ROC(Close)))

# Running the regression model for VIPIND
# Note that we're using the closing prices directly here; adjust as needed if you want to use adjusted prices
regression <- lm(VIPIND.NS.Close ~ NSEI.Close, data = as.data.frame(Returns))
# Slope parameter = beta in CAPM model
summary(regression)
