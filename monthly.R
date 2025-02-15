library(readxl)
library(quantmod)
library(xts)
library(tseries)
library(rugarch)
library(rmgarch)

# Market Index Data
NSE <- getSymbols("^NSEI", src = "yahoo", from = "2020-04-01", to = "2024-03-31", auto.assign = FALSE, periodicity = "monthly")
NSE <- na.omit(NSE)

# Symbols for VIPIND, VISAKAIND, VISHNU
library(quantmod)

symbols <- c("VIPIND.NS", "VISAKAIND.NS", "VISHNU.NS")
stocks <- list()

for(symbol in symbols) {
  stocks[[symbol]] <- getSymbols(symbol, src = "yahoo", from = "2020-04-01", to = "2024-03-31", auto.assign = FALSE, periodicity = "monthly")
  stocks[[symbol]] <- na.omit(stocks[[symbol]])
}

names(stocks) <- symbols

# T-Bill Data
D_W_M_T_Bills <- read_excel("D:\\BITS\\3RD YEAR\\SEM 2\\ECON\\FRAM\\M-T-BILLS.xlsx")
D_W_M_T_Bills <- as.data.frame(D_W_M_T_Bills)
D_W_M_T_Billsxts <- xts(D_W_M_T_Bills[,-1], order.by = as.Date(D_W_M_T_Bills$Date))
D_W_M_T_Billsxts <- xts(D_W_M_T_Bills$`T-Bill% (Monthly)`, order.by = as.Date(D_W_M_T_Bills$Date))


exNSE <- as.data.frame(monthlyReturn(NSE$NSEI.Close)[-1, ] - D_W_M_T_Bills$`T-Bill% (Monthly)`)



# Calculate Excess Returns for each stock
excess_returns <- lapply(stocks, function(stock) {
  monthlyReturn(Cl(stock)) - D_W_M_T_Bills$`T-Bill% (Monthly)`
})
names(excess_returns) <- symbols

# Excess Return of the Market
exNSE <- monthlyReturn(Cl(NSE)) - D_W_M_T_Bills$`T-Bill% (Monthly)`






library(readxl)
library(quantmod)
library(xts)
library(tseries)
library(rugarch)
library(rmgarch)

# Load T-Bills data
D_W_M_T_Bills <- read_excel("D:\\BITS\\3RD YEAR\\SEM 2\\ECON\\FRAM\\T-Bills_2024 (2).xlsx")
D_W_M_T_Bills <- as.data.frame(D_W_M_T_Bills)
D_W_M_T_Billsxts <- xts(D_W_M_T_Bills[,-1], order.by = as.Date(D_W_M_T_Bills$Date))

# Data Collection for NSE and Stocks
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2024-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
NSE <- na.omit(NSE)

vipind <- getSymbols.yahoo("VIPIND.NS", from = "2020-04-01", to = "2024-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
vipind <- na.omit(vipind)

visakaind <- getSymbols.yahoo("VISAKAIND.NS", from = "2020-04-01", to = "2024-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
visakaind <- na.omit(visakaind)

vishnu <- getSymbols.yahoo("VISHNU.NS", from = "2020-04-01", to = "2024-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
vishnu <- na.omit(vishnu)

# Excess Returns (Example for one stock, repeat for others)
exNSE <- as.data.frame(monthlyReturn(NSE$NSEI.Close)[-1, ] - D_W_M_T_Bills$`Monthly Yield %`)
exvipind <- as.data.frame(monthlyReturn(vipind$VIPIND.NS.Close)[-1, ] - D_W_M_T_Bills$`Monthly Yield %`)

# Combine and prepare data for CAPM (Example for VIPIND, repeat for others)
exret_vipind <- cbind(exNSE, exvipind)
colnames(exret_vipind) <- c('NSEI.ExcessReturns', 'VIPIND.ExcessReturns')
returns_vipind <- as.xts(exret_vipind)

# CAPM Model for VIPIND (Repeat similarly for VISAKAIND and VISHNU)
regression_vipind <- lm(VIPIND.ExcessReturns ~ NSEI.ExcessReturns, data.frame(returns_vipind[]))
summary(regression_vipind)





