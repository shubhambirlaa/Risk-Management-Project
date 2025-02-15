# Install and load necessary packages
if (!require("quantmod")) install.packages("quantmod")
library(quantmod)

if (!require("readxl")) install.packages("readxl")
library(readxl)

if (!require("xts")) install.packages("xts")
library(xts)

if (!require("PerformanceAnalytics")) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Getting data for VIPIND from Yahoo Finance
VIPIND <- getSymbols.yahoo("VIPIND.NS", from = "2020-04-01", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE,periodicity = "weekly")
VIPIND <- na.omit(VIPIND)

# Nifty Index as Market
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE,periodicity = "weekly")
NSE <- na.omit(NSE)

# T-Bill Data
# Make sure to replace with your actual file path and file name
D_W_M_T_Bills <- read_excel("D:\\BITS\\3RD YEAR\\SEM 2\\ECON\\FRAM\\W-T-BILLS.xlsx")
colnames(D_W_M_T_Bills)
D_W_M_T_Bills <- as.data.frame(D_W_M_T_Bills)
colnames(D_W_M_T_Bills)
D_W_M_T_Bills_xts <- xts(D_W_M_T_Bills[,-1], order.by = as.Date(D_W_M_T_Bills$Date))
colnames(D_W_M_T_Bills_xts)

# Excess Returns for VIPIND and NSE
exVIPIND <- dailyReturn(VIPIND$VIPIND.NS.Adjusted)[-1, ] - na.locf(D_W_M_T_Bills_xts$`Weekly Yield %`)
exNSE <- dailyReturn(NSE$NSEI.Adjusted)[-1, ] - na.locf(D_W_M_T_Bills_xts$`Weekly Yield %`)

# Combining the Excess Returns
exret <- merge(exVIPIND, exNSE, all = FALSE)
colnames(exret) <- c('VIPIND.ExcessReturns','NSEI.ExcessReturns')

# Convert to xts
returns <- as.xts(exret)

# CAPM Model for VIPIND
regression <- lm(VIPIND.ExcessReturns ~ NSEI.ExcessReturns, data = as.data.frame(returns[]))
summary(regression)

# Note: Replace "/path/to/your/D-T-Bills.xlsx" with the actual path to your T-Bill data file.


VISAKAIND <- getSymbols.yahoo("VISAKAIND.NS", from = "2020-04-01", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE, periodicity = "weekly")
VISAKAIND <- na.omit(VISAKAIND)
exVISAKAIND <- dailyReturn(VISAKAIND$VISAKAIND.NS.Adjusted)[-1, ] - na.locf(D_W_M_T_Bills_xts$`Weekly Yield %`)
exNSE <- dailyReturn(NSE$NSEI.Adjusted)[-1, ] - na.locf(D_W_M_T_Bills_xts$`Weekly Yield %`)
exret <- merge(exVISAKAIND, exNSE, all = FALSE)
colnames(exret) <- c('VISAKAIND.ExcessReturns','NSEI.ExcessReturns')

# Convert to xts
returns <- as.xts(exret)

# CAPM Model for VISAKAIND
regression <- lm(VISAKAIND.ExcessReturns ~ NSEI.ExcessReturns, data = as.data.frame(returns[]))
summary(regression)



VISHNU <- getSymbols.yahoo("VISHNU.NS", from = "2020-04-01", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE, periodicity = "weekly")
VISHNU <- na.omit(VISHNU)
exVISHNU <- dailyReturn(VISHNU$VISHNU.NS.Adjusted)[-1, ] - na.locf(D_W_M_T_Bills_xts$`Weekly Yield %`)
exNSE <- dailyReturn(NSE$NSEI.Adjusted)[-1, ] - na.locf(D_W_M_T_Bills_xts$`Weekly Yield %`)

# Combining the Excess Returns
exret <- merge(exVISHNU, exNSE, all = FALSE)
colnames(exret) <- c('VISHNU.ExcessReturns','NSEI.ExcessReturns')

# Convert to xts
returns <- as.xts(exret)

# CAPM Model for VISHNU
regression <- lm(VISHNU.ExcessReturns ~ NSEI.ExcessReturns, data = as.data.frame(returns[]))
summary(regression)