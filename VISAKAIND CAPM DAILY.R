# Install and load necessary packages
if (!require("quantmod")) install.packages("quantmod")
library(quantmod)

if (!require("readxl")) install.packages("readxl")
library(readxl)

if (!require("xts")) install.packages("xts")
library(xts)

if (!require("PerformanceAnalytics")) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Getting data for VISAKAIND from Yahoo Finance
VISAKAIND <- getSymbols.yahoo("VISAKAIND.NS", from = "2020-04-01", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE)
VISAKAIND <- na.omit(VISAKAIND)

# Nifty Index as Market
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE)
NSE <- na.omit(NSE)

# T-Bill Data
# Make sure to replace with your actual file path and file name
D_W_M_T_Bills <- read_excel("D:\\BITS\\3RD YEAR\\SEM 2\\ECON\\FRAM\\D-T-BILLS.xlsx")
D_W_M_T_Bills <- as.data.frame(D_W_M_T_Bills)
D_W_M_T_Bills_xts <- xts(D_W_M_T_Bills[,-1], order.by = as.Date(D_W_M_T_Bills$Date))
colnames(D_W_M_T_Bills_xts)
# Excess Returns for VISAKAIND and NSE
exVISAKAIND <- dailyReturn(VISAKAIND$VISAKAIND.NS.Adjusted)[-1, ] - na.locf(D_W_M_T_Bills_xts$`Daily Yield %`)
exNSE <- dailyReturn(NSE$NSEI.Adjusted)[-1, ] - na.locf(D_W_M_T_Bills_xts$`Daily Yield %`)

# Combining the Excess Returns
exret <- merge(exVISAKAIND, exNSE, all = FALSE)
colnames(exret) <- c('VISAKAIND.ExcessReturns','NSEI.ExcessReturns')

# Convert to xts
returns <- as.xts(exret)

# CAPM Model for VISAKAIND
regression <- lm(VISAKAIND.ExcessReturns ~ NSEI.ExcessReturns, data = as.data.frame(returns[]))
summary(regression)
