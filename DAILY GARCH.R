library(quantmod)
library(TTR)
library(rugarch)

# Data Collection for VIPIND, VISAKAIND, and VISHNU
vipind <- getSymbols("VIPIND.NS", from = "2020-04-01", to = "2024-03-31", periodicity = "daily", auto.assign = FALSE)
visakaind <- getSymbols("VISAKAIND.NS", from ="2020-04-01", to = "2024-03-31", periodicity = "daily", auto.assign = FALSE) 
vishnu <- getSymbols("VISHNU.NS", from ="2020-04-01", to = "2024-03-31", periodicity = "daily", auto.assign = FALSE) 

# Assuming you meant displaying the head for each data frame
head(vipind)
head(visakaind)
head(vishnu)

# Return Calculation
rvipind <- dailyReturn(vipind)
rvisakaind <- dailyReturn(visakaind)
rvishnu <- dailyReturn(vishnu)

# Implementing Univariate GARCH
ug_spec = ugarchspec()
ug_spec
# Implementing EGARCH
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))

# Estimating the models for each stock
ugfit_vipind = ugarchfit(spec = ug_spec, data = rvipind) 
ugfit_visakaind = ugarchfit(spec = ug_spec, data = rvisakaind) 
ugfit_vishnu = ugarchfit(spec = ug_spec, data = rvishnu) 

egfit_vipind = ugarchfit(spec = eg_spec, data = rvipind)
egfit_visakaind = ugarchfit(spec = eg_spec, data = rvisakaind)
egfit_vishnu = ugarchfit(spec = eg_spec, data = rvishnu)

ugfit_vipind 
ugfit_visakaind
ugfit_vishnu 

egfit_vipind 
egfit_visakaind
egfit_vishnu 

# Forecasting
ugforecast_vipind = ugarchforecast(ugfit_vipind, n.ahead=10)
ugforecast_visakaind = ugarchforecast(ugfit_visakaind, n.ahead=10) 
ugforecast_vishnu = ugarchforecast(ugfit_vishnu, n.ahead=10) 

egforecast_vipind = ugarchforecast(egfit_vipind, n.ahead=10) 
egforecast_visakaind = ugarchforecast(egfit_visakaind, n.ahead=10) 
egforecast_vishnu = ugarchforecast(egfit_vishnu, n.ahead=10) 


ugforecast_vipind 
ugforecast_visakaind
ugforecast_vishnu 

egforecast_vipind 
egforecast_visakaind 
egforecast_vishnu 
