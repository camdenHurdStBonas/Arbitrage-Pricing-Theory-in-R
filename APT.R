APT <- function(ticker, start_date, end_date, FRED_API_Key){
  # Load the libraries
  library(quantmod)
  library(fredr)
  library(tidyverse)
  library(lubridate)
  library(fredr)
  #turn on the WHITESTRAP package; note that this is a user-created command and MUST BE CITED
  library(whitestrap)
  #turn on lmtest; note that this is a user-created command and MUST BE CITED
  library(lmtest)
  #turn on car; note that this is a user-created command and MUST BE CITED
  library(car)
  
  # Fetch stock stock prices from Yahoo Finance
  stock_data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
  # Convert the Adjusted Close column to monthly data
  stock_monthly <- to.monthly(Ad(get(stock_data)), indexAt = "firstof", OHLC = FALSE)
  # Create a data frame with monthly adjusted close prices
  stock_stock <- data.frame(
    date = index(stock_monthly),               # Monthly dates
    price = as.numeric(stock_monthly)          # Monthly adjusted close price
  ) %>%
    rename(stock_Price = price)                # Rename price column
  
  # Fetch stock stock prices from Yahoo Finance
  getSymbols("SPY", src = "yahoo", from = start_date, to = end_date)
  # Convert the Adjusted Close column to monthly data
  spy_monthly <- to.monthly(SPY$SPY.Adjusted, indexAt = "firstof", OHLC = FALSE)
  # Create a data frame with monthly adjusted close prices
  spy_etf <- data.frame(
    date = index(spy_monthly),               # Monthly dates
    price = as.numeric(spy_monthly)          # Monthly adjusted close price
  ) %>%
    rename(SPY_Price = price)                # Rename price column
  
  # Fetch stock stock prices from Yahoo Finance
  getSymbols("IWM", src = "yahoo", from = start_date, to = end_date)
  # Convert the Adjusted Close column to monthly data
  iwm_monthly <- to.monthly(IWM$IWM.Adjusted, indexAt = "firstof", OHLC = FALSE)
  # Create a data frame with monthly adjusted close prices
  iwm_etf <- data.frame(
    date = index(iwm_monthly),               # Monthly dates
    price = as.numeric(iwm_monthly)          # Monthly adjusted close price
  ) %>%
    rename(IWM_Price = price)                # Rename price column
  
  # Fetch stock stock prices from Yahoo Finance
  getSymbols("IWB", src = "yahoo", from = start_date, to = end_date)
  # Convert the Adjusted Close column to monthly data
  iwb_monthly <- to.monthly(IWB$IWB.Adjusted, indexAt = "firstof", OHLC = FALSE)
  # Create a data frame with monthly adjusted close prices
  iwb_etf <- data.frame(
    date = index(iwb_monthly),               # Monthly dates
    price = as.numeric(iwb_monthly)          # Monthly adjusted close price
  ) %>%
    rename(IWB_Price = price)                # Rename price column
  
  # Fetch stock stock prices from Yahoo Finance
  getSymbols("IWD", src = "yahoo", from = start_date, to = end_date)
  # Convert the Adjusted Close column to monthly data
  iwd_monthly <- to.monthly(IWD$IWD.Adjusted, indexAt = "firstof", OHLC = FALSE)
  # Create a data frame with monthly adjusted close prices
  iwd_etf <- data.frame(
    date = index(iwd_monthly),               # Monthly dates
    price = as.numeric(iwd_monthly)          # Monthly adjusted close price
  ) %>%
    rename(IWD_Price = price)                # Rename price column
  
  # Fetch stock stock prices from Yahoo Finance
  getSymbols("IWF", src = "yahoo", from = start_date, to = end_date)
  # Convert the Adjusted Close column to monthly data
  iwf_monthly <- to.monthly(IWF$IWF.Adjusted, indexAt = "firstof", OHLC = FALSE)
  # Create a data frame with monthly adjusted close prices
  iwf_etf <- data.frame(
    date = index(iwf_monthly),               # Monthly dates
    price = as.numeric(iwf_monthly)          # Monthly adjusted close price
  ) %>%
    rename(IWF_Price = price)                # Rename price column
  
  # Fetch stock stock prices from Yahoo Finance
  getSymbols("VIG", src = "yahoo", from = start_date, to = end_date)
  # Convert the Adjusted Close column to monthly data
  vig_monthly <- to.monthly(VIG$VIG.Adjusted, indexAt = "firstof", OHLC = FALSE)
  # Create a data frame with monthly adjusted close prices
  vig_etf <- data.frame(
    date = index(vig_monthly),               # Monthly dates
    price = as.numeric(vig_monthly)          # Monthly adjusted close price
  ) %>%
    rename(VIG_Price = price)                # Rename price column
  
  # Fetch stock stock prices from Yahoo Finance
  getSymbols("QQQ", src = "yahoo", from = start_date, to = end_date)
  # Convert the Adjusted Close column to monthly data
  qqq_monthly <- to.monthly(QQQ$QQQ.Adjusted, indexAt = "firstof", OHLC = FALSE)
  # Create a data frame with monthly adjusted close prices
  qqq_etf <- data.frame(
    date = index(qqq_monthly),               # Monthly dates
    price = as.numeric(qqq_monthly)          # Monthly adjusted close price
  ) %>%
    rename(QQQ_Price = price)                # Rename price column
  
  fredr_set_key(FRED_API_Key)
  fredr_get_key()
  
  # Fetch Federal Funds Effective Rate from FRED
  ffr_data <- fredr(series_id = "FEDFUNDS", observation_start = as.Date(start_date))
  ffr_data <- ffr_data %>%
    rename(FederalFundsRate = value) %>%
    select(date, FederalFundsRate)
  
  # Fetch CPI (Consumer Price Index for All Urban Consumers) from FRED
  cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date(start_date))
  cpi_data <- cpi_data %>%
    rename(CPI = value) %>%
    select(date, CPI)
  
  # Merge data frames by date
  data_merged <- stock_stock %>%
    left_join(cpi_data, by = "date") %>%
    left_join(ffr_data, by = "date") %>%
    left_join(spy_etf, by = "date") %>%
    left_join(iwm_etf, by = "date") %>%
    left_join(iwb_etf, by = "date") %>%
    left_join(iwd_etf, by = "date") %>%
    left_join(iwf_etf, by = "date") %>%
    left_join(vig_etf, by = "date") %>%
    left_join(qqq_etf, by = "date")
  
  # Calculate stock returns from stock_Price
  data_merged <- data_merged %>%
    mutate(MonthlyFederalFundsRate = lag(FederalFundsRate)/1200) %>%
    mutate(InflationRiskPremium = log(CPI / lag(CPI)) - lag(FederalFundsRate)/1200) %>%
    mutate(stock_Excess_Ln_Return = log(stock_Price / lag(stock_Price)) - lag(FederalFundsRate)/1200) %>%
    mutate(SPY_Excess_Ln_Return = log(SPY_Price / lag(SPY_Price)) - lag(FederalFundsRate)/1200) %>%
    mutate(SMB = (log(IWM_Price / lag(IWM_Price)) - lag(FederalFundsRate)/1200) - (log(IWB_Price / lag(IWB_Price)) - lag(FederalFundsRate)/1200)) %>%
    mutate(HML = (log(IWD_Price / lag(IWD_Price)) - lag(FederalFundsRate)/1200) - (log(IWF_Price / lag(IWF_Price)) - lag(FederalFundsRate)/1200)) %>%
    mutate(RMW = (log(SPY_Price / lag(SPY_Price)) - lag(FederalFundsRate)/1200) - (log(IWM_Price / lag(IWM_Price)) - lag(FederalFundsRate)/1200)) %>%
    mutate(CMA = (log(VIG_Price / lag(VIG_Price)) - lag(FederalFundsRate)/1200) - (log(QQQ_Price / lag(QQQ_Price)) - lag(FederalFundsRate)/1200)) %>%
    na.omit()
  
  # Fit the linear regression model
  model <- lm(stock_Excess_Ln_Return ~ SPY_Excess_Ln_Return + SMB + HML + RMW + CMA + InflationRiskPremium, data = data_merged)
  print(summary(model)) #summarizes the model 
  
  #Estimate the White's Test
  print(white_test(model))
  
  #Correct the standard errors for heteroskedasticity
  print(coeftest(model, vcov=hccm))
  
  #Perform the F test with robust standard errors; see Heiss page 110 and 144
  myH0 <- c("SPY_Excess_Ln_Return=0", "SMB=0", "HML=0", "RMW=0", "CMA=0", "InflationRiskPremium=0")
  linearHypothesis(model, myH0, vcov=hccm)

}

#   Set up

#Cleared the work space
rm(list=ls())
#sets the variable "path" to the name of Working Directory
path <- "/Users/camdenhurd/desktop/Desktop/St Bonaventure Classes/SIMM/Rstudio for SIMM"
#Set the Working Directory using the variable "path"
setwd(path)

ticker <- "QQQ"
start_date <- "2000-01-01"
end_date <- "2024-10-02"
FRED_API_Key <- ""

APT(ticker, start_date, end_date, FRED_API_Key)
