# Author: Antonio Minondo
# File: risk_analysis.R
# Description: This file generates risk_analysis table for options portolio. Uses file
# opt.csv downloaded from Etrade Api in order to fill the columns
# Current file contains information on the folowing options:
# "AAPL" "BA"   "BLK"  "BUD"  "CMG"  "CVX"  "FB"   "GS"   "IBM"  "JNJ"  "JPM"  "M"    "MAR" 
# "MCD"  "MMM"  "NFLX" "NVDA" "PXD"  "SPY"  "UAL"  "UNP"  "WYNN"
# Date: June 22, 2017
# ----------------------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(quantmod)
library(tidyr)


#GLOBALS
contract_size=100
#-------------------------------------------------------------------------------------

#open files --------------------------------------------------------------------------
opt = read_csv("opt.csv")
margin_return = read_csv("margin_return.csv")

# -----------------------------------------------------------------------------------

#get unique quotes and tickers
tickers = unique(opt$Ticker)
quotes = opt %>% group_by(Ticker) %>% summarize(Spot = unique(Spot))

#aggregate quantities by type of option
cts = opt %>% group_by(Ticker,Type) %>% summarize(aggr=sum(Quantity))
cts = spread(cts,Type,aggr)
#replace 0s before joining main tbl
cts = cts %>% replace_na(list(Call = 0,Put=0))

#create main tbl
tbl = data.frame(Ticker=tickers,Spot=quotes$Spot)
tbl = inner_join(tbl,cts,by="Ticker")
#rename cols
tbl = dplyr::rename(tbl,cts_C=Call)
tbl = dplyr::rename(tbl,cts_P=Put)

tbl$cts_Net = tbl$cts_P-tbl$cts_C #add col

#Notional
notional = margin_return %>% group_by(Ticker,Type) %>% summarize(aggr = sum(Notional))
notional = spread(notional,Type,aggr)
#replace 0s before joining main tbl
notional = notional %>% replace_na(list(Call = 0,Put=0))
#merging notional...
tbl = inner_join(tbl,notional, by="Ticker")
#renaming columns
tbl = dplyr::rename(tbl,Notional_C=Call)
tbl = dplyr::rename(tbl,Notional_P=Put)

#get aggregate greeks
greeks = opt %>% group_by(Ticker) %>% summarize(Delta=sum(T.Delta),Gamma=sum(T.Gamma),Theta=sum(T.Theta),Vega=sum(T.Vega))

#no NAs should be in this file, but for robustness sake, remove na's before joining
#main tbl
greeks = greeks %>% replace_na(list(Delta = 0,Gamma=0,Theta=0,Vega=0))
#merging greeks...
tbl = inner_join(tbl,greeks, by="Ticker")

# %Delta
tbl$`%_Delta` = tbl$Delta * tbl$Spot / contract_size

# %_Gamma
tbl$`%_Gamma` = tbl$Gamma * tbl$Spot / contract_size

# % of port delta
total_delta = sum(tbl$Delta)
tbl$`%_of_Port_Delta` = tbl$Delta/total_delta*100

# %_Delta / Port % Delta
total.perc.delta = sum(tbl$`%_Delta`)
total.perc.delta
tbl$`%Delta_of_Port_%Delta` = tbl$`%_Delta`/total.perc.delta*100

# % of port gamma
total_gamma = sum(tbl$Gamma)
tbl$`%_of_Port_Gamma` = tbl$Gamma/total_gamma*100

# %_gamma / Port % gamma
total.perc.gamma = sum(tbl$`%_Gamma`)
total.perc.gamma
tbl$`%Gamma_of_Port_%Gamma` = tbl$`%_Gamma`/total.perc.gamma*100

# % of port theta
total_theta = sum(tbl$Theta)
tbl$`%_of_Port_Theta` = tbl$Theta/total_theta*100

#Batting Average
tbl$batting_avg = tbl$Theta/tbl$`%_Delta`

#log roll
tbl$log_roll = tbl$`%_Gamma`/tbl$`%_Delta`

#theta_gamma_cvg
tbl$theta_gamma_cvg = -tbl$Theta/tbl$`%_Gamma`

write_csv(tbl,"risk_tbl.csv")



