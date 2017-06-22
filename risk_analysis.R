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

#runs opt script
#source("port_greeks.R")

#open files --------------------------------------------------------------------------
opt = read_csv("opt.csv")
margin_return = read_csv("margin_return.csv")


#--------------------------------------------------------------------------------------
#CREATE RISK ANALYSIS TABLE
#--------------------------------------------------------------------------------------
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


# GENERATE INFO FROM RISK ANALYSIS AND MARGIN RETURN DATA

#expiration table
exp_tbl = margin_return %>% group_by(Expiration) %>% summarize(premium = abs(sum(Premium)))
tmp = margin_return %>% group_by(Expiration,Type) %>% summarize(sum = sum(`Total Margin`))
tmp = spread(tmp,Type,sum )
exp_tbl = inner_join(exp_tbl,tmp, by="Expiration")

exp_tbl = dplyr::rename(exp_tbl,Tot_Margin_C=Call)
exp_tbl = dplyr::rename(exp_tbl,Tot_Margin_P=Put)
View(exp_tbl)

exp_summary = c(premium_tot = sum(exp_tbl$premium),totMarginP = sum(exp_tbl$Tot_Margin_P, na.rm=T),totMarginC = sum(exp_tbl$Tot_Margin_C, na.rm=T))
exp_summary




mgn_1_Delta = sum(tbl$Notional_P)*.25
cash = 7336380.33
shortfall = mgn_1_Delta+cash
Port_Value = cash-exp_summary[["premium_tot"]]

High_Water = 6958779
climb = Port_Value-High_Water
Drawdown=climb/High_Water*100
Gain_Rqd = (High_Water/Port_Value-1)*100


# % Mkt Table

perc_Mkt = c(2,1,-1,-2,-3,-4,-5,-10)
perc_Mkt = as.data.frame(perc_Mkt)
perc_Mkt = mutate(perc_Mkt, PL = perc_Mkt * sum(tbl$`%_Delta`)+(.5*perc_Mkt*sum(tbl$`%_Gamma`)))
