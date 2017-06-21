library(dplyr)
library(readr)
library(quantmod)
library(tidyr)


#GLOBALS
contract_size=100
#-------------------------------------------------------------------------------------

opt = read_csv("opt.csv")


tickers = unique(opt$Ticker)
#find quotes of tickers
quotes2 = sapply(tickers, function(x){
  getQuote(x)$Last
})

#aggregate quantities by type of option
cts = opt %>% group_by(Ticker,Type) %>% summarize(aggr=sum(Quantity)) #filter(opt,Ticker=="AAPL",Type=="Put")
cts = spread(cts,Type,aggr)

tbl = data.frame(Ticker=tickers,spot=quotes2)
tbl = inner_join(cts,tbl,by="Ticker")
tbl = tbl[c(1,4,2,3)] #reorder columns
#rename cols
tbl = rename(tbl,cts_C=Call)
tbl = rename(tbl,cts_P=Put)
tbl$cts_Net = tbl$cts_P-tbl$cts_C #add col

#get aggregate greeks
greeks = opt %>% group_by(Ticker) %>% summarize(Delta=sum(T.Delta),Gamma=sum(T.Gamma),Theta=sum(T.Theta),Vega=sum(T.Vega))
tbl = inner_join(tbl,greeks, by="Ticker")

# %Delta
tbl$`%_Delta` = tbl$Delta * tbl$spot / contract_size

# %_Gamma
tbl$`%_Gamma` = tbl$Gamma * tbl$spot / contract_size

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
