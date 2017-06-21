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
