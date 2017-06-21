library(dplyr)
library(readr)
library(quantmod)
library(tidyr)
opt = read_csv("opt.csv")
View(opt)


tickers = unique(opt$Ticker)
#find quotes of tickers
quotes2 = sapply(tickers, function(x){
  getQuote(x)$Last
})

#aggregate quantities by type of option
cts = opt %>% group_by(Ticker,Type) %>% summarize(aggr=sum(Quantity)) #filter(opt,Ticker=="AAPL",Type=="Put")
cts = spread(cts,Type,aggr)

tbl = data.frame(tickers,spot=quotes2)
tbl = cbind(tbl,cts_P = cts[[3]],cts_C= cts[[2]]) #add aggregates to tbl

tbl$cts_Net = tbl$cts_P-tbl$cts_C

View(opt)
