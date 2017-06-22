library(dplyr)
library(readr)
#read in file
opt = read_csv("Jun14 opt.csv")
#GLOBALS------------------------------------------------------------------
contract_size=100
#-------------------------------------------------------------------------

#generate Ticker and Type columns
opt$Ticker = strsplit(opt$Symbol, " ") %>% sapply(function(x){
  x[[1]]
})

opt$Type = strsplit(opt$Symbol, " ") %>% sapply(function(x){
  x[[6]]
})

#clean columns I don't need
opt$`Symbol Description`=NULL
opt$`Last Trade`=NULL
opt$`$ Chg. Close`=NULL
opt$Bid = NULL
opt$Ask=NULL
opt$`IV %`=NULL

#get unique tickers and get spots
tickers = unique(opt$Ticker)
quotes = sapply(tickers, function(x){
  getQuote(x)$Last
})
quotes=data.frame(Ticker=tickers,Spot=quotes)
#adding needed columns
opt = mutate(opt, T.Delta=opt$Quantity*opt$Delta*contract_size)
opt = mutate(opt, T.Theta=opt$Quantity*opt$Theta*contract_size)
opt = mutate(opt, T.Gamma=opt$Quantity*opt$Gamma*contract_size)
opt = mutate(opt, T.Vega=opt$Quantity*opt$Vega*contract_size)
opt = mutate(opt, T.Rho=opt$Quantity*opt$Rho*contract_size)
opt = opt[c(1,8,2,3,4,5,6,7,9:14)] #reorder columns

#Forumla for Spot
for(id in 1:nrow(opt)){
  val = quotes[quotes$Ticker==opt[id,]$Ticker,]$Spot
  print(val)
  #opt[id,]$Spot = val
}

write_csv(opt,"opt.csv")
