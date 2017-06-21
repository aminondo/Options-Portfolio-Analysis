library(dplyr)
library(readr)

opt = read_csv("Jun14 opt.csv")
View(opt)

opt$Ticker = strsplit(opt$Symbol, " ") %>% sapply(function(x){
  x[[1]]
})
opt$`Symbol Description`=NULL
opt$`Last Trade`=NULL
opt$`$ Chg. Close`=NULL
opt$Bid = NULL
opt$Ask=NULL
opt$`IV %`=NULL

contract_size=100
opt = mutate(opt, T.Delta=opt$Quantity*opt$Delta*contract_size)
opt = mutate(opt, T.Theta=opt$Quantity*opt$Theta*contract_size)
opt = mutate(opt, T.Gamma=opt$Quantity*opt$Gamma*contract_size)
opt = mutate(opt, T.Vega=opt$Quantity*opt$Vega*contract_size)
opt = mutate(opt, T.Rho=opt$Quantity*opt$Rho*contract_size)
opt = opt[c(1,8,2,3,4,5,6,7,9:14)] #reorder columns

write_csv(opt,"opt.csv")
