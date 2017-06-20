library(dplyr)
library(readr)

opt = read_csv("Jun14 opt.csv")
View(opt)

strsplit(opt$Symbol, " ") %>% sapply(function(x){
  x[[1]]
})

opt$sym = strsplit(opt$Symbol, " ") %>% sapply(function(x){
  x[[1]]
})

