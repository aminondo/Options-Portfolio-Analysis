library(readr)
library(magrittr)
library(dplyr)

#Read's CSV file and makes the first row the column names
margin_return <- read_csv("~/Desktop/Options-Portfolio-Analysis/Jun14 PRDMR Report.csv")
View(margin_return)
colnames(margin_return) = margin_return[1, ] # the first row will be the header
margin_return = margin_return[-1, ]          # removing the first row.

#mutate variables from characters to numeric
margin_return$Qty = as.numeric(margin_return$Qty)
margin_return$Bid = as.numeric(margin_return$Bid)
margin_return$Ask = as.numeric(margin_return$Ask)
margin_return$Strike = lapply(margin_return$Strike, function(x){ gsub("\\$", "", x)}) #remove $ sign from strike price
margin_return$Strike = as.numeric(margin_return$Strike)


#set up contract size
Contract_Size = 100

#Start developing PRDMR_REPORT

#Things that need to change = Today, Spot, Premium, Total Margin, Marin+Delta, Margin-Delta, %G/L, Gross RR, Annual, Yld / Put G, Yld / Put A

#Chnage Today to Sys.Date
margin_return %>% mutate(Today = Sys.Date()) %>% format(Today, format = "%m/%d/%Y") #still need to learn how to reformat the date

#Forumla for Spot
margin_return %>% mutate(Spot = )

#Formula for premium
margin_return %>% mutate(Premium = median(Bid+Ask)*Qty*Contract_Size)

#Formula for Min Req.
margin_return %>% mutate(`Min. Req.` = Strike-Qty*Contract_Size*0.1)

#Formula for Min Req.
margin_return %>% mutate(Notional = Strike*Qty*Contract_Size)

#Magin Not.