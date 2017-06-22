library(readr)
library(magrittr)
library(dplyr)
library(quantmod)
library(reshape)


#Read's CSV file and makes the first row the column names
margin_return <- read_csv("Jun14 PRDMR Report.csv")
margin_return = margin_return[-c(86:89), ]

#get quotes and flip the quotes table
tickers = unique(margin_return$Ticker)
quotes2 = sapply(tickers, function(x){
  getQuote(x)$Last
})

quotes2$Ticker = colnames(quotes2[[1]]) #Coerces the table into a list
quotes2 = melt(quotes2, "1") #Flips the table
colnames(quotes2) = c("Spot", "Ticker") #Rename the columns
quotes2 = quotes2[c(2,1)] #Flips the order of the columns

quotes2 = quotes2[-c(23, 24, 25, 26), ]

#Forumla for Spot
for(id in 1:nrow(margin_return)){
  val = quotes2[quotes2$Ticker==margin_return[id,]$Ticker,]$Spot
  margin_return[id,]$Spot = val
}



#mutate variables from characters to numeric
margin_return$Qty = as.numeric(margin_return$Qty)
margin_return$Bid = as.numeric(margin_return$Bid)
margin_return$Ask = as.numeric(margin_return$Ask)
margin_return$Strike = lapply(margin_return$Strike, function(x){ gsub("\\$", "", x)}) #remove $ sign from strike price
margin_return$Strike = as.numeric(margin_return$Strike)
margin_return$Spot = as.numeric(margin_return$Spot)
margin_return$`Magin Not.`= as.numeric(margin_return$`Magin Not.`)
margin_return$`OOMP/C` = as.numeric(margin_return$`OOMP/C`) 
margin_return$`Min. Req.` = as.numeric(margin_return$`Min. Req.`)
margin_return$`Gross Margin` = as.numeric(margin_return$`Gross Margin`)
margin_return$Premium = as.numeric(margin_return$Premium) 
margin_return$Bid = as.numeric(margin_return$Bid)
margin_return$Ask = as.numeric(margin_return$Ask) 
margin_return$`Net Margin` = as.numeric(margin_return$`Net Margin`)
margin_return$`Total Margin` = as.numeric(margin_return$`Total Margin`)
margin_return$`Spot + 1%` = as.numeric(margin_return$`Spot + 1%`) 
margin_return$`N+Notional` = as.numeric(margin_return$`N+Notional`)
margin_return$`NGM+` = as.numeric(margin_return$`NGM+`)
margin_return$`NOOMP/C+` = as.numeric(margin_return$`NOOMP/C+`)
margin_return$`Margin+Delta`= as.numeric(margin_return$`Margin+Delta`)
margin_return$`Spot - 1%` = as.numeric(margin_return$`Spot - 1%`)
margin_return$`N-Notional`= as.numeric(margin_return$`N-Notional`)
margin_return$`NGM-` = as.numeric(margin_return$`NGM-`)
margin_return$`NOOMP/C-` = as.numeric(margin_return$`NOOMP/C-`)
margin_return$`Margin-Delta`= as.numeric(margin_return$`Margin-Delta`) 
margin_return$`% G/L`= as.numeric(margin_return$`% G/L`) 
margin_return$`Gross RR`= as.numeric(margin_return$`Gross RR`) Annual
margin_return$Annual= as.numeric(margin_return$Annual)

#set up contract size & req_standard
Contract_Size = 100
Req_Standard = 0.2
Days_in_Year = 365

#Start developing PRDMR_REPORT

#Things that need to change = Today, Spot, Premium, Total Margin, Marin+Delta, Margin-Delta, %G/L, Gross RR, Annual, Yld / Put G, Yld / Put A

#Chnage Today to Sys.Date
margin_return %>% mutate(Today = Sys.Date()) %>% format(Today, format = "%m/%d/%Y") #still need to learn how to reformat the date

#Formula for premium
margin_return %>% mutate(Premium = median(Bid+Ask)*Qty*Contract_Size)

#Formula for Min Req.
margin_return %>% mutate(`Min. Req.` = Strike-Qty*Contract_Size*0.1)

#Notional
margin_return %>% mutate(Notional = Strike*Qty*Contract_Size)

#Magin Not.
margin_return %>% mutate(`Magin Not.` = Spot*Qty)
write_csv(margin_return, "margin_return.csv")

#Gross Margin 
margin_return%>% mutate(`Gross Margin` = -(`Magin Not.`*Req_Standard))

#OOMP/C 
margin_return%>% mutate(`OOMP/C` = ((Spot-Strike)*(-Qty*Contract_Size)))

#Net Margin 

margin_return%>% mutate(`Net Margin` = (ifelse(Type == "Call", max(`Gross Margin`+ `OOMP/C`, `Min. Req.`), max(`Gross Margin`- `OOMP/C`, `Min. Req.`))))

#Premium 
margin_return%>% mutate(Premium = (median(Bid:Ask)*Qty*Contract_Size))

#Total Margin
margin_return%>% mutate(`Total Margin` = `Net Margin` - Premium)

#`Spot + 1%`
margin_return%>% mutate(`Spot + 1%` = Spot + Spot*0.01)

#N+Notional
margin_return%>% mutate(`N+Notional` = `Spot + 1%`*Qty*Contract_Size)

#NGM+
margin_return%>% mutate(`NGM+`= -(`N+Notional`*Req_Standard))

#NOOMP/C+
margin_return%>% mutate(`NOOMP/C+` = (`Spot + 1%` - Strike)*(-Qty*Contract_Size))

#Margin+Delta
margin_return%>% mutate(`Margin+Delta` = (`Net Margin` - ifelse(Type == "Call", max(`NGM+` + `NOOMP/C+`, `Min. Req.`), max(`NGM+` - `NOOMP/C+`, `Min. Req.`))))

#Spot-1%
margin_return %>% mutate(`Spot - 1%` = Spot - Spot*0.01)

#N-Notional
margin_return%>% mutate(`N-Notional` = `Spot - 1%`*Qty*Contract_Size)

#NGM-
margin_return%>% mutate(`NGM-`= -(`N-Notional`*Req_Standard))

#NOOMP/C-
margin_return%>% mutate(`NOOMP/C-` = (`Spot - 1%`-Strike)*(-Qty*Contract_Size))

#Margin-Delta
margin_return%>% mutate(`Margin-Delta` = (`Net Margin` - ifelse(Type == "Call", max(`NGM-` + `NOOMP/C-`, `Min. Req.`), max(`NGM-` - `NOOMP/C-`, `Min. Req.`))))

#%G/L
margin_return%>% mutate(`% G/L` = (1 - (`Last Trade`/`Cost Basis`)))
                        
#Gross RR
margin_return%>% mutate(`Gross RR` = abs(Premium/`Net Margin`))

#Annual #Need to change the variable Expiration and Today to dates
#margin_return%>% mutate(Annual = if(Expiration - Today = 0, "Expiring", Days_in_Year/(Expiration - Today) * `Gross RR`))

