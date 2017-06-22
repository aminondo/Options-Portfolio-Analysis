library(readr)
library(magrittr)
library(dplyr)
library(quantmod)
library(reshape)

#set up GLOBALS -------------------------------------------------------------------------
Contract_Size = 100
Req_Standard = 0.2
Days_in_Year = 365


#Read's CSV file and makes the first row the column names
opt = read_csv("opt.csv")
margin_return <- read_csv("Jun14 PRDMR Report.csv")
margin_return = margin_return[-c(86:89), ]


#get unique quotes and tickers
tickers = unique(opt$Ticker)
quotes = opt %>% group_by(Ticker) %>% summarize(Spot = unique(Spot))


#Transfer Spot from opt file to margin_return table
for(id in 1:nrow(margin_return)){
  val = quotes[quotes$Ticker==margin_return[id,]$Ticker,]$Spot
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
margin_return$`Gross RR`= as.numeric(margin_return$`Gross RR`)
margin_return$Annual= as.numeric(margin_return$Annual)
margin_return$`Yld / Put G` = as.numeric(margin_return$`Yld / Put G`) 
margin_return$`Yld / Put A` = as.numeric(margin_return$`Yld / Put A`)


#Make Today and Expiration dates; Create Days Until Expiration Column
margin_return = margin_return %>% mutate(Today = Sys.Date())
margin_return = margin_return %>% mutate(Expiration = as.Date(margin_return$Expiration, "%m/%d/%y"))
margin_return$Days_Until_Expire = margin_return$Expiration - margin_return$Today
margin_return = margin_return%>% mutate(Days_Until_Expire = as.numeric(Days_Until_Expire))

#Start developing PRDMR_REPORT
#Formula for premium
margin_return = margin_return %>% mutate(Premium = (Bid+Ask)/2*Qty*Contract_Size)

#Formula for Min Req.
margin_return = margin_return %>% mutate(`Min. Req.` = Strike*-Qty*Contract_Size*0.1)

#Notional
margin_return = margin_return %>% mutate(Notional = Strike*Qty*Contract_Size)

#Magin Not.
margin_return = margin_return %>% mutate(`Magin Not.` = Spot*Qty)
#write_csv(margin_return, "margin_return.csv")

#Gross Margin 
margin_return = margin_return%>% mutate(`Gross Margin` = -(`Magin Not.`*Req_Standard))

#OOMP/C 
margin_return = margin_return%>% mutate(`OOMP/C` = ((Spot-Strike)*(-Qty*Contract_Size)))

#Net Margin 
margin_return = margin_return%>% mutate(`Net Margin` = (ifelse(Type == "Call", 
                                                               ifelse(`Gross Margin`+ `OOMP/C`>`Min. Req.`,`Gross Margin`+ `OOMP/C`,`Min. Req.`),
                                                               ifelse(`Gross Margin`- `OOMP/C`>`Min. Req.`,`Gross Margin`- `OOMP/C`,`Min. Req.`))))


#Total Margin
margin_return = margin_return%>% mutate(`Total Margin` = `Net Margin` - Premium)

#`Spot + 1%`
margin_return = margin_return%>% mutate(`Spot + 1%` = Spot + Spot*0.01)

#N+Notional
margin_return = margin_return%>% mutate(`N+Notional` = `Spot + 1%`*Qty*Contract_Size)

#NGM+
margin_return = margin_return%>% mutate(`NGM+`= -(`N+Notional`*Req_Standard))

#NOOMP/C+
margin_return = margin_return%>% mutate(`NOOMP/C+` = (`Spot + 1%` - Strike)*(-Qty*Contract_Size))

#Margin+Delta
margin_return = margin_return%>% mutate(`Margin+Delta` = (`Net Margin` - ifelse(Type == "Call", 
                                                                                ifelse(`NGM+` + `NOOMP/C+`>`Min. Req.`,`NGM+` + `NOOMP/C+`,`Min. Req.`), 
                                                                                ifelse(`NGM+` - `NOOMP/C+`>`Min. Req.`,`NGM+` - `NOOMP/C+`,`Min. Req.`))))

#Spot-1%
margin_return = margin_return %>% mutate(`Spot - 1%` = Spot - Spot*0.01)

#N-Notional
margin_return = margin_return%>% mutate(`N-Notional` = `Spot - 1%`*Qty*Contract_Size)

#NGM-
margin_return = margin_return%>% mutate(`NGM-`= -(`N-Notional`*Req_Standard))

#NOOMP/C-
margin_return = margin_return%>% mutate(`NOOMP/C-` = (`Spot - 1%`-Strike)*(-Qty*Contract_Size))

#Margin-Delta#Fix this one
margin_return = margin_return%>% mutate(`Margin-Delta` = (`Net Margin` - ifelse(Type == "Call", 
                                                                                ifelse(`NGM-` + `NOOMP/C-` > `Min. Req.`, `NGM-` + `NOOMP/C-`, `Min. Req.`), 
                                                                                ifelse(`NGM-` - `NOOMP/C-` > `Min. Req.`, `NGM-` - `NOOMP/C-`, `Min. Req.`))))

#%G/L
margin_return = margin_return%>% mutate(`% G/L` = (1 - (`Last Trade`/`Cost Basis`)))
                        
#Gross RR
margin_return = margin_return%>% mutate(`Gross RR` = abs(Premium/`Net Margin`))

#Annual #Need to change the variable Expiration and Today to dates
margin_return = margin_return%>% mutate(Annual = ifelse(Days_Until_Expire == 0, "Expiring", Days_in_Year/(Days_Until_Expire) * `Gross RR`))

#Yld/Put G
margin_return = margin_return %>% mutate(`Yld / Put G` = ifelse(Type == "Call", NA, ifelse(Spot<Strike, abs(((Spot-Strike)*Qty*Contract_Size) + Premium)/`Net Margin`, NA)))

#Yld/Put A
margin_return = margin_return%>% mutate(`Yld / Put A` = ifelse(is.na(`Yld / Put G`), NA, Days_in_Year/(Days_Until_Expire)*`Yld / Put G`))
