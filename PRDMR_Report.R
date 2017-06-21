library(readr)
library(magrittr)
library(dplyr)

#Read's CSV file and makes the first row the column names
PRDMR_Report <- read_csv("~/Desktop/Hackathon - Stock Options/DAS Theta Fund - PRDMR Report (6-14-17).csv")
View(PRDMR_Report)
colnames(PRDMR_Report) = PRDMR_Report[1, ] # the first row will be the header
PRDMR_Report = PRDMR_Report[-1, ]          # removing the first row.


#Start developing PRDMR_REPORT

#Things that need to change = Today, Spot, Premium, Total Margin, Marin+Delta, Margin-Delta, %G/L, Gross RR, Annual, Yld / Put G, Yld / Put A

#Chnage Today to Sys.Date
PRDMR_Report %>% mutate(Today = Sys.Date()) %>% format(Today, format = "%m/%d/%Y") #still need to learn how to reformat the date

#Forumla for Spot