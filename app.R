#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(RCurl)
library(jsonlite)
library(plyr)
library(quantmod)
library(ggplot2)
library(gridExtra)
library(flipsideR)

wd <- getwd()
setwd(wd)

options = read_csv("Jun14 opt.csv")

port = read_csv("Jun14 Port.csv")

risk_table = read_csv("risk_tbl.csv")

totals_table = read_csv("totals_by_ticker.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Stock Options"),
 
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    width = 3,
    wellPanel(
     textInput("ticker_selection", "Ticker", placeholder = "ex: AAPL or GOOGL"),
     textOutput("text"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        id = "tabs", 
        tabPanel("Risk Table", id = "risktbl", dataTableOutput("risktable")),
        tabPanel("Totals by ticker", id = "totals_ticker", dataTableOutput("totalsbyticker")),
        tabPanel("Port",id = "port",  dataTableOutput("portplot")),
        tabPanel("Option", id = "option", dataTableOutput("optionplot")),
        tabPanel("Graph" , id = 3, plotOutput("graph1"))
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$optionplot <- renderDataTable({
      options
  })
  output$risktable = renderDataTable({
    risk_table
  })
  output$portplot = renderDataTable({
      port
  })
  output$totalsbyticker = renderDataTable({
    totals_table
  })

  output$text = renderText({
    sprintf("the current tab is %s", input$tabs)
  })
  

  output$graph1 = renderPlot({
    Stock = tryCatch(getOptionChain(input$ticker_selection), error = function(e) NULL)
    Stock$type = factor(Stock$type)
    next.expiry = min(Stock$expiry)
    current = getQuote(input$ticker_selection)
    g1 = ggplot(subset(Stock, expiry == next.expiry), aes(x = strike)) +
      ggtitle(sprintf("For next expiration date of %s", next.expiry)) + 
      geom_point(aes(y = premium, col = type)) +
      geom_vline(xintercept =as.numeric(current$Last), lty = "dotted") +
      xlab("") + ylab("Option Premium") +
      scale_colour_manual(values = c("red", "blue")) +
      facet_wrap(~ type) +
      theme_classic() + theme(legend.position = "none")
    g2 <- ggplot(subset(Stock, expiry == next.expiry), aes(x = strike)) +
      geom_linerange(aes(ymin = 0, ymax = open.interest / 10000, col = type)) +
      geom_vline(xintercept = current$Last, lty = "dotted") +
      xlab("Strike Price") + ylab("Open Interest (10 000)") +
      scale_colour_manual(values = c("red", "blue")) +
      facet_wrap(~ type) +
      theme_classic() + theme(legend.position = "none")
    grid.arrange( g1, g2, ncol=1)
    
  }, height = 1200, width = 1000)
}

# Run the application 
shinyApp(ui = ui, server = server)