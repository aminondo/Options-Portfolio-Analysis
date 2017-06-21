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

wd <- getwd()
setwd(wd)

options = read_csv("Jun14 opt.csv")

port = read_csv("Jun14 Port.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Stock Options"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
   br(),
   wellPanel(   helpText("Charts of Data"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        tabPanel("Port", dataTableOutput("portplot")),
        tabPanel("Option", dataTableOutput("optionplot"))
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$optionplot <- renderDataTable({
      options
  })
  
  output$portplot = renderDataTable({
      port
  })

}

# Run the application 
shinyApp(ui = ui, server = server)