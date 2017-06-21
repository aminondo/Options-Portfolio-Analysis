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

options = read_csv("opt.csv")

port = read_csv("port.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = h3("Radio buttons"),
                   choices = list("options" = 1, "port" = 2
                   ),selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("distPlot"),
      textOutput("test")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderDataTable({
    switch(
      as.numeric(input$radio),
      options,
      port
    )
  })
  
  output$test = renderText({
    print(input$radio)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)