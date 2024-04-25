#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(readr)
cardio_train <- read_delim("~/Documents/dsfinal/cardio_train.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE) 
cardio_train$ap_mean <- rowMeans(cardio_train[ , c(6,7)], na.rm=TRUE)
optionsList = c('ap_mean','ap_hi','ap_lo')
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("How does a person's weight affect their blood pressure levels?"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    inputPanel(
      varSelectInput("yvar", "Blood Pressure Adjustment (low, high, average)", cardio_train[ , c(6,7,14)], selected = "ap_mean"),
      sliderInput("span", label = "Confidence Level adjustment:",
                  min = 0.2, max = 0.95, value = 0.95, step = 0.05),
      sliderInput("xrange", label = "Range of Weight Values:",
                  min = 0, max = 200, value = 200, step = 10),
      sliderInput("yrange", label = "Range of Blood Pressure Values:",
                  min = 25, max = 250, value = 250, step = 10)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("line")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$line <- renderPlot({ggplot(cardio_train, aes(x=weight, !!input$yvar)) + geom_point() + stat_smooth( mapping = NULL, data = NULL, geom = "smooth", position = "identity", method = NULL, formula = NULL, se = TRUE, n = 80, span = 0.75, fullrange = FALSE, level = input$span, method.args = list(), na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE) + xlim(0,input$xrange) + ylim(25,input$yrange) + theme_bw()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)