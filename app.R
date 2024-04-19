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
cardio_train <- read_delim("C:/Users/User/Documents/finalp/cardio_train.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Weight and Blood Pressure"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    inputPanel(
      sliderInput("span", label = "Span adjustment:",
                  min = 0.2, max = 0.9, value = 0.5, step = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("line")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  cardio_train$ap_mean <- rowMeans(cardio_train[ , c(5,6)], na.rm=TRUE)
  output$line <- renderPlot({ggplot(cardio_train,
                                    aes(x=weight, y=ap_mean)) + geom_point() + geom_smooth(method="loess", formula=y~x, se=FALSE, span=input$span) + xlim(0,200) + ylim(25,250) + theme_bw()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)