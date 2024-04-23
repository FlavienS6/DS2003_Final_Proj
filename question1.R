#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)

card<-read_delim("cardio_train.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
card$cardio <- factor(card$cardio, labels = c("No", "Yes"))
card$smoke <- factor(card$smoke, labels = c("Non Smoker", "Smoker"))
card$alco <- factor(card$alco, labels = c("Doesn't Use Alcohol", "Uses Alcohol"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Smoking and Alcohol Consumption"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(
              inputId="xvar",
              label="Choose independent variable",
              choices=c("smoking","alcohol")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("bar")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$bar <- renderPlot({
      if(input$xvar=="smoking"){
        tab<-as.data.frame(table(card$smoke,card$cardio))
        colnames(tab)<-c("smoking","cardiovascular","count")
        ggplot(tab,aes(x=smoking,y=count,fill=cardiovascular))+
        geom_bar(stat="identity")+
        labs(x="Smoking",y="Count",fill="Cardiovascular Disease")+
        scale_fill_manual(values=c("No"="grey","Yes"="blue"))+
        theme_minimal()
      }
      else{
        tab<-as.data.frame(table(card$alco,card$cardio))
        colnames(tab)<-c("alcohol","cardiovascular","count")
        ggplot(tab,aes(x=alcohol,y=count,fill=cardiovascular))+
        geom_bar(stat="identity")+
        labs(x ="Alcohol",y="Count",fill="Cardiovascular Disease")+
        scale_fill_manual(values=c("No"="grey","Yes"="blue"))+
        theme_minimal()
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
