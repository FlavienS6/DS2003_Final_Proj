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
library(ggplot2)

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
            ),
            selectInput(
              inputId="chart",
              label="Choose chart type",
              choices=c("bar","pie")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot <- renderPlot({
      if(input$chart=="bar"){
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
      }
      else if(input$chart=="pie"){
        if(input$xvar=="smoking"){
          tab<-as.data.frame(table(card$smoke,card$cardio))
          colnames(tab)<-c("smoking","cardiovascular","count")
          counts<-tapply(tab$count,tab$smoking,sum)
          tab$prop<-tab$count/counts[as.character(tab$smoking)]
          ggplot(tab,aes(x="",y=prop,fill=cardiovascular))+
            geom_bar(stat="identity",width=1)+
            coord_polar("y",start=0)+
            labs(fill="Cardiovascular Disease")+
            theme_void()+
            facet_wrap(~smoking)
        }
        else{
          tab<-as.data.frame(table(card$alco,card$cardio))
          colnames(tab)<-c("alcohol","cardiovascular","count")
          counts<-tapply(tab$count,tab$alcohol,sum)
          tab$prop<-tab$count/counts[as.character(tab$alcohol)]
          ggplot(tab,aes(x="",y=prop,fill=cardiovascular))+
            geom_bar(stat="identity",width=1)+
            coord_polar("y",start=0)+
            labs(fill="Cardiovascular Disease")+
            theme_void()+
            facet_wrap(~alcohol)
        }
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
