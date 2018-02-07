#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data <- reactive({iris[iris$Species == input$Species,]})
  
  output$plot1 <- renderPlot({
    
    if(input$Part=="s"){
      ggplot(data(), aes(x = Sepal.Length, y = Sepal.Width))+
        geom_point()+
        labs(x = "Length")+
        labs(y = "Width")
    } else {
      ggplot(data(), aes(x = Petal.Length, y = Petal.Width))+
        geom_point()+
        labs(x = "Length")+
        labs(y = "Width")
    }
    
  })
  
})
