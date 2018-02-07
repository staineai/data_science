#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  headerPanel('Petal and Sepal Length and Width by Iris Species'),
  sidebarPanel(
    selectInput('Species', 'Select Species', as.character(unique(iris$Species))),
    selectInput('Part', 'Select Flower Part', list("Sepal"="s", "Petal" = "p"))
  ),
  mainPanel(
    plotOutput('plot1')
  )
))
  