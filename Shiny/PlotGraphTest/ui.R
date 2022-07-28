#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(data.table)
library (ggplot2)

source("DataDiagnostics.R")

DoDiagnostics <<- function() {
setwd("C:/BRERC")

MyData<-read.csv("BRERC2_dataset_vsmall2.csv") #this is the CSV made in run first?


#Needs to be Y-M-D
MyData$dateofrecord <- as.Date(MyData$dateofrecord)
class(MyData$dateofrecord)

# Run some data diagnostics on our data
#and save the results to a png file
#png(file="C:/BRERC/saving_plot.png",
#    width=600, height=350)
results <- dataDiagnostics(taxa = MyData$Species,
                           site = MyData$Site,
                           time_period = MyData$dateofrecord,
                           progress_bar = FALSE)
#dev.off()

#write results that display in console to a txt file.
sink(file = "lm_output.txt")
B()
sink() #end diversion of output

}

# Run some data diagnostics on our data
#and save the results to a png file
#png(file="C:/BRERC/saving_text.png",
#width=600, height=350)
#B()
#dev.off()


#resultas <<- B()

#print(resultas <- B())

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("BRERC Data Tool"),

    # Sidebar 
#    sidebarLayout(
#        sidebarPanel(
            shinyjs::useShinyjs(),
            actionButton("btn","Click me"),
            textOutput("text"),
            
sidebarLayout(
  
  # Sidebar with a slider input
  sidebarPanel(
    
    uiOutput('textWithHTML')
    #sliderInput("obs",
    #            "Number of observations:",
    #            min = 0,
    #            max = 1000,
    #            value = 500)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    imageOutput("myImage")
  )
#textOutput('textWithNewlines'),

    )
)
)
