# DataDiagnostics Shiny App

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

# Define UI for application
shinyUI(fluidPage(

# Application title
titlePanel(h1("BRERC DataDiagnostics Tool",h3("Tells you whether your data is
statistically robust."))),
    
#Buttons
shinyjs::useShinyjs(),
actionButton("btn2","Data Diagnostics"),
textOutput("text"),
            
  sidebarLayout(
  
    # Sidebar
    sidebarPanel(
    #textOutput('textWithNewlines'),
    uiOutput('textWithHTML')
    #sliderInput("obs",
    #            "Number of observations:",
    #            min = 0,
    #            max = 1000,
    #            value = 500)
    ),
  
      #Main panel with plot.
      # Show the plot png
      mainPanel(
      imageOutput("myImage")
      )

  )
))


DoDiagnostics <<- function() {
  setwd("C:/BRERC")
  
  MyData <- read.csv("BRERC2.csv") #this is the CSV made in run first?
  
  
  #Needs to be Y-M-D
  MyData$dateofrecord <- as.Date(MyData$dateofrecord)
  class(MyData$dateofrecord)
  
  # Run some data diagnostics on our data
  results <- dataDiagnostics(taxa = MyData$Species,
                             site = MyData$Site,
                             time_period = MyData$dateofrecord,
                             progress_bar = FALSE)

  #write results that display in console to a txt file.
  sink(file = "lm_output.txt")
  B()
  sink() #end diversion of output
  
  }
