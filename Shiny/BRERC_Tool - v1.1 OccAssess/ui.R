#OccAssess Shiny App

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
library(lubridate)
library(tidyverse)
library(occAssess)
library(rnrfa)
library(sparta)

source("DataDiagnostics.R")

funcList <- list("DoAssess1", "DoAssess2", "DoAssess3", "DoAssess4")

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
titlePanel(h1("BRERC Tools",h4("OccAssess Functions"))),
  
  #Buttons
shinyjs::useShinyjs(),
actionButton("btn","Placeholder"),
actionButton("btn2","Assess Record Number"),
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
  print("This function enables researchers to quickly establish how the number of records has changed over time. Note the argument normalize which, if TRUE, will rescale the counts for each level of identifier to enable comparisons where they are very different.")
  sink() #end diversion of output
  
  }


DoAssess1 <<- function() {
  
setwd("C:/BRERC")

MyData<-read.csv("BRERC2.CSV") #This is the CSV you made in 'run first'

#1.####Assess Record Number#####

# This function enables researchers to quickly establish how the number of records has changed over time. 
# Note the argument "normalize" which, if TRUE, will rescale the counts for each level of identifier to 
# enable comparisons where they are very different.

periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)

#dataset is too big to look at everything at once, will need subset

nRec <- assessRecordNumber(dat = MyData,
                           periods = periods,
                           species = "Species",
                           x = "east",
                           y = "north",
                           year = "Year", 
                           spatialUncertainty = "Uncertainty",
                           identifier = "taxagroup",
                           normalize = FALSE)
str(nRec$data)

plot(nRec$plot)

#write results that display in console to a txt file.
sink(file = "lm_output.txt")
print("This function enables researchers to quickly establish how the number of records has changed over time. Note the argument normalize which, if TRUE, will rescale the counts for each level of identifier to enable comparisons where they are very different.")
sink() #end diversion of output

}

  DoAssess2 <<- function() {
  
  
  
  
  }
