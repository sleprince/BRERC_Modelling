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

setwd("C:/BRERC")

MyData<-read.csv("BRERC2_dataset_vsmall2.csv") #this is the CSV made in run first?


#Needs to be Y-M-D
MyData$dateofrecord <- as.Date(MyData$dateofrecord)
class(MyData$dateofrecord)


# Run some data diagnostics on our data
#and save the results to a png file
png(file="C:/BRERC/saving_plot.png",
    width=600, height=350)
#hist(Temperature, col="gold")
results <- dataDiagnostics(taxa = MyData$Species,
                           site = MyData$Site,
                           time_period = MyData$dateofrecord,
                           progress_bar = FALSE)
dev.off()

resultas <- B()
graph <- dataDiagnostics(taxa = MyData$Species,
                                  site = MyData$Site,
                                  time_period = MyData$dateofrecord,
                                  progress_bar = FALSE,
                                  plot = TRUE)

plot(graph <- dataDiagnostics(taxa = MyData$Species,
                              site = MyData$Site,
                              time_period = MyData$dateofrecord,
                              progress_bar = FALSE,
                              plot = TRUE))

print(resultas <- B())

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
#    sidebarLayout(
#        sidebarPanel(
            shinyjs::useShinyjs(),
            actionButton("btn","Click me"),
            textOutput("text"),
            
#          )
#        ),

        # Show a plot of the generated distribution
#        mainPanel(
            plotOutput("distPlot")
#        )
    )
)
