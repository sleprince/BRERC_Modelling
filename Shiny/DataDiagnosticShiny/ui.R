#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(sparta)
library(shiny)

setwd("C:/BRERC")
MyData<-read.csv("BRERC2_dataset_vsmall2.csv") #this is the CSV made in run first?


#Needs to be Y-M-D
MyData$dateofrecord <- as.Date(MyData$dateofrecord)
class(MyData$dateofrecord)


# Run some data diagnostics on our data
results <- dataDiagnostics(taxa = MyData$Species,
                           site = MyData$Site,
                           time_period = MyData$dateofrecord,
                           progress_bar = TRUE,
                           plot = TRUE)
#debug (dataDiagnostics)
#undebug(dataDiagnostics)

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

# Define UI for application
shinyUI(fluidPage(
  pre(id = "console"),
  titlePanel("A very simple demonstration of actionButton and isolate in shiny"),
  sidebarLayout(
    sidebarPanel(
      textInput("text1", "Enter your first name"),
      textInput("text2", "Enter your last name"),
      actionButton("action", "Update last name!"),
      p("Click on the Update button to update and display the last name. Here the reactiveness of the input widget is controlled by actionButton on the client and isolate on the server side")
    ),
    mainPanel(
      textOutput("txt1"),
      textOutput("txt2")
    )
  )
))