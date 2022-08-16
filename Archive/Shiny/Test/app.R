#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("DataDiagnostics.R")
source("Run_First.R")
source("global.R")

server <- function(input, output, clientData) {
  
  GenerateContent <- function() {
    
    output$myImage <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.png')
      
      
      
      # Generate the PNG
      png(outfile, width=400, height=320)
      DoDiagnostics()
      dev.off() #main="Generated in renderImage()"
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = 400,
           height = 320,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    output$textWithHTML <- renderUI({
      rawText <- readLines('lm_output.txt') # get raw text
      
      # split the text into a list of character vectors
      #   Each element in the list contains one line
      splitText <- stringi::stri_split(str = rawText, regex = '\\n')
      
      # wrap a paragraph tag around each element in the list
      replacedText <- lapply(splitText, p)
      
      return(replacedText)
    })
    
    # message(modelListSummary$coefficients)
    #message_string <- "Hello there"
    #print(message_string)
    
    #message(message_string)
    #message()
    #message("one")
    #Sys.sleep(0.5)
    #message("two")
  }
  
  observeEvent(input$btn2, {
    withCallingHandlers({
      shinyjs::html("text", "")
      GenerateContent()
    },
    message = function(m)
    {
      #message = results
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
    
  })
  
  observeEvent(input$btn, {
    ({
      #shinyjs::html("text", "")
      #RunningFirst()
      #FixedData()
      shinyjs::html(id = "text", html = "DONE", add = TRUE)
      
    })
    
  })
  
}

#osg_parse(grid_refs = "TQ722213")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("BRERC Data Tool"),
  
  #Buttons
  shinyjs::useShinyjs(),
  actionButton("btn","Run First"),
  #            textOutput("text"),
  
  #           shinyjs::useShinyjs(),
  actionButton("btn2","Data Diagnostics"),
  textOutput("text"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      #textOutput('textWithNewlines'),
      uiOutput('textWithHTML')
      #sliderInput("obs",
      #            "Number of observations:",
      #            min = 0,
      #            max = 1000,
      #            value = 500)
    ),
    
    # Show the plot png
    mainPanel(
      imageOutput("myImage")
    )
    
  )
)

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
  B()
  sink() #end diversion of output
  
}

# Run the application 
shinyApp(ui = ui, server = server)
