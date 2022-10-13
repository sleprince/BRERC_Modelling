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

setwd("C:/BRERC")

#chr = 'this is a string'
MyData<-read.csv("Avon_Birds.CSV") #This is the CSV you made in 'run first'

Updater <<- function(Data) {
  
  #First remove the dataframe.
  #rm(MyData)
  
  MyData<-Data
  
}

#array containing the 5 OccAssess function names.
funcList <<- list("DoAssess1", "DoAssess2", "DoAssess3", "DoAssess4", "DoAssess5")

#array containing CSV filenames
CSVList <<- list("Avon_Birds.CSV", "Avon_Butterflies.CSV")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(h1("BRERC Tools",h4("OccAssess Functions"))),
  #radioButtons("normalize", "Normalize?", list("TRUE", "FALSE"), "")  
  #Buttons
  shinyjs::useShinyjs(),
  actionButton("btn2","Assess Record Number (ARN)"),
  actionButton("btn4","ARN Normalized"),
  actionButton("btn","Assess Species Number"),
  actionButton("btn3","Assess Species ID"),
  #actionButton("btn5","Assess Rarity Bias"),
  downloadButton('downloadPlot', 'Download Plot'),
  textOutput("text"),
  
  sidebarLayout(
    
    # Sidebar
    sidebarPanel(
      #textOutput('textWithNewlines'),
      uiOutput('textWithHTML'),
      
      selectInput("csvs", "Choose Database", c("Avon_Birds.CSV","BRERC.CSV", "Avon_Butterflies.CSV"), selected = "Avon_Birds.CSV"),
      actionButton("btn6","Load CSV")
      #sliderInput("obs",
      #            "Number of observations:",
      #            min = 0,
      #            max = 1000,
      #            value = 500)
    ),
    
    #Main panel with plot.
    # Show the plot png
    mainPanel(
      imageOutput("myImage"),
      textOutput("csv"),
      #ChosenCSV = ("csv"),
      #UpdateLoadedCSV()
    )
    
  )
)


DoAssess1 <<- function() {
  
  #1.####Assess Record Number#####
  
  # This function enables researchers to quickly establish how the number of records has changed over time. 
  # Note the argument "normalize" which, if TRUE, will rescale the counts for each level of identifier to 
  # enable comparisons where they are very different.
  
  periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)
  #lists every year in these ranges.
  #periods <- as.numeric(unlist(periods))
  
  #alternative way, gets rid of the weird number of records problem.
  #periods <- list(1950, 1960, 1970, 1980, 1990, 2000, 2010)
  
  #converting the above list of time periods to a string. (1950:1959 etc)
  Periods <- toString(periods)
  print(Periods)
  
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
  
  #converting the period groups made in OccAssess to a String (1 to 7)
  nRecPeriods <- toString(nRec$data$Period)
  print(nRecPeriods)
  
  MyPlot <- ggplot2::ggplot(data = nRec$data, ggplot2::aes(y = nRec$data$val, x = nRec$data$Period, colour = group, group = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_linedraw() +
    #adding in the custom x axis ticks
    #ggplot2::scale_x_discrete(breaks=c(nRecPeriods),
    #                           labels=c(Periods)) +
    ggplot2::ylab("Number of records") +
    ggplot2::labs(colour = "",
                  x = "Period")
  
  
  MyPlot + scale_x_discrete(labels=c(periods))
  
  #MyPlot$labels$x = (labels=c(periods))
  
  plot(MyPlot)
  
  
  #write results that display in console to a txt file.
  sink(file = "lm_output.txt")
  print("This function enables researchers to quickly establish how the number of records has changed over time. Note the argument normalize which, if TRUE, will rescale the counts for each level of identifier to enable comparisons where they are very different.")
  sink() #end diversion of output
  
}

DoAssess2 <<- function() {
  
  #2.####Assess Species Number#####
  
  # In addition to the number of records, you may wish to know how the number of species 
  # (taxonomic coverage) in your dataset changes over time. For this you can use the function assessSpeciesNumber
  
  
  periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)
  
  nSpec <- assessSpeciesNumber(dat = MyData,
                               periods = periods,
                               species = "Species",
                               x = "east",
                               y = "north",
                               year = "Year", 
                               spatialUncertainty = "Uncertainty",
                               identifier = "taxagroup",
                               normalize = FALSE)
  
  str(nSpec$data)
  #Plot function 2 results.
  plot(nSpec$plot)
  
  #write results that display in console to a txt file.
  sink(file = "lm_output.txt")
  print("In addition to the number of records, you may wish to know how the number of species (taxonomic coverage) in your dataset changes over time.")
  sink() #end diversion of output
  
}

DoAssess3 <<- function() {
  
  #3.####Assess Species ID######
  
  #It has been speculated that apparent changes in taxonomic coverage could, in fact, reflect a 
  # change in taxonomic expertise over time. For example, if fewer individuals have the skill to 
  #identify certain species, then it may not appear in your dataset in the later periods. 
  #The function assessSpeciesID treats the proportion of species identified to species level 
  #as a proxy for taxonomic expertise:
  
  
  periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)
  
  propID <- assessSpeciesID(dat = MyData,
                            periods = periods,
                            type = "proportion",
                            species = "Species",
                            x = "east",
                            y = "north",
                            year = "Year", 
                            spatialUncertainty = "Uncertainty",
                            identifier = "taxagroup")
  str(propID$data)
  
  #Plot function 3 results
  plot(propID$plot)
  
  #write results that display in console to a txt file.
  sink(file = "lm_output.txt")
  print("It has been speculated that apparent changes in taxonomic coverage could, in fact, reflect a change in taxonomic expertise over time. For example, if fewer individuals have the skill to identify certain species, then it may not appear in your dataset in the later periods. The function assessSpeciesID treats the proportion of species identified to species level as a proxy for taxonomic expertise.")
  sink() #end diversion of output
  
}

DoAssess4 <<- function() {
  
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
                             normalize = TRUE)
  str(nRec$data)
  
  plot(nRec$plot)
  
  #write results that display in console to a txt file.
  sink(file = "lm_output.txt")
  print("This function enables researchers to quickly establish how the number of records has changed over time. Note the argument normalize which, if TRUE, will rescale the counts for each level of identifier to enable comparisons where they are very different.")
  sink() #end diversion of output
  
}  

DoAssess5 <<- function() {
  
  #4.####assess Rarity Bias#####
  
  # A number of studies have defined taxonomic bias in a dataset as the degree of 
  # proportionality between species' range sizes (usually proxied by the number of 
  # grid cells on which it has been recorded) and the total number of records. One 
  # can regress the number of records on range size, and the residuals give an index 
  # of how over-or undersampled a species is given its prevalence. The function assessSpeciesBias
  # conducts these analyses for each time period, and uses the r2 value from the linear regressions 
  # as an index proportionality between range sizes and number of records. Higher values indicate 
  # that species' are sampled in proportion to their range sizes whereas lower values indicate that 
  # some species are over- or undersampled.
  
  
  
  periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)
  
  taxBias <- assessRarityBias(dat = MyData,
                              periods = periods,
                              res = 0.5,
                              prevPerPeriod = FALSE,
                              species = "Species",
                              x = "east",
                              y = "north",
                              year = "Year", 
                              spatialUncertainty = "Uncertainty",
                              identifier = "taxagroup")
  
  
  str(taxBias$data)
  
  #plot(taxBias$plot)
  
  taxBias$plot +ggplot2::ylim(c(0,1))
  
  #write results that display in console to a txt file.
  sink(file = "lm_output.txt")
  print("It has been speculated that apparent changes in taxonomic coverage could, in fact, reflect a change in taxonomic expertise over time. For example, if fewer individuals have the skill to identify certain species, then it may not appear in your dataset in the later periods. The function assessSpeciesID treats the proportion of species identified to species level as a proxy for taxonomic expertise.")
  sink() #end diversion of output
  
}  

# Define server logic required
server <- function(input, output, session) {
  
  #output$csv <- renderText(input$csvs)
  
  observeEvent(input$btn6, {
    withCallingHandlers({
      shinyjs::html("text", "")
      
      if (input$csvs == "Avon_Butterflies.CSV") {
        
        y = 2
        
      } 
      
      if (input$csvs == "BRERC2.CSV") {
        
        MyData<-read.csv("Avon_Butterflies")
      } 
      
      if (input$csvs == "Avon_Birds.CSV") {
        
        y = 1
      } 
      
      
      UpdateLoadedCSV(int = y, MyData)
      
    },
    
    message = function(m)
      
    {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
    
  })
  
  observeEvent(input$btn5, {
    withCallingHandlers({
      
      if (input$csvs == 'BRERC.CSV') {
        
        y <<- 1
        UpdateLoadedCSV(y, MyData)
        
      } 
      
      if (input$csvs == 'BRERC2.CSV') {
        
        MyData<-read.csv("Avon_Butterflies")
      } 
      
      if (input$csvs == 'Avon_Birds.CSV') {
        
        y <<- 0
        UpdateLoadedCSV(y, MyData)
      } 
      
      
      
    })   
  })
  
  UpdateLoadedCSV <<- function(int, MyData) {
    
    
    ChosenCSV = CSVList[int]
    ChosenCSV <- toString(ChosenCSV) #has to be a string to work.
    #print(ChosenCSV)
    #First remove the dataframe.
    rm(MyData)
    #MyData<-read.csv(ChosenCSV)
    Updater(ChosenCSV)
    
    
    
  }
  
  GenerateContent <- function(f) {
    
    output$myImage <- renderImage({
      #Calls the function to do DataDiagnostics and makes the resulting plot
      #into a png file.
      
      # A temp file to save the output.
      # This file will be removed later by renderImage
      #figure out how to keep image as BRERC want to use it.
      
      outfile <<- tempfile(fileext='.png')
      
      # Generate the PNG
      png(outfile, width=400, height=350)
      
      #call the function that makes the plot
      #match.fun(AssessFun)
      f()
      #DoAssess1()
      dev.off() #main="Generated in renderImage()"
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = 400,
           height = 320,
           alt = "This is alternate text",
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    
    output$textWithHTML <- renderUI({
      rawText <- readLines('lm_output.txt') # get raw text
      
      # split the text into a list of character vectors
      #   Each element in the list contains one line
      splitText <- stringi::stri_split(str = rawText, regex = '\\n')
      
      # wrap a paragraph tag around each element in the list
      replacedText <- lapply(splitText, p)
      
      return(replacedText)
    })
    
    output$downloadPlot <- downloadHandler(
      filename = "Shinyplot.png",
      content = function(file) {
        png(file)
        f()
        dev.off()
      })
    
    
  }
  
  
  observeEvent(input$btn2, {
    withCallingHandlers({
      shinyjs::html("text", "")
      
      x <- 1
      f <- get(funcList[[x]])
      #AssessFun <- paste0("DoAssess", x)
      GenerateContent(f)
    },
    
    message = function(m)
      
    {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
    
  })
  
  observeEvent(input$btn, {
    withCallingHandlers({
      shinyjs::html("text", "")
      
      x <- 2
      f <- get(funcList[[x]])
      #AssessFun <- paste0("DoAssess", x)
      GenerateContent(f)
    },
    
    message = function(m)
      
    {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
    
  })
  
  observeEvent(input$btn3, {
    withCallingHandlers({
      shinyjs::html("text", "")
      
      x <- 3
      f <- get(funcList[[x]])
      #AssessFun <- paste0("DoAssess", x)
      GenerateContent(f)
    },
    
    message = function(m)
      
    {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
    
  })
  
  observeEvent(input$btn4, {
    withCallingHandlers({
      shinyjs::html("text", "")
      
      x <- 4
      f <- get(funcList[[x]])
      GenerateContent(f)
    },
    
    message = function(m)
      
    {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
    
  })
  
  observeEvent(input$btn5, {
    withCallingHandlers({
      shinyjs::html("text", "")
      
      x <- 5
      f <- get(funcList[[x]])
      GenerateContent(f)
    },
    
    message = function(m)
      
    {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
    
  })
  
}

shinyApp(ui, server)