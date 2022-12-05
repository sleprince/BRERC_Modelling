#OccAssess Shiny App

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required
shinyServer(function(input, output, clientData) {
  
  reactive({
  if (input$csvs == "Avon_Butterflies.CSV") {
    
    y <<- 2
    
  } 
  
  if (input$csvs == "BRERC2.CSV") {
    
    
  } 
  
  if (input$csvs == "Avon_Birds.CSV") {
    
    y <<- 1
  } 
    
    
    nRec$data$Period <- list((input$start):(input$end))
    
    
    
    
  })
  
  if (y == 1)
  { MyDF <<- df1 }
  
  if (y == 2)
  { MyDF <<- df2 } 
  
  nRec <<- assessRecordNumber(dat = MyDF,
                              periods = periods,
                              species = "Species",
                              x = "east",
                              y = "north",
                              year = "Year", 
                              spatialUncertainty = "Uncertainty",
                              identifier = "taxagroup",
                              normalize = FALSE)
  
  nRec$data$Period <- as.numeric(unlist(periods))
  
  dat <- reactive({
   nRec$data[nRec$data$Period %in% seq(from=min(input$start),to=max(input$end),by=1),]
   #nRec$data[nRec$data$val %in% seq(from=min(input$start),to=max(input$start),by=1),]
 
  })
  
  write.csv(nRec$data,"C://BRERC//BRERC2.csv", row.names = TRUE)
  system("chmod 644 C://BRERC//BRERC2.csv")
  
  output$plot2<-renderPlot({
    #ggplot(nRec$data(),aes(y = nRec$data$val, x = periods))+geom_point(colour='red'),height = 400,width = 600)
    ggplot2::ggplot(dat(), ggplot2::aes(y = nRec$data$val, x = nRec$data$Period, colour = nRec$data$group, group = nRec$data$group)) +
    #adding in the custom x axis ticks
    #has to be at the start***
      ggplot2::scale_x_continuous(n.breaks = 10) +
      ggplot2::scale_y_continuous(n.breaks = 10) +
      ggplot2::geom_point() + 
      ggplot2::geom_line() +
      ggplot2::theme_linedraw() +
      ggplot2::ylab("Number of records") +
      ggplot2::labs(colour = "",
                    x = "Period")
  })
  
      
     
 
      

  
  observeEvent(input$btn7, {
    withCallingHandlers({
  
      output$plot2<-renderPlot({
        #ggplot(nRec$data(),aes(y = nRec$data$val, x = periods))+geom_point(colour='red'),height = 400,width = 600)
        ggplot2::ggplot(dat(), ggplot2::aes(y = nRec$data$val, x = nRec$data$Period, colour = nRec$data$group, group = nRec$data$group)) +
          #adding in the custom x axis ticks
          #has to be at the start***
          ggplot2::scale_x_continuous(n.breaks = 10) +
          ggplot2::scale_y_continuous(n.breaks = 10) +
          ggplot2::geom_point() + 
          ggplot2::geom_line() +
          ggplot2::theme_linedraw() +
          ggplot2::ylab("Number of records") +
          ggplot2::labs(colour = "",
                        x = "Period")
    

    
    })   
})
})
  
  UpdateLoadedCSV <<- function(int) {
    
    
    ChosenCSV = CSVList[int]
    ChosenCSV <- toString(ChosenCSV) #has to be a string to work.
    #print(ChosenCSV)
    #First remove the dataframe.
    #rm(MyData)
    #Data<-read.csv(ChosenCSV)
    Updater(Chosen = ChosenCSV)
    
    
    
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
  
})