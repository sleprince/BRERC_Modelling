#
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
  
  foom <- function() {
    
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
  
  observeEvent(input$btn, {
    withCallingHandlers({
      shinyjs::html("text", "")
      foom()
    },
    message = function(m)
    {
      #message = results
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
    
      })

})






