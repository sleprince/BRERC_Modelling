#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  foom <- function() {
    
    
    # message(modelListSummary$coefficients)
    message_string <- "Hello there"
    print(message_string)
    message(message_string)
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

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
#        x    <- faithful[, 2]
#        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        #bins <- 2
        # draw the histogram with the specified number of bins
      plot(graph <- dataDiagnostics(taxa = MyData$Species,
                                    site = MyData$Site,
                                    time_period = MyData$dateofrecord,
                                    progress_bar = FALSE,
                                    plot = TRUE))
          
    })

#})
