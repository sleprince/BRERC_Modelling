#OccAssess Shiny App

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

DoAssess1 <<- function() {
  
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
  
  
  write.csv(nRec$data,"C://BRERC//BRERC2.csv", row.names = TRUE)
  system("chmod 644 C://BRERC//BRERC2.csv")
  

  
}


# Define server logic required
shinyServer(function(input, output, clientData) {
  
  #reactive({
    
    #nRec$data$Period <- list((input$start):(input$end))
  #})
  
 # nRec$data$Period <- as.numeric(unlist(periods))
  
  #dat <- reactive({
   # nRec$data[nRec$data$Period %in% seq(from=min(input$start),to=max(input$end),by=1),]
    
  #})
  
  
  
      
     
 
      

  
  observeEvent(input$btn7, {
    withCallingHandlers({
      
      
  
      output$plot2<-renderPlot({
        #ggplot(nRec$data(),aes(y = nRec$data$val, x = periods))+geom_point(colour='red'),height = 400,width = 600)
        ggplot2::ggplot(nRec$data(), ggplot2::aes(y = nRec$data$val, x = nRec$data$Period, colour = nRec$data$group, group = nRec$data$group)) +
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
  

  
  GenerateContent <- function(f) {
    

    f()
      
  }
  
  
        observeEvent(input$btn2, {


          
          x <- 1
          f <- get(funcList[[x]])
          GenerateContent(f)
          
          nRec$data$Period <- as.numeric(unlist(periods))
          
          output$plot2<-renderPlot({
            #ggplot(nRec$data(),aes(y = nRec$data$val, x = periods))+geom_point(colour='red'),height = 400,width = 600)
            ggplot2::ggplot(nRec$data, ggplot2::aes(y = nRec$data$val, x = nRec$data$Period, colour = nRec$data$group, group = nRec$data$group)) +
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
  
        observeEvent(input$btn, {
          
          
          
          x <- 1
          f <- get(funcList[[x]])
          GenerateContent(f)
          
          
          
          
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