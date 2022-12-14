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
  
  nRec <<- assessRecordNumber(dat = MyData,
                              periods = periods,
                              species = "Species",
                              x = "east",
                              y = "north",
                              year = "Year", 
                              spatialUncertainty = "Uncertainty",
                              identifier = "taxagroup",
                              normalize = FALSE)
  
  #for the csv
  nRec$data$Period <- as.numeric(unlist(periods))
  
  
  write.csv(nRec$data,"C://BRERC//BRERC2f1.csv", row.names = TRUE)
  system("chmod 644 C://BRERC//BRERC2f1.csv")
  

  
}

DoAssess2 <<- function() {
  
  #2.####Assess Species Number#####
  
  # In addition to the number of records, you may wish to know how the number of species 
  # (taxonomic coverage) in your dataset changes over time. For this you can use the function assessSpeciesNumber
  
  
  #periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)
  
  nSpec <<- assessSpeciesNumber(dat = MyData,
                               periods = periods,
                               species = "Species",
                               x = "east",
                               y = "north",
                               year = "Year", 
                               spatialUncertainty = "Uncertainty",
                               identifier = "taxagroup",
                               normalize = FALSE)
  
  #for the csv
  nSpec$data$Period <- as.numeric(unlist(periods))
  
  #str(nSpec$data)
  #Plot function 2 results.
 
  
  write.csv(nSpec$data,"C://BRERC//BRERC2f2.csv", row.names = TRUE)
  system("chmod 644 C://BRERC//BRERC2f2.csv")
  


  
}

DoAssess3 <<- function() {
  
  #3.####Assess Species ID######
  
  #It has been speculated that apparent changes in taxonomic coverage could, in fact, reflect a 
  # change in taxonomic expertise over time. For example, if fewer individuals have the skill to 
  #identify certain species, then it may not appear in your dataset in the later periods. 
  #The function assessSpeciesID treats the proportion of species identified to species level 
  #as a proxy for taxonomic expertise:
  
  
  #periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)
  
  propID <<- assessSpeciesID(dat = MyData,
                             periods = periods,
                             type = "proportion",
                             species = "Species",
                             x = "east",
                             y = "north",
                             year = "Year", 
                             spatialUncertainty = "Uncertainty",
                             identifier = "taxagroup")
  
  
  #for the csv
  propID$data$Period <- as.numeric(unlist(periods))
  
  
  write.csv(propID$data,"C://BRERC//BRERC2f3.csv", row.names = TRUE)
  system("chmod 644 C://BRERC//BRERC2f3.csv")
  
  
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
          
          #for the plot.
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
          
          
          
          x <- 2
          f <- get(funcList[[x]])
          GenerateContent(f)
          
          #for the plot.
          nSpec$data$Period <- as.numeric(unlist(periods))
          
          output$plot2<-renderPlot({
          
              ggplot2::ggplot(nSpec$data, ggplot2::aes(y = val, x = nSpec$data$Period, colour = group, group = group)) +
              ggplot2::scale_x_continuous(n.breaks = 10) +
              ggplot2::scale_y_continuous(n.breaks = 10) +
              ggplot2::geom_point() +
              ggplot2::geom_line() +
              ggplot2::theme_linedraw() +
              ggplot2::ylab("Number of species recorded") +
              ggplot2::xlab("Period") +
              ggplot2::labs(colour = "")
          
          })
          
        })
        
        
        observeEvent(input$btn3, {
          
          x <- 3
          f <- get(funcList[[x]])
          GenerateContent(f)
          
          
          #for the plot.
          propID$data$Period <- as.numeric(unlist(periods))
          
          output$plot2<-renderPlot({
            

              ggplot2::ggplot(propID$data, ggplot2::aes(y = prop, x = propID$data$Period, colour = group, group = group)) +
              ggplot2::scale_x_continuous(n.breaks = 10) +
              ggplot2::scale_y_continuous(n.breaks = 10) +
              ggplot2::geom_point() +
              ggplot2::geom_line() +
              ggplot2::theme_linedraw() +
              ggplot2::ylab(ylab) +
              ggplot2::xlab("Period")
            ggplot2::labs(colour = "")

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