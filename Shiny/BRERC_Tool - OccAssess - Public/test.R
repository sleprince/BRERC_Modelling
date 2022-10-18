#rm(list = ls())
library(shiny)
library(ggplot2)

num<-c(1,2,3,4,5)
let<-c("A","B","C","D","E")
date<-c("2015-5-1","2015-6-1","2015-7-1","2015-8-1","2015-9-1")
MyDF <- data.frame(num,let,date)

ui <- fluidPage(
  titlePanel(title=h4("Period of Years", align="center")),
  sidebarPanel( 
    sliderInput("start", "Start Year:",min = 1950, max = 2010,step=1,value=c(1,2))),
    sliderInput("end", "End Year:",min = input$start, max = 2020,step=1,value=c(1,2)))
  mainPanel(plotOutput("plot2"))

server <- function(input,output){
  
  dat <- reactive({
    test <- MyDF[MyDF$periods %in% seq(from=min(input$start),to=max(input$end),by=1),]
    print(test)
    test
  })
  
  output$plot2<-renderPlot({
    ggplot(nRec$data(),aes(y = nRec$data$val, x = periods))+geom_point(colour='red')},height = 400,width = 600)}

shinyApp(ui, server)