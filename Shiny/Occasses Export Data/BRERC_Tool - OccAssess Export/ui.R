#OccAssess Shiny App

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
titlePanel(h1("BRERC Tools",h4("OccAssess Functions"))),
#radioButtons("normalize", "Normalize?", list("TRUE", "FALSE"), "")  
  #Buttons
shinyjs::useShinyjs(),
actionButton("btn2","Records Over Time"),
actionButton("btn4","ARN Normalized"),
actionButton("btn","Assess Species Number"),
actionButton("btn3","Assess Species ID"),
actionButton("btn5","Assess Rarity Bias"),
actionButton("btn7","Do it"),
downloadButton('downloadPlot', 'Download Plot'),
#textOutput("text"),



plotOutput("plot2"),



#titlePanel(title=h4("Period of Years", align="center"))

sidebarPanel( 


  
  sidebarLayout(
    
    # Sidebar
    sidebarPanel(
      #textOutput('textWithNewlines'),
      #uiOutput('textWithHTML'),
      
      #selectInput("csvs", "Choose Database", c("Avon_Birds.CSV","BRERC.CSV", "Avon_Butterflies.CSV"), selected = "Avon_Birds.CSV")
      sliderInput("start", "Start Year:", min = min(periods), max = max((periods)+1), step=1 , value = c(min(periods), max(periods)+1)),
      ),
    
    #Main panel with plot.
    # Show the plot png
    mainPanel(
      #imageOutput("myImage"),
      #textOutput("csv"),

      #ChosenCSV = ("csv"),
      #UpdateLoaddCSV()
    )
    
  )
)))



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