######Prep Dataset#####
setwd("C:/BRERC")

#library(lubridate)
#library(tidyverse)
#library(dplyr)
library(occAssess)
#library(rnrfa)
#library(sparta)

MyData<-read.csv("ALLBCCv1pt1.csv") #This is the CSV you made in 'run first'
MyData<-read.csv("tiny.csv")



str(MyData)

periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)

#clean up data
MyData2 <- MyData[, c("Species", "east", "north", "Year", "Uncertainty",
                      "taxagroup")]

rm(MyData)

taxBias <- assessRarityBias(dat = MyData2,
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

taxBias$plot +ggplot2::ylim(c(0,1))

#Sys.setenv('R_MAX_VSIZE'=32000000000)