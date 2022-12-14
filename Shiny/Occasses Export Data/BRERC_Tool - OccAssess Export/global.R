library(shiny)
library(dplyr)
library(data.table)
library (ggplot2)
library(lubridate)
library(tidyverse)
library(occAssess)
library(rnrfa)
library(sparta)

#DATA FRAMES ONLY LOAD SUCCESSFULLY IN THIS BIT

setwd("C:/BRERC")

#array containing the 5 OccAssess function names.
funcList <<- list("DoAssess1", "DoAssess2", "DoAssess3", "DoAssess4", "DoAssess5")

#array containing CSV filenames
#CSVList <<- list("Avon_Birds.CSV", "Avon_Butterflies.CSV")

#df1 = read.csv("Avon_Birds.CSV") #This is the CSV you made in 'run first'
#df1 = read.csv("BRERC.CSV")
#MyDF = read.csv("BRERC.CSV")
MyData = read.csv("BRERC.CSV")

#y <<- 1

periods <- list(1950:2020)
periods <- as.numeric(unlist(periods))
#Periods <- periods

#periods <- list(1950:1959, 1960:1969, 1970:1979, 1980:1989, 1990:1999, 2000:2009, 2010:2019)

