library(lubridate)
library(tidyverse)
library(dplyr)
library(occAssess)
library(rnrfa)

#SPARTA has a sperarate install script as it is complicated, you should run that first. 
library(sparta)

#DATA FRAMES ONLY LOAD SUCCESSFULLY IN THIS BIT

#setwd("C:/BRERC")
#getwd()

#array containing the 5 OccAssess function names.
funcList <<- list("DoAssess1", "DoAssess2", "DoAssess3", "DoAssess4", "DoAssess5")

setwd("C:/BRERC")

df <- read.csv("BRERC.csv") #this is the original CSV

