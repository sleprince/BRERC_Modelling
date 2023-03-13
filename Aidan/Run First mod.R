

#Run this code to get a new CSV to work with that contains all the data

#Set your working directory
setwd("C:/BRERC")

#I called that dataset BRERC, you will need to change that. Don't touch anything
#else though!

df<-read.csv("BRERC.csv")

library(lubridate)
library(tidyverse)
library(dplyr)
library(occAssess)
library(rnrfa)

#SPARTA has a sperarate install script as it is complicated, you should run that first. 
library(sparta)


#add eastings and northing

# If required, remove spaces in the NGR
df$Site = gsub(" ", "", df$Site, fixed=T)
# Convert NGR to easting and northing
x = osg_parse(df$Site)
# Extract easting and northing from the list, x
df$east = x[[1]]
df$north = x[[2]]


#Adds a randomised date in the year the observation was taken (useful where functions require a full date)
df$Date<-as.POSIXct(paste0(df$Year, "-01-01")) + 
  lubridate::days(sample(0:364, nrow(df), TRUE))

df$Date <- as.Date(df$Date)


#too few records pre-1959 to work with
MyData <- subset(df, df$Year > 1959)
#MyData <- df
rm(df)
rm(x)

#add in time periods (can change to whatever you like, i've taken decades)
##IMORTANT: Can't use a unit of less than a year if using the 'date' column generated above


#NOTE - the date2timeperiod function seems to have stopped working???? 
#time_periods <- data.frame(start = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
#end = c(1969, 1979, 1989, 1999, 2009, 2019, 2029))
#MyData$TP<- date2timeperiod(MyData$Date, time_periods)

#Made a work around.
Time <-MyData %>% mutate(TP =
                           case_when(Year >= 1990 & Year <= 1999 ~ "1", 
                                     Year >= 2000 & Year <= 2009 ~ "2",
                                     Year >= 2010 & Year <= 2019 ~ "3")

                                      
)

rm(MyData)
MyData<- Time
rm(Time)

#Count how many of each species - useful to know but not needed
summary<- MyData %>% count(Species, sort = TRUE)

#add uncertainty#
#Darwin standard uses 0.7km uncertainty for a 1km gridsquare as standard, as used by NBN

MyData$Count <- nchar(MyData$Site)



cert <-MyData %>% mutate(Uncertainty =
                           case_when(Count == 4 ~ "7", 
                                     Count == 6 ~ "0.7",
                                     Count == 8 ~ "0.07",
                                     Count == 10 ~ "0.007",
                                     Count == 12 ~ "0.0007")
)

MyData <- cert
rm(cert)

MyData$TP  <- as.numeric(MyData$TP)
MyData$Uncertainty  <- as.numeric(MyData$Uncertainty)
MyData$Year <- as.integer(MyData$Year)

#####At this point you have a dataset which works with Darwin Standard#####

#Writing data to a new csv file called BRERC2, you have to run RStudio as Admin
#or you'll see "no permission to open file"
#df <- MyData
write.csv(MyData,"C://BRERC//BRERC2.csv", row.names = TRUE)
system("chmod 644 C://BRERC//BRERC2.csv")

