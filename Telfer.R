#Telfer - compares taxa against each other across a set timespan to 
# give a change index score. You need to have a large amount of data to do this 
# (i.e + 100 lines per taxa in each time point)

setwd("D:/BRERC")
library(sparta)
MyData<-read.csv("BRERC.csv")


#Needs to be Y-M-D
MyData$dateofrecord <- as.Date(MyData$dateofrecord)
class(MyData$dateofrecord)


#Subset to get rid of pre 1960 as numbers too low for comparison with Telfer. 
#you might not need to do this step with smaller datasets
TelData <- subset(MyData, MyData$Year > 1959)

#Telfer needs time periods, not dates
## Create a new column for the time period
# First define my time periods, lookng at a summary can help you understand 
# what you are working with 
summary(TelData$dateofrecord)

# No need to have this many time periods, can just be 2
time_periods <- data.frame(start = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
                           end = c(1969, 1979, 1989, 1999, 2009, 2019, 2029))


# Now use these to assign my dates to time periods
TelData$tp <- date2timeperiod(TelData$dateofrecord, time_periods)

# we can look at a table to see how many records there are per time period
table(TelData$tp)

#  1      2      3      4      5      6       
# 1832   4263  36279  77628 310146  25272 

#The Telfer index for each species is the standardized residual from a linear 
# regression across all species and is a measure of relative change only as the 
# average real trend across species is obscured (Isaac et al (2014); Telfer et al, 2002).
# Telfer is used for comparing two time periods and if you have more than this 
# the telfer function will all pair-wise comparisons.


telfer_results <- telfer(taxa = TelData$Species,
                         site = TelData$Site,
                         time_period = TelData$tp,
                         minSite = 2)


# then you can extract your dataframe as a csv to convert it into usable data
write.csv(telfer_results, "telfer.csv")


