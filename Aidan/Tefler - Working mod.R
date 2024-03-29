

setwd("C:/BRERC")
library(sparta)
MyData<-read.csv("BRERC2.csv") #This is the CSV made in run first


#Needs to be Y-M-D
MyData$dateofrecord <- as.Date(MyData$dateofrecord)
class(MyData$dateofrecord)


#Subset to get rid of pre 1960 as numbers too low for comparison with Telfer. 
#you might not need to do this step with smaller datasets
MyData <- subset(MyData, MyData$Year > 1959)

TelData <- MyData[, c("taxagroup", "Site", "Species", "dateofrecord")]

#write.csv(TelData, "telData.csv") OPTIONAL

#Telfer needs time periods, not dates
## Create a new column for the time period
# First define my time periods, lookng at a summary can help you understand 
# what you are working with 
summary(TelData$dateofrecord)

#na.omit(TelData$dateofrecord)

# No need to have this many time periods, can just be 2
time_periods <- data.frame(start = c(1985, 2018),
                           end = c(1985, 2018))

#custom time periods for bcc_fox dataset
#time_periods <- data.frame(start = c(1960, 1970, 2000, 2010, 2020),
#                           end = c(1969, 1979, 2009, 2019, 2029))


#na.omit(time_periods)

# Now use these to assign my dates to time periods
TelData$tp <- date2timeperiod(TelData$dateofrecord, time_periods)



# we can look at a table to see how many records there are per time period
table(TelData$tp)

TelData <- na.omit(TelData)
#na.omit(TelData$tp)

#  1      2      3      4      5      6       
# 1832   4263  36279  77628 310146  25272 

#The Telfer index for each species is the standardized residual from a linear 
# regression across all species and is a measure of relative change only as the 
# average real trend across species is obscured (Isaac et al (2014); Telfer et al, 2002).
# Telfer is used for comparing two time periods and if you have more than this 
# the telfer function will all pair-wise comparisons.



#use the function taken from DataDiagnostics to plot the pre-tefler data to a
#plot called results. DOESN'T WORK
#results <- plotTefler(taxa = TelData$Species,
#                      site = TelData$Site,
#                      time_period = TelData$tp,
#                      progress_bar = FALSE)


telfer_results <- telfer(taxa = TelData$Species,
                         site = TelData$Site,
                         time_period = TelData$tp,
                         useIterations = TRUE,
                         minSite = 2)
list(telfer_results)

#plot(telfer_results) #does not work, figure margins too large.

#plot(results)


# then you can extract your dataframe as a csv to convert it into usable data
write.csv(telfer_results, "telfer.csv")




