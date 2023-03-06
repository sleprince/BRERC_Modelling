#Pasted from the part of DataDiagnostics that does plots I think, maybe the intention was for other functions
#like Tefler to use this too to make plots, but didn't get implemented?

plotTefler <<- function(taxa, site, time_period, plot = TRUE, progress_bar = TRUE){
  
  
  # Create dataframe from vectors
  taxa_data <- distinct(data.frame(taxa, site, time_period))
  if(progress_bar) setTxtProgressBar(pb, 2)
  
  if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
    recOverTime <- as.numeric(format(time_period,'%Y'))
  } else {
    recOverTime <- time_period
  }
  if(progress_bar) setTxtProgressBar(pb, 3)
  
  # Model the trend in records over time
  bars <- table(recOverTime, dnn = 'RecordsPerYear')
  mData <- data.frame(time_period = as.numeric(names(bars)), count = as.numeric(bars))
  modelRecs <- glm(count ~ time_period, data = mData)
  modelRecsSummary <- summary(modelRecs)  
  if(progress_bar) setTxtProgressBar(pb, 5)
  
  # Reshape the data
  space_time <- dcast(taxa_data, time_period + site ~ ., value.var='taxa',
                      fun.aggregate = function(x) length(unique(x)))
  names(space_time)[ncol(space_time)] <- 'listLength'  
  if(progress_bar) setTxtProgressBar(pb, 9)
  
  # Model the trend in list length
  modelList <- glm(listLength ~ time_period, family = 'poisson', data = space_time)
  modelListSummary <- summary(modelList)  
  if(progress_bar) setTxtProgressBar(pb, 10)
  if(progress_bar) cat('\n\n')
  
  if(plot){
    # Setup plot space
    par(mfrow = c(2,1))
    par(mar = c(0.1, 4.1, 4.1, 2.1))
    
    # Plot a simple histogram
    barplot(height = as.numeric(bars),
            ylab = 'Number of records',
            main = 'Change in records and list length over time')
    
    # Plot the change in list length over time
    par(mar = c(5.1, 4.1, 0.1, 2.1))
    
    if('POSIXct' %in% class(time_period) | 'Date' %in% class(time_period)){
      boxplot(listLength ~ as.numeric(format(space_time$time_period,'%Y')),
              data = space_time,
              xlab = 'Time Period',
              ylab = 'List Length',
              frame.plot=FALSE,
              ylim = c(min(space_time$listLength), max(space_time$listLength)))
    } else {
      boxplot(listLength ~ space_time$time_period,
              data = space_time,
              xlab = 'Time Period',
              ylab = 'List Length',
              frame.plot=FALSE,
              ylim = c(min(space_time$listLength), max(space_time$listLength))) 
    }
    
    
  }
  
  invisible(list(RecordsPerYear = bars, VisitListLength = space_time, modelRecs = modelRecs, modelList = modelList))
}
#In R functions have to be defined at the start.





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
time_periods <- data.frame(start = c(1990, 2000, 2010),
                           end = c(1999, 2009, 2019))

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
                         useIterations = FALSE,
                         minSite = 1)
list(telfer_results)

#plot(telfer_results) #does not work, figure margins too large.

#plot(results)


# then you can extract your dataframe as a csv to convert it into usable data
write.csv(telfer_results, "telfer.csv")




