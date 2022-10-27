######Prep Dataset#####
#you need to use a small sample of data for this script
##Plot functionality only seems to work when the 1 line of code run at once.
#Function 4 does not seem to work, could be to do with dummy dataset I'm using.
setwd("C:/BRERC")

library(lubridate)
library(tidyverse)
library(dplyr)
library(occAssess)
library(rnrfa)
library(sparta)

MyData<-read.csv("BRERC2.CSV") #This is the CSV you made in 'run first'

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
                           normalize = FALSE)
str(nRec$data)

nRec$plot
#Plot function 1 results.
#This plots against 'identifier', I've set that to be taxa group but could be species if small dataset












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
nSpec$plot




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
propID$plot


#This deosn't really work for the dataset as is, will need some playing about to see the benefits. 


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

browser()
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

taxBias$plot +ggplot2::ylim(c(0,1))


