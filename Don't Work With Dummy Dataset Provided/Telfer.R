#Telfer - compares taxa against each other across a set timespan to 
# give a change index score. You need to have a large amount of data to do this 
# (i.e + 100 lines per taxa in each time point)

#' Telfer's change index
library(dplyr)
#' 
#' Telfers change index is designed to assess the relative change in range size of species 
#' between two time periods (Telfer et al 2002). This function can take multiple time periods
#' and will complete all pairwise comparisons.
#' 
#' @param taxa A character vector of taxon names
#' @param site A character vector of site names
#' @param time_period A numeric vector of user defined time periods, or a date vector
#' @param minSite The minimum number of sites occupied in the first time period in
#'        order for a trend to be calculated for a taxon.
#' @param useIterations A logical variable indicating whether iterations should be used.
#'        Iterations are used to account for increased variation in the logit proportions
#'        close to zero and one (see Telfer et al 2002). Default is \code{TRUE}
#' @param iterations If \code{useIterations} is \code{TRUE}, then this parameter indicates
#'        the number of iterations to be used. In Telfer et al 2002 the number of iterations
#'        used were 7 and 8 for the two datasets for which it was applied. The defualt here
#'        is 10.
#' @importFrom reshape2 dcast
#' @examples
#' 
#' # Create fake data
#' SS <- 5000 # number of observations
#' taxa <- sample(letters, SS, replace = TRUE)
#' site <- sample(paste('A', 1:20, sep = ''), SS, replace = TRUE)
#' time_period <- sample(1:3, SS, replace = TRUE)
#' 
#' TelferResult <- telfer(taxa, site, time_period)
#' head(TelferResult)
#' 
#' @export
#' @references Telfer, M.G., Preston, C.D., & Rothery, P. (2002) A general method for
#'             measuring relative change in range size from biological atlas data.
#'             Biological Conservation, 107, 99-109.

telfer <<- function(taxa, site, time_period, minSite = 5, useIterations = TRUE, iterations = 10){
  
  # Perform error checks
  errorChecks(taxa = taxa, site = site, time_period = time_period, minSite = minSite,
              useIterations = useIterations, iterations = iterations)
  
  taxa_data <- data.frame(taxa, site, time_period)
  
  # Create a list of all pairwise comparisons of time periods
  TP_combos <- t(combn(x = sort(unique(time_period)), m = 2))
  
  # For each pair of time periods go through and compare them
  TelferList <- apply(X = TP_combos, MARGIN = 1, FUN = function(TPs){
    
    # Do the core Telfer analysis
    basic_temp <- telfer_func(taxa_data[taxa_data$time_period %in% TPs,], iterations = iterations,
                              useIterations = useIterations, minSite = minSite)[[1]]
    
    colnames(basic_temp)[4] <- paste('Telfer_', TPs[1], '_', TPs[2], sep = '')
    colnames(basic_temp)[3] <- paste('Nsite_', TPs[2], sep = '')
    colnames(basic_temp)[2] <- paste('Nsite_', TPs[1], sep = '')
    
    #Add in NAs
    basic_temp <- merge(basic_temp, data.frame(taxa = sort(unique(taxa))), all = TRUE)
    
    return(basic_temp)
    
  })
  
  Telfer_out <- Reduce(function(a,b) merge(a, b, all = TRUE, by = "taxa"), TelferList)  
  
  return(Telfer_out)
  
}

#' @importFrom dplyr distinct

errorChecks <- function(taxa = NULL, site = NULL, survey = NULL, replicate = NULL, closure_period = NULL, time_period = NULL, 
                        startDate = NULL, endDate = NULL, Date = NULL, 
                        time_periodsDF = NULL, dist = NULL, sim = NULL,
                        dist_sub = NULL, sim_sub = NULL, minSite = NULL, useIterations = NULL,
                        iterations = NULL, overdispersion = NULL, verbose = NULL,
                        list_length = NULL, site_effect = NULL, family = NULL,
                        n_iterations = NULL, burnin = NULL, thinning = NULL,
                        n_chains = NULL, seed = NULL, year_col = NULL, site_col = NULL,
                        sp_col = NULL, start_col = NULL, end_col = NULL, phi = NULL,
                        alpha = NULL, non_benchmark_sp = NULL, fres_site_filter = NULL,
                        time_periods = NULL, frespath = NULL, species_to_include = NULL){
  
  # Create a list of all non-null arguements that should be of equal length
  valid_argumentsTEMP <- list(taxa=taxa,
                              site=site,
                              survey=survey,
                              closure_period=closure_period,
                              replicate=replicate,
                              time_period=time_period,
                              startDate=startDate,
                              endDate=endDate)
  valid_arguments <- valid_argumentsTEMP[!unlist(lapply(valid_argumentsTEMP, FUN = is.null))]
  
  # Check these are all the same length
  if(length(valid_arguments) > 0){
    lengths <- sapply(valid_arguments, length)
    # This tests if all are the same
    if(abs(max(lengths) - min(lengths)) > .Machine$double.eps ^ 0.5){
      stop(paste('The following arguements are not of equal length:', paste(names(valid_arguments), collapse = ', ')))
    }
  }
  
  if(!is.null(taxa) & !is.null(site) & !is.null(survey)){
    
    if(!is.null(replicate)){
      df <- data.frame(taxa, site, survey, replicate)
    } else {
      df <- data.frame(taxa, site, survey)
    }
    
    NR1 <- nrow(df)
    NR2 <- nrow(distinct(df))
    
    if(NR1 != NR2) warning(paste(NR1 - NR2, 'out of', NR1, 'observations will be removed as duplicates'))
    
  }
  
  if(!is.null(taxa) & !is.null(site) & !is.null(time_period)){
    
    df <- data.frame(taxa, site, time_period)
    NR1 <- nrow(df)
    NR2 <- nrow(distinct(df))
    
    if(NR1 != NR2) warning(paste(NR1 - NR2, 'out of', NR1, 'observations will be removed as duplicates'))
    
  }
  
  ###### Make sure there are no NAs
  
  ### Checks for taxa ###
  if(!is.null(taxa)){    
    if(!all(!is.na(taxa))) stop('taxa must not contain NAs')    
  }
  
  ### Checks for site ###
  if(!is.null(site)){    
    if(!all(!is.na(site))) stop('site must not contain NAs')
    if(!all(site != '')) stop("site must not contain empty values (i.e. '')")
  }
  
  ### Checks for closure period ###
  if(!is.null(closure_period)){    
    if(!all(!is.na(closure_period))) stop('closure_period must not contain NAs')    
  }
  
  ### Checks for replicate ###
  if(!is.null(replicate)){    
    if(!all(!is.na(replicate))) stop('replicate must not contain NAs')    
  }
  
  ### Checks for time_period ###
  if(!is.null(time_period)){    
    if(!all(!is.na(time_period))) stop('time_period must not contain NAs')    
  }
  
  ### Checks for startDate ###
  if(!is.null(startDate)){
    if(!'POSIXct' %in% class(startDate) & !'Date' %in% class(startDate)){
      stop(paste('startDate is not in a date format. This should be of class "Date" or "POSIXct"'))
    }
    # Make sure there are no NAs
    if(!all(!is.na(startDate))) stop('startDate must not contain NAs')
  }
  
  ### Checks for Date ###
  if(!is.null(Date)){
    if(!'POSIXct' %in% class(Date) & !'Date' %in% class(Date) & !'data.frame' %in% class(Date)){
      stop(paste('Date must be a data.frame or date vector'))
    }
    # Make sure there are no NAs
    if(!all(!is.na(Date))) stop('Date must not contain NAs')
  }
  
  ### Checks for endDate ###
  if(!is.null(endDate)){
    if(!'POSIXct' %in% class(endDate) & !'Date' %in% class(endDate)){
      stop(paste('endDate is not in a date format. This should be of class "Date" or "POSIXct"'))
    }
    # Make sure there are no NAs
    if(!all(!is.na(endDate))) stop('endDate must not contain NAs')
  }
  
  ### Checks for time_periodsDF ###
  if(!is.null(time_periodsDF)){
    # Ensure end year is after start year
    if(any(time_periodsDF[,2] < time_periodsDF[,1])) stop('In time_periods end years must be greater than or equal to start years')
    
    # Ensure year ranges don't overlap
    starts <- tail(time_periodsDF$start, -1)
    ends <- head(time_periodsDF$end, -1)
    if(any(ends > starts)) stop('In time_periods year ranges cannot overlap')  
  }
  
  ### Checks for dist ###
  if(!is.null(dist)){
    
    if(class(dist) != 'data.frame') stop('dist must be a data.frame')
    if(ncol(dist) != 3) stop('dist must have three columns') 
    if(!class(dist[,3]) %in% c('numeric', 'integer')) stop('the value column in dist must be an integer or numeric')
    
    # Check distance table contains all combinations of sites
    sites <- unique(c(as.character(dist[,1]), as.character(dist[,2])))
    combinations_temp <- merge(sites, sites)
    all_combinations <- paste(combinations_temp[,1],combinations_temp[,2])
    data_combinations <- paste(dist[,1],dist[,2])
    if(!all(all_combinations %in% data_combinations)){
      stop('dist table does not include all possible combinations of sites')
    }    
  }
  
  ### Checks for sim ###
  if(!is.null(sim)){
    
    if(class(sim) != 'data.frame') stop('sim must be a data.frame')
    if(!all(lapply(sim[,2:ncol(sim)], class) %in% c('numeric', 'integer'))) stop('the values in sim must be integers or numeric')
    
  }
  
  ### Checks for sim_sub and dist_sub ###
  if(!is.null(sim_sub) & !is.null(dist_sub)){
    
    if(!class(dist_sub) %in% c('numeric', 'integer')) stop('dist_sub must be integer or numeric')
    if(!class(sim_sub) %in% c('numeric', 'integer')) stop('sim_sub must be integer or numeric')
    if(dist_sub <= sim_sub) stop("'dist_sub' cannot be smaller than or equal to 'sim_sub'")
    
  }
  
  ### checks for minSite ###
  if(!is.null(minSite)){
    
    if(!class(minSite) %in% c('numeric', 'integer')) stop('minSite must be numeric or integer')
    
  }
  
  ### checks for useIterations ###
  if(!is.null(useIterations)){
    
    if(class(useIterations) != 'logical') stop('useIterations must be logical')
    
  }
  
  ### checks for iterations ###
  if(!is.null(iterations)){
    
    if(!class(iterations) %in% c('numeric', 'integer')) stop('iterations must be numeric or integer')
    
  }
  
  ### checks for overdispersion ###
  if(!is.null(overdispersion)){
    
    if(class(overdispersion) != 'logical') stop('overdispersion must be logical')
    
  }
  
  ### checks for verbose ###
  if(!is.null(verbose)){
    
    if(class(verbose) != 'logical') stop('verbose must be logical')
    
  }
  
  ### checks for list_length ###
  if(!is.null(list_length)){
    
    if(class(list_length) != 'logical') stop('list_length must be logical')
    
  }
  
  ### checks for site_effect ###
  if(!is.null(site_effect)){
    
    if(class(site_effect) != 'logical') stop('site_effect must be logical')
    
  }  
  
  ### checks for family ###
  if(!is.null(family)){
    
    if(!family %in% c('Binomial', 'Bernoulli')){
      
      stop('family must be either Binomial or Bernoulli')
      
    }
    
    if(!is.null(list_length)){
      
      if(list_length & family == 'Binomial'){
        warning('When list_length is TRUE family will default to Bernoulli')
      }      
    }
  }
  
  ### checks for species_to_include ###
  
  if(!is.null(species_to_include)){
    
    missing_species <- species_to_include[!species_to_include %in% unique(taxa)]
    
    if(length(missing_species) > 0){
      
      warning('The following species in species_to_include are not in your data: ',
              paste(missing_species, collapse = ', '))
      
    }
  }
  
  ### check BUGS parameters ###
  if(!is.null(c(n_iterations, burnin, thinning, n_chains))){
    if(!is.numeric(n_iterations)) stop('n_iterations should be numeric')
    if(!is.numeric(burnin)) stop('burnin should be numeric')
    if(!is.numeric(thinning)) stop('thinning should be numeric')
    if(!is.numeric(n_chains)) stop('n_chains should be numeric')
    
    
    if(burnin > n_iterations) stop('Burn in (burnin) must not be larger that the number of iteration (n_iterations)')
    if(thinning > n_iterations) stop('thinning must not be larger that the number of iteration (n_iterations)')
    
  }
  
  if(!is.null(seed)){
    
    if(!is.numeric(seed)) stop('seed muct be numeric')
    
  }  
  
  ## Checks for frescalo
  if(!is.null(year_col)){
    if(is.na(year_col)){
      if(!is.null(start_col) & !is.null(end_col)){
        if(is.na(start_col)|is.na(end_col)){
          stop('year_col or start_col and end_col must be given')
        } else {  
          if(!is.na(start_col)|!is.na(end_col)){
            stop('year_col cannot be used at the same time as start_col and end_col')
          }
        }
      }
    }
  }
  
  if(!is.null(phi)){
    if(phi>0.95|phi<0.5){
      stop("phi is outside permitted range of 0.50 to 0.95")
    } 
  }
  
  if(!is.null(alpha)){
    if(alpha>0.5|alpha<0.08){
      stop("alpha is outside permitted range of 0.08 to 0.50")
    } 
  }
  
  if(!is.null(non_benchmark_sp)){    
    if(any(!is.vector(non_benchmark_sp), !is.character(non_benchmark_sp))){
      stop('non_benchmark_sp must be a character vector')
    }
  }
  
  if(!is.null(fres_site_filter)){
    if(any(!is.vector(fres_site_filter), !is.character(fres_site_filter))){
      stop('fres_site_filter must be a character vector')
    }  
  }
  
  if(!is.null(time_periods)){
    if(!is.data.frame(time_periods)) stop('time_periods should be a data.frame. e.g. "data.frame(start=c(1980,1990),end=c(1989,1999))"')
  }
  
  if(!is.null(frespath)){
    if(!grepl('.exe$', tolower(frespath))) stop("filepath is not the path to a '.exe' file") 
    if(!file.exists(frespath)) stop(paste(frespath, 'does not exist'))
  }
}  

#' @param taxa A character vector of taxon names, as long as the number of observations.
#' @param site A character vector of site names, as long as the number of observations.
#' @param time_period A numeric vector of user defined time periods, or a date vector,
#'        as long as the number of observations.
#' @param plot Logical, if \code{TRUE} plots and model results will be printed to
#'        the console
#' @param progress_bar If \code{TRUE} a progress bar is printed to console
#' 

plotDiagnostics <- function(taxa, site, time_period, plot = TRUE, progress_bar = TRUE){
  
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
}

#invisible(list(RecordsPerYear = bars, VisitListLength = space_time, modelRecs = modelRecs, modelList = modelList))



setwd("C:/BRERC")
library(sparta)
MyData<-read.csv("BRERC2.csv") #This is the CSV made in run first?


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
#list(telfer_results)
#plot(telfer_results)


# then you can extract your dataframe as a csv to convert it into usable data
write.csv(telfer_results, "telfer.csv")


