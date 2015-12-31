library(doParallel)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tm)
library(rattle)
library(rpart)
library(caret)

setwd("Data_Science_Repo/Capstone/data_set/yelp_dataset_challenge_academic_dataset/")

## reading only necessary data
dataTip <- readRDS("dataTip.rds")
dataBusiness <- readRDS("dataBusiness.rds")

dataBusiness <- as.data.frame(dataBusiness)
#unique(dataBusiness$open) ## make sure that only TRUE/FALSE values are in the column

dataBusinessWTip <- subset(dataBusiness, business_id %in% dataTip$business_id)

# ## the following function cleans the names of the cities in a data frame 'data'
# cleanDataCities <- function(data){
#   uniqueLocations <- unique(data$city) ## find unique locations / cities
# ## the following gets rid of most cities written w/ or w/o diacritics or have additional chars, such as "N." in N. Las Vegas
#   for (i in 1:length(uniqueLocations)){
#     fixLoc <- sapply(uniqueLocations, function(x) grepl(uniqueLocations[i], x))
#     fixLoc <- unlist(fixLoc)
#     uniqueLocations[fixLoc] <- uniqueLocations[i]
#   }
#   uniqueLocations <- iconv(uniqueLocations, to="ASCII//TRANSLIT") ## remove diacritics
#   
#   data$city <- iconv(data$city, to="ASCII//TRANSLIT") ## remove diacritics
#   ## the following cleans the city names according to the unique locations
#   ## in the data frame 'data'
#   for (i in 1:length(uniqueLocations)){
#     fixLoc <- sapply(data$city, function(x) grepl(uniqueLocations[i], x))
#     fixLoc <- unlist(fixLoc)
#     data$city[fixLoc] <- uniqueLocations[i]
#   }
#   return(data)
# }
# dataBusiness <- cleanDataCities(dataBusiness)
# saveRDS(dataBusiness,"dataBusiness.rds")

## creating data frame with only usefull variables for the classification
dataBusinessQM <- data.frame(business_id = dataBusiness$business_id, open = dataBusiness$open, city = dataBusiness$city, review_count = dataBusiness$review_count,
                             state=dataBusiness$state, stars = dataBusiness$stars)
dataBusinessQMbad <- filter(dataBusinessQM, stars < 3) ## find the businesses with bad reviews
dataBusinessQMgood <- filter(dataBusinessQM, stars >4) ## find businesses with good reviews

## train a classification model according to the number of reviews and stars given data frame 'data'
trainDataOpen <- function(data){
## find the probability of staying an open business
  # divide the dataset into 70% data for trainign and 30% for testing
  inTrain <- createDataPartition(data$open, list = F, p = 0.7)
  training <- dataBusinessQM[inTrain,]
  testing <- dataBusinessQM[-inTrain,]
  
  training$open <- as.numeric(training$open)
  testing$open <- as.numeric(testing$open)
  modFit <- train(open~review_count+stars,  method="rpart",data=training) ## do not put open variable 
  ## as a factor in order to compute probabolities
  fancyRpartPlot(modFit$finalModel, sub="") ## draw a fancy classification tree
  save(modFit, file="businessData.rda")
  return(modFit)
}

modFit <- trainDataOpen(dataBusinessQM)

## load the data with specific reviews
reviews <- readRDS("dataReviewsDF.rds")
## find how long are businesses reviewed and the dependence of the time on how many stars has a business in average
findTimeDependence <- function(reviews){
  reviews$date <- as.character(reviews$date)
  reviews$date <- strptime(reviews$date, format="%Y-%m-%d")
}

## find the soonest and the latest review for a specific business within given reviews
findMinMaxDateReview <- function(reviews){
  reviews$date <- as.character(reviews$date)
  reviewsByID <- group_by(reviews, business_id)
  summaryByIDdat<-summarise(reviewsByID, min(date)) # find the first date if the review in database
  summaryByIDdatMax<-summarise(reviewsByID, max(date)) # find the last date if the review in database
  summaryByIDdat <- cbind(summaryByIDdat, summaryByIDdatMax$`max(date)`) # put the reviews with min.max together
  names(summaryByIDdat) <- c("business_id", "minDate", "maxDate")
  # convert to the date format
  summaryByIDdat$minDate <- strptime(summaryByIDdat$minDate, format = "%Y-%m-%d")
  summaryByIDdat$maxDate <- strptime(summaryByIDdat$maxDate, format = "%Y-%m-%d")
  rows <- nrow(summaryByIDdat)
  timeRange <- as.numeric(summaryByIDdat$maxDate[1:rows] - summaryByIDdat$minDate[1:rows]) ## find the time range of reviews
  timeRange <- timeRange/24/60/60 ## convert from seconds to number of days
  summaryByIDdat <- cbind(summaryByIDdat, timeRange = timeRange)
  return(summaryByIDdat)
}

## find the number of stras, given the business data and summary of the
## business data by business ID
findNumberOfStars <- function(dataBusiness, summaryByIDdat){
  rows <- nrow(summaryByIDdat)
  stars <- rep(0, rows)
  open <- rep(0, rows)
  for (i in 1:rows){
    idx <- which(summaryByIDdat$business_id[i] == dataBusiness$business_id)
    stars[i] <- dataBusiness$stars[idx]
    open[i] <- dataBusiness$open[idx]
  }
  summaryByIDdat <- cbind(summaryByIDdat, stars, open)
  return(summaryByIDdat)
}
## find the first and the last date of the review
summaryByIDdat <- findMinMaxDateReview(reviews)
summaryByIDdat$minDate <- as.character(summaryByIDdat$minDate) # convert to character
summaryByIDdat$maxDate <- as.character(summaryByIDdat$maxDate) # convert to character
summaryByIDdatStars <- findNumberOfStars(dataBusiness, summaryByIDdat)

# this function find the mean time of the business duration as a function of the number of stars
findStarsTimeRange <- function(dataBusiness, summaryByIDdatStars, timeThreshold, allAbove){
  ## if we want the data for businesses with the first review above time threshold
  if (allAbove){
    starsTimeRange <- filter(summaryByIDdatStars, minDate > timeThreshold) 
  }
  ## if we want the data for businesses with the first review at the time threshold
  else{
    starsTimeRange <- filter(summaryByIDdatStars, substr(minDate,1,4) == timeThreshold)
  }
  ## group and summarize the data for the number of stars and time range of the reviews
  starsTimeRange <- group_by(starsTimeRange,stars, open)
  starsTimeRange <- summarize(starsTimeRange, meanTime=mean(timeRange))
  starsTimeRange <- mutate(starsTimeRange, maxYears = 2015 - as.numeric(timeThreshold))
  return(starsTimeRange)
}

allAbove = TRUE
## find the number of stars and normalize the data
starsTimeRange <- findStarsTimeRange(dataBusiness, summaryByIDdatStars, '0000-01-01',allAbove)
starsTimeRange$meanTime <- starsTimeRange$meanTime/max(starsTimeRange$meanTime) # normalizing the data

## find the number of stars and normalize the data for all years in the database (need to be automatized)
for (i in 2004:2013){
  timeThreshold <- as.character(i)
  starsTimeRangeDummy <- findStarsTimeRange(dataBusiness, summaryByIDdatStars, timeThreshold, TRUE)
  starsTimeRangeDummy$meanTime <- starsTimeRangeDummy$meanTime/max(starsTimeRangeDummy$meanTime)
  starsTimeRange <- rbind(starsTimeRange, starsTimeRangeDummy)
}
saveRDS(starsTimeRange, file="starsTimeRange.rds")

# filter the results according to the open variable
starsTimeRangeOpen <- filter(starsTimeRange, open == 1) # businesses that are open
starsTimeRangeClose <- filter(starsTimeRange, open == 0) # businesses that are closed

######--------------- modeling the duration of the business according to the average star number ------------------######
# find a polynomial fit to the mean time the business is open as a function of stars for open/closed businesses
modelOpen <- lm(starsTimeRangeOpen$meanTime~poly(starsTimeRangeOpen$stars, 3)) 
modelClose <- lm(starsTimeRangeClose$meanTime~poly(starsTimeRangeClose$stars, 3))
# predict function is used to find the linear fit and then drawing the data
predicted.intervalsOpen <- predict(modelOpen, data.frame(starsTimeRangeOpen$stars), interval = 'confidence', level = 0.975)
predicted.intervalsClose <- predict(modelClose, data.frame(starsTimeRangeClose$stars), interval = 'confidence', level = 0.975)
# create a data frame with the predicted time as a function of the number of stars
starsTimeRangeOpenPredicted <- data.frame(stars = starsTimeRangeOpen$stars, predicted = predicted.intervalsOpen[,1])
starsTimeRangeOpenPredictedPlusErr <- data.frame(stars = starsTimeRangeOpen$stars, predicted = predicted.intervalsOpen[,2])
starsTimeRangeOpenPredictedMinErr <- data.frame(stars = starsTimeRangeOpen$stars, predicted = predicted.intervalsOpen[,3])

starsTimeRangeClosePredicted <- data.frame(stars = starsTimeRangeClose$stars, predicted = predicted.intervalsClose[,1])
starsTimeRangeClosePredictedPlusErr <- data.frame(stars = starsTimeRangeClose$stars, predicted = predicted.intervalsClose[,2])
starsTimeRangeClosePredictedMinErr <- data.frame(stars = starsTimeRangeClose$stars, predicted = predicted.intervalsClose[,3])

# plot the data together with the fits to open/closed businesses
plot<-ggplot(data=starsTimeRange) + geom_point(aes(x = stars, y = meanTime, color = open, size=maxYears)) + ylab("Avg. Time Open")
plot <- plot + geom_line(data  = starsTimeRangeClosePredicted, aes(x = stars, y = predicted), size = 2)
plot <- plot + geom_line(data  = starsTimeRangeOpenPredicted, aes(x = stars, y = predicted), color='#00B0F6', size = 2)
plot <- plot + geom_line(data  = starsTimeRangeOpenPredictedPlusErr, aes(x = stars, y = predicted), color='grey', size = 1)
plot <- plot + geom_line(data  = starsTimeRangeOpenPredictedMinErr, aes(x = stars, y = predicted), color='grey', size = 1)
plot <- plot + geom_line(data  = starsTimeRangeClosePredictedPlusErr, aes(x = stars, y = predicted), color='grey', size =1)
plot <- plot + geom_line(data  = starsTimeRangeClosePredictedMinErr, aes(x = stars, y = predicted), color='grey', size = 1)
plot + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
######------------------------ end of the modeling and plotting the data -----------------------------------------------######

######----------------------- find the time evolution of the >=4 star and <=3 star businesses---------------------------######

# the input is the name of the type of the business such as badBusiness or goodBusiness, minYear which is the year 
# when the first review occured for the business and the last year, minRating and maxRating variables to find the 
# businesses with min and max average rating
findBusinessesYearRating <- function(typeBusinessName, minYear, maxYear, minRating, maxRating){
  typeBusinessName <- NULL
  for (i in minYear:maxYear){
    typeBusinessName$minYear  <- filter(summaryByIDdatStars, stars >=minRating & stars <= maxRating & substr(as.character(minDate), 1, 4) == as.character(minYear))  
  }
  return (typeBusinessName)
}

#   goodBusiness2004  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2004')  
#   goodBusiness2005  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2005')  
#   goodBusiness2006  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2006')  
#   goodBusiness2007  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2007')  
#   goodBusiness2008  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2008')  
#   goodBusiness2009  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2009')  
#   goodBusiness2010  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2010')  
#   goodBusiness2011  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2011')  
#   goodBusiness2012  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2012')  
#   goodBusiness2013  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2013')  
#   goodBusiness2014  <- filter(summaryByIDdatStars, stars >=4 & substr(minDate, 1, 4) == '2014')  
# 
#   badBusiness2004  <- filter(summaryByIDdatStars, stars <=2 & substr(minDate, 1, 4) == '2004')  
#   badBusiness2005  <- filter(summaryByIDdatStars, stars <=2 & substr(minDate, 1, 4) == '2005')  
#   badBusiness2006  <- filter(summaryByIDdatStars, stars <=2 & substr(minDate, 1, 4) == '2006')  
#   badBusiness2007  <- filter(summaryByIDdatStars, stars <=2 & substr(minDate, 1, 4) == '2007')  
#   
#   avgBusiness2004  <- filter(summaryByIDdatStars, stars >2.5 & stars <= 3.5 & substr(minDate, 1, 4) == '2004')  
#   avgBusiness2005  <- filter(summaryByIDdatStars, stars >2.5 & stars <= 3.5 & substr(minDate, 1, 4) == '2005')  
#   avgBusiness2006  <- filter(summaryByIDdatStars, stars >2.5 & stars <= 3.5 & substr(minDate, 1, 4) == '2006')  
  
# find the time evolution of the business, given a specific business   
findTimeEvolution <- function(goodBusiness) {
  timeEvolution <- NULL
  businessId <- NULL
  businessId <- filter(reviews, business_id %in% goodBusiness$business_id)
  businessId <- select(businessId, c(1,4,5))
  businessId$date <- as.character(businessId$date)
  businessId <- mutate(businessId, year = substr(date, 1, 4) )
  businessIdByYear <- group_by(businessId, business_id, year)
  businessIdByYear <- summarize(businessIdByYear, mean_stars=mean(stars))
  return(businessIdByYear)
}

# goodBusiness2004TimeEvolution <- findTimeEvolution(goodBusiness2004)
# goodBusiness2005TimeEvolution <- findTimeEvolution(goodBusiness2005)
# goodBusiness2006TimeEvolution <- findTimeEvolution(goodBusiness2006)
# 
badBusiness2004TimeEvolution <- findTimeEvolution(badBusiness2004)
badBusiness2005TimeEvolution <- findTimeEvolution(badBusiness2005)
badBusiness2006TimeEvolution <- findTimeEvolution(badBusiness2006)
badBusiness2007TimeEvolution <- findTimeEvolution(badBusiness2007)
# 
# avgBusiness2004TimeEvolution <- findTimeEvolution(avgBusiness2004)
# avgBusiness2005TimeEvolution <- findTimeEvolution(avgBusiness2005)
# avgBusiness2006TimeEvolution <- findTimeEvolution(avgBusiness2006)

# make a plot of the time evolution of the businesses of the certain category
plotTimeEvolution <- function(dataBusinessEvolution, minDat, maxDate){
  dataBusinessEvolution$year <- as.numeric(dataBusinessEvolution$year)
  ## make a prediction model of the time evolution
  model <- lm(mean_stars ~ poly(year, 3), data=dataBusinessEvolution)
  grid <- with(dataBusinessEvolution, expand.grid(year = seq(minDat, maxDate, by=1), business_id = levels(factor(business_id))))
  grid$mean_stars <- stats::predict(model, newdata = grid)
  # find the standard errors and the error bars to have a 95% precision
  err <- stats::predict(model, newdata=grid, se = TRUE)
  grid$ucl <- err$fit + 1.96 * err$se.fit
  grid$lcl <- err$fit - 1.96 * err$se.fit
  ## plot the data
  plot <- qplot(year, mean_stars, data=dataBusinessEvolution, colour = factor(business_id), size=4) 
  plot <- plot + geom_smooth(aes(ymin = lcl, ymax = ucl),data=grid, stat = 'identity', size=1) + ylab("Avg. Rating (Stars)")
  plot <- plot +geom_smooth(data=dataBusinessEvolution, stat = 'identity', size=0.1) + geom_line(data=grid, size=1)  
  plot + guides(colour = FALSE) + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
}
  ######---------------------------------------------------------------------------------------------------######
