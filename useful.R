### This file is useful for data preprocessing
####
library(jsonlite)
library(lubridate)
library(doParallel)
library(dplyr)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(tm)

#dataBusiness<-llply(as.list("yelp_academic_dataset_business.json"),function(x) stream_in(file(x),pagesize=1000))
#saveRDS(dataBusiness, "dataBusiness.rds")
dataBusiness <- readRDS("dataBusiness.rds")

# some exploratory analysis, looking at classes, names, unique values
dataBusiness<-as.data.frame(dataBusiness)
businessNames<-sapply(dataBusiness,class)
businessCat<-unlist(dataBusiness$categories)
uniqueCat<-unique(businessCat)

## selecting the data for the businesses that are closed and open
closedStars<-dataBusiness[dataBusiness$open == F,]$stars
openStars<-dataBusiness[dataBusiness$open == T,]$stars

hist(openStars, col=rgb(1,0,0,0.5), main="Overlapping Histogram",breaks = 9)
hist(closedStars, col=rgb(0,0,1,0.5), add=T,breaks=9)

## selecting only data with numeric input
newDataBusiness <- data.frame(open=as.numeric(dataBusiness$open), review_count=dataBusiness$review_count, 
                              longitude = dataBusiness$longitude, stars = dataBusiness$stars, latitude = dataBusiness$latitude)
newDataByStarsOpen <- group_by(newDataBusiness, stars, open)

### sum of the reviews by stars and open/close business category
reviewCountByStarsOpen<-summarize(newDataByStarsOpen, sum(review_count))
names(reviewCountByStarsOpen)<-c("stars", "open", "review_count")

reviewCountByStarsClosed<-filter(reviewCountByStarsOpen, open == 0)
reviewCountByStarsOpen<-filter(reviewCountByStarsOpen, open == 1)

## counting the number of stars for open/close businesses
starsOpen <- group_by(dataBusiness[dataBusiness$open == 1,], stars)
starsOpen<-summarize(starsOpen,n())
names(starsOpen) = c("stars", "count")

starsClose <- group_by(dataBusiness[dataBusiness$open == 0,], stars)
starsClose<-summarize(starsClose,n())
names(starsClose) = c("stars", "count")

### find the number of reviews by stars for closed/open, normalized by the number of stars for close/open business
countsPerReviewPerStar <- data.frame(stars=starsClose$stars,close=reviewCountByStarsClosed$review_count/starsClose$count,
                                     open=reviewCountByStarsOpen$review_count/starsOpen$count)

ratioOpenClosed <- data.frame(stars=countsPerReviewPerStar$stars, ratio=countsPerReviewPerStar$close/countsPerReviewPerStar$open*100)

countsPerReviewPerStar<-melt(countsPerReviewPerStar,id=c("stars"))
plot <- ggplot(countsPerReviewPerStar,aes(x=stars, y=value, fill=variable)) + geom_bar(stat="identity",position="dodge",alpha=0.4)
plot 

## stemming and stripping document

dataTip<-readRDS("dataTip.rds")
tipText<-dataTip$text

tipText<-sapply(tipText,stemDocument)
tipText<-sapply(tipText,tolower)
tipText<-sapply(tipText,function(x) removeWords(x, stopwords("english")))

## finding the businesses with both tip and business files

bids <- intersect(dataTip$business_id, dataBusiness$business_id)
businessSelectedCat <- dataBusiness[, -c(3,14)]
busienssWTips <- subset(dataBusiness, business_id %in% bids)
TipByBusiness <- group_by(dataTip, business_id)
businessTipDate <-  cbind(summarize(TipByBusiness, min(date)), summarize(TipByBusiness, max(date)[2]))
names(businessTipDate) <- c("business_id", "min_date", "max_date")
