rm(list=ls())
library(plyr)
library(ggplot2)
setwd("/dotomi/tmp/viewability_analysis/")
data <- read.csv("IAS Viewability Data with Dates.csv")

data[,1] <- as.Date(data[,1], format="%m-%d-%Y")
data[,4] <- as.numeric(gsub("%", "", as.character(data[,4])))
data[,4] <- replace(data[,4], is.na(data[,4]), 0)
data[,5] <- as.numeric(gsub(",", "",as.character(data[,5])))
data[,5] <- replace(data[,5], is.na(data[,5]), 0)
data[,6] <- as.numeric(gsub(",", "", as.character(data[,6])))
data[,6] <- replace(data[,6], is.na(data[,6]), 0)
data$Viewed.Impressions <- round(data[,4]*data[,5]/100)


daily_summary <- ddply(data, .(Date), function(x){
   c(length(unique(x$Publisher.ID)),
     length(unique(paste(x$Publisher.ID, x$Placement.ID))),
     sum(x$Total.Impressions),
     sum(x$Measured.Impressions),
     sum(x$Viewed.Impressions),
     sum(x$Measured.Impressions)/sum(x$Total.Impressions),
     sum(x$Viewed.Impressions)/sum(x$Measured.Impressions),
     sum(x$Total.Impressions>=100),
     sum(x$Measured.Impressions>=100),
     sum(x$Total.Impressions>=1000),
     sum(x$Measured.Impressions>=1000)
   )
}) 

names(daily_summary) <- c("Date", "Publishers", "Pub.Placement", "Total.Impressions", "Measured.Impressions", "Viewed.Impressions",
                          "Measured.rate", "Viewed.Rate", "More.100.imps", "More.100.Meas", "More.1000.Imps", "More.1000.Meas")

ggplot(daily_summary, aes(x=Date, y=Publishers))+geom_line()
ggplot(daily_summary, aes(x=Date, y=Pub.Placement))+geom_line()
ggplot(daily_summary, aes(x=Date, y=Total.Impressions))+geom_line(colour="red")+
   geom_line(aes(y=Measured.Impressions), colour="blue")+ 
   geom_line(aes(y=Viewed.Impressions), colour="green") 
   

ggplot(daily_summary, aes(x=Date))+
   geom_line(aes(y=Measured.rate), colour="blue")+ 
   geom_line(aes(y=Viewed.Rate), colour="green") 


pub_data <- ddply(data, .(Publisher.ID, Date), function(x){c(
   length(x$Placement.ID),
   sum(x$Measured.Impressions),
   sum(x$Total.Impressions),
   sum(x$Viewed.Impressions),
   sum(x$Measured.Impressions)/sum(x$Total.Impressions),
   sum(x$Viewed.Impressions)/sum(x$Measured.Impressions),
   sum(x$Total.Impressions>=100),
   sum(x$Measured.Impressions>=100),
   sum(x$Total.Impressions>=1000),
   sum(x$Measured.Impressions>=1000)   
   )})

names(pub_data) <- c("Publisher.ID", "Date", "Placements", "Measured.Impressions",
                     "Total.Impressions", "Viewed.Impressions", "Measured.Rate",
                     "Viewed.Rate", "More.100.Imps", "More.100.Meas",
                     "More.1000.Imps", "More.1000.Meas")

publisher_summary <- ddply(pub_data, .(Publisher.ID), function(x){c(
   length(x$Date),
   mean(x$Placements), min(x$Placements), max(x$Placements),
   mean(x$Measured.Impressions), sd(x$Measured.Impressions),
   mean(x$Total.Impressions), sd(x$Total.Impressions),
   mean(x$Viewed.Impressions), sd(x$Viewed.Impressions),
   mean(x$Measured.Rate), sd(x$Measured.Rate),
   mean(x$Viewed.Rate), sd(x$Viewed.Rate)
)})


data.mod <- subset(data, data$Total.Impressions>=1000)
data.mod <- ddply(data.mod, .(Publisher.ID, Placement.ID), transform, mean.Imps = mean(Total.Impressions), sd.Imps = sd(Total.Impressions), max.Imps= max(Total.Impressions), min.Imps=min(Total.Impressions), days=length(Total.Impressions))
data.mod <- data.mod[order(data.mod$mean.Imps, decreasing=TRUE),]

data.mod <- subset(data.mod, data.mod$days>=20)
data.mod$Measure.Rate <- data.mod$Measured.Impressions/data.mod$Total.Impressions


pub.place.summary <- ddply(data.mod, .(Publisher.ID, Placement.ID), function(x){c(
   max(x$mean.Imps),
   max(x$max.Imps),
   max(x$sd.Imps),
   min(x$min.Imps),
   max(x$days),
   mean(x$Measure.Rate), sd(x$Measure.Rate), max(x$Measure.Rate), min(x$Measure.Rate),
   mean(x$X..In.View), sd(x$X..In.View), max(x$X..In.View), min(x$X..In.View)   
   )   
}) 

names(pub.place.summary) <- c("Publisher.ID", "Placement.ID", "mean.Imps", "max.Imps", "sd.Imps", 
                              "min.Imps", "days", "mean.Measure.Rate", "sd.Measure.Rate",
                              "max.Measure.Rate", "min.Measure.Rate", "mean.In.View", "sd.In.View",
                              "max.In.View", "min.In.View")

 ggplot(pub.place.summary, aes(x=mean.In.View, y=sd.In.View))+ geom_point()
 ggplot(pub.place.summary, aes(x=mean.Imps, y=sd.In.View))+ geom_point()
 ggplot(pub.place.summary, aes(x=min.Imps, y=sd.In.View))+ geom_point()
 ggplot(pub.place.summary, aes(x=days, y=sd.In.View))+ geom_point()
 ggplot(pub.place.summary, aes(x=mean.Measure.Rate, y=sd.Measure.Rate))+ geom_point()
 ggplot(pub.place.summary, aes(x=mean.Measure.Rate, y=mean.In.View))+ geom_point()
 ggplot(pub.place.summary, aes(x=sd.Measure.Rate, y=sd.In.View))+ geom_point()
 pub.place.summary <- pub.place.summary[order(pub.place.summary$sd.In.View, decreasing = TRUE),]


pub <- 67581
place <- 1090028

example <- subset(data.mod, data.mod$Publisher.ID== pub & data.mod$Placement.ID==place)
ggplot(example, aes(x=Date, y=X..In.View))+ geom_line()
ggplot(example, aes(x=Measured.Impressions, y=X..In.View)) + geom_point()


ggplot(data.mod, aes(x=Date, y=X..In.View, colour=paste(Publisher.ID, "-", Placement.ID)))+ geom_line()

t1 <- ddply(data, .(Publisher.ID, Placement.ID), function(x){
   c(length(unique(x$Date)),
     sum(x$Measured.Impressions),
     sum(x$Total.Impressions),
     sum(x$Viewed.Impressions))
})

t2 <- ddply(t1, .(Placement.ID), function(x){length(x$Placement.ID)})

ddply(data, .(Date), function(x){
  c(sum(x$Total.Impressions),
    sum(x$Measured.Impressions),
    sum(x$Viewed.Impressions)/sum(x$Measured.Impressions),
    sum(x$Measured.Impressions)/sum(x$Total.Impressions),
    length(unique(x$Publisher.ID))) 
})


