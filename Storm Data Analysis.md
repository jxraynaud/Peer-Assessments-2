ECONOMICAL AND HEALTH IMPACT OF WEATHER EVENTS IN THE US. 
========================================================

## Synopsis

With Global Warning there is a risk that weather activity grows. Thus it's important, for proper emergency planning to asses the damage both on health and on economy caused by the different types of events. This analysis focus on assessing the damage potential of the weather events broken down by type.

## Data Processing


```r
## We first check to see if the file containing the raw data is in our working directory
## If it isn't we download it.
if(!file.exists("StormData.csv.bz2")){
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="StormData.csv.bz2", method="auto")
        daydl <- as.character(Sys.Date()) 
        timedl <- strsplit(as.character(Sys.time()), " ")[[1]][2]
        write.table(c(daydl,timedl),"datedl.txt",sep=" ")    # file used to save the date of the DL
        rm(daydl,timedl)
} 
bz <- bzfile("StormData.csv.bz2") # Can't read this directly as a Data Table... Snif.
StormData <- read.csv(bz)
rm(bz) 
```

The Dataset was downloaded the 2014-07-27 at 22:59:56 CEST. It contained  902297 observations of 37 variables.

[https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf](Here) you can find the codebook for this original dataset.

To assess the economical impact of the events we have two column :
_ PROPDMG with the value of the damage made to properties
_ CROPDMG with the value of the damage made to crops

Each damage type has an additionnal multiplier in columns PROPDMGEXP and CROPDMGEXP with a lot of possible value. The 3 most noticable values are : K, M, B, meaning thousands, millions, billions. We need to rationalise this by setting the same scale to all the values.  

In order to assess the economical impact we are going to create a new column named EcoImpact and calculate it's value by additing the two type of damages properly scaled.


```r
StormData$EcoDmg = StormData$PROPDMG * (10^3 * (StormData$PROPDMGEXP %in% "M") + 10^6 * (StormData$PROPDMGEXP %in% "B")) + StormData$CROPDMG * (10^3 * (StormData$CROPDMGEXP %in% "M") + 10^6 * (StormData$CROPDMGEXP %in% "M"))
```

To assess the health impact we are going to sum the column FATALITIES and the column INJURIES in one column : HealthDmg


```r
StormData$HealthDmg =  StormData$FATALITIES + StormData$INJURIES
```



The data column EVTYPE which describe the events is pretty messy :
- Upper and lower case used indiscrimenaly
- "s" added to some description for example you can find "urban Flood" and "URBAN FLOODS"

The first thing we are going to do is simply lower the case of all factor in EVTYPE.

```r
StormData$EVTYPE <- factor(tolower(as.character(StormData$EVTYPE)))
```

Then we are going to look at the 10 type of Events with the most economical and human impact using the mean function.


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.1
```

```r
# Get the health damage aggregation
HealthDmg <- aggregate(HealthDmg~EVTYPE, data=StormData, mean)
HealthDmg <- HealthDmg[order(HealthDmg$HealthDmg, decreasing=TRUE),]
# Get the 10 most dangerous EVTYPE
evtype1 <- as.character(HealthDmg[1:10,"EVTYPE"])

#Get the economic damage aggregation
EcoDmg <- aggregate(EcoDmg~EVTYPE, data=StormData, mean)
EcoDmg <- EcoDmg[order(EcoDmg$EcoDmg, decreasing=TRUE),]
# Get the 10 most dangerous EVTYPE
evtype2 <- as.character(EcoDmg[1:10,"EVTYPE"])

# Merge the 2 vectors.
evtype <- c(evtype1,evtype2)

# Subset the initial dataset to get only the selected factors
StormData <- StormData[StormData$EVTYPE %in% evtype,]
```


## Results

# Health Impact

For the selected events, let's compare the sum of the fatalities and injuries :


```r
library(ggplot2)

HealthDmg <- aggregate(HealthDmg~EVTYPE, data=StormData, mean)
print(ggplot(HealthDmg, aes(x = EVTYPE, y = HealthDmg)) + geom_bar(stat = "identity", 
    colour = "black", fill = "lightblue") + geom_text(aes(label = HealthDmg), 
    vjust = 1.5) + labs(x = "Event Type", y = "Fatalities+injuries (Mean)") + ggtitle("Fatalities & injuries by Event Type") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1, colour = "black"), 
        plot.title = element_text(face = "bold", size = 18), axis.title = element_text(face = "bold", 
            size = 14)))
```

![plot of chunk healthdiagrm](figure/healthdiagrm.png) 

We can notice that some events with a big economical impact have no health impact.

# Economical Impact

For the selected events, let's compare the economical impact :


```r
library(ggplot2)

EcoDmg <- aggregate(EcoDmg~EVTYPE, data=StormData, mean)
print(ggplot(EcoDmg, aes(x = EVTYPE, y = EcoDmg)) + geom_bar(stat = "identity", 
    colour = "black", fill = "lightblue") + geom_text(aes(label = EcoDmg), 
    vjust = 1.5) + labs(x = "Event Type", y = "Impact in k$ (Mean)") + ggtitle("Economical Impact by Event Type") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1, colour = "black"), 
        plot.title = element_text(face = "bold", size = 18), axis.title = element_text(face = "bold", 
            size = 14)))
```

![plot of chunk ecodiagrm](figure/ecodiagrm.png) 




