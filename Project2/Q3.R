library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)
library(xlsx)

#reads the data from the xls file
HappyIndexData=read.xlsx("/home/aditya/Desktop/Aditya/stats/Project\ 2/hpi-data-clean.xlsx",sheetIndex = 1)

#plots the histogram of the Happy Index Data
hist(HappyIndexData$Happy.Planet.Index,probability = F,col=gray(0.9))

# Calculation Standard Deviation
sd(HappyIndexData$Happy.Planet.Index)
#[1] 9.111629

median(HappyIndexData$Happy.Planet.Index)
#[1] 41.97981

IQR(HappyIndexData$Happy.Planet.Index)
#[1] 12.84922

#makes the boxplot of Happy Planet Index
boxplot(HappyIndexData$Happy.Planet.Index,ylab="Happy Index")

#makes the boxplot of life expectancy versus happy planet index
boxplot(Happy.Planet.Index ~Life.Expectancy,data=HappyIndexData,xlab="LIFE EXPECTANCY",ylab=" HAPPY INDEX")



#makes the scatter plot of life expectancy vs happy planet index
plot(HappyIndexData$Life.Expectancy,HappyIndexData$Happy.Planet.Index,xlab="happy planet index",ylab="life expectancy")
abline(lm(HappyIndexData$Happy.Planet.Index~HappyIndexData$Life.Expectancy ),col="red")

#makes the scatter plot of footprint vs happy planet index
plot(HappyIndexData$Footprint,HappyIndexData$Happy.Planet.Index,xlab="happy planet index",ylab="Footprint")
abline(lm(HappyIndexData$Happy.Planet.Index~HappyIndexData$Footprint ),col="red")

#makes the scatter plot of happy life years vs happy planet index
plot(HappyIndexData$Happy.Life.Years,HappyIndexData$Happy.Planet.Index,xlab="happy planet index",ylab="happy life years")
abline(lm(HappyIndexData$Happy.Planet.Index~HappyIndexData$Happy.Life.Years ),col="red")

#makes the scatter plot of well being vs happy planet index
plot(HappyIndexData$Well.being,HappyIndexData$Happy.Planet.Index,xlab="happy planet index",ylab="Well Being")
abline(lm(HappyIndexData$Happy.Planet.Index~HappyIndexData$Well.being ),col="red")


cor(HappyIndexData$Happy.Planet.Index,HappyIndexData$Well.being)
#[1] 0.4530088

cor(HappyIndexData$Happy.Planet.Index,HappyIndexData$Life.Expectancy)
#[1] 0.5109242

cor(HappyIndexData$Happy.Planet.Index,HappyIndexData$Footprint)
#[1] -0.2382588

cor(HappyIndexData$Happy.Planet.Index,HappyIndexData$Happy.Life.Years)
#[1] 0.5013601



