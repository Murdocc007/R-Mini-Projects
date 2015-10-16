library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)
library(xlsx)

#reading teh data
usa.data=read.xlsx("/home/aditya/Desktop/Aditya/stats/Project\ 2/usstatesWTID.xls",sheetIndex = 1)

#renaming columns
colnames(usa.data)[6]="top1"
usa.data$State=tolower(usa.data$State)
colnames(usa.data)[3]="state"
usa.data$state=as.factor(usa.data$state)

usa.df <- map_data("state")
colnames(usa.df) [5] <- "state"

#convert to lower case
usa.df$state=tolower(usa.df$state)
usa.df$state <- as.factor(usa.df$state)

#getting data for both the years
usa.data2012=usa.data[usa.data$Year==2012,]
usa.data1999=usa.data[usa.data$Year==1999,]

#merging the data by states
usa.data=merge(usa.data1999,usa.data2012,by="state")

#making another column withing the data frame that calculates the difference between the top 1%
usa.data=within(usa.data,{difference=abs(top1.x-top1.y)})[,c("state","difference")]
usa.data=usa.data[,c("state","difference")]
colnames(usa.data)[2]="top1";

#making the join between final (1%) data frame and state data fram
temp=data.frame(state=usa.data$state,top=usa.data$top1)
usa.df=join(usa.df,temp,by="state",type="inner")

#brks=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3)
brks=c(1,2,3,4,5,6)

#data frame that consists of a tupe(long,lat)~state name. Long and Lat are mean of the range of corresponding values
cnames <- aggregate(cbind(usa.df$long, usa.df$lat) ~ usa.df$state, data=usa.df, 
                    FUN=function(x)mean(range(x)))

colnames(cnames)[1]="state"
colnames(cnames)[2]="long"
colnames(cnames)[3]="lat"

p=ggplot()+
  geom_polygon(data = usa.df, aes(x = long, y = lat, group = group, fill = top), 
               color = "black", size = 0.15)+
  geom_text(data=cnames,aes(x=long, y=lat , label=state),size=2)+
  scale_fill_distiller(palette = "Reds", breaks = brks, trans = "reverse") +
  theme_nothing(legend = TRUE) +
  labs(title = "Difference between top 1% of the population(2012-1999)", fill = "")	

#saving the file
ggsave(p, file = "/home/aditya/Desktop/Aditya/stats/Project\ 2/usatop4.pdf")