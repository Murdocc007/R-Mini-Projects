library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)
library(xlsx)


usa.data=read.xlsx("/home/aditya/Desktop/Aditya/stats/Project\ 2/usstatesWTID.xls",sheetIndex = 1)
colnames(usa.data)[6]="top1"
usa.data$State=tolower(usa.data$State)
colnames(usa.data)[3]="state"
usa.data$state=as.factor(usa.data$state)

usa.df <- map_data("state")
colnames(usa.df) [5] <- "state"
usa.df$state=tolower(usa.df$state)
usa.df$state <- as.factor(usa.df$state)
usa.data=usa.data[usa.data$Year==2012,]

temp=data.frame(state=usa.data$state,top=usa.data$top1)
usa.df=join(usa.df,temp,by="state",type="inner")


brks=c(5,10,15,20,25,30,35,40,45,50,55,60,65,70)

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
  labs(title = "Top 1% of the population(2012)", fill = "")	


ggsave(p, file = "/home/aditya/Desktop/Aditya/stats/Project\ 2/usatop2.pdf")