library(rgdal)  
library(ggplot2)
require(maptools)
require(plyr)
require(svglite)
install.packages("svglite")
# First read in the shapefile

#set your own path
setwd('/')
list.files('/')
china = readOGR(dsn=("./china_shapefile/"), layer="bou2_4p")

#test for drawing china sp
# Clean the data, then fortify to a df
china@data$id = rownames(china@data)
china.points = fortify(china, region="id")
china.df = join(china.points, china@data, by="id")
china.df
# Dont draw the islands in south china sea
china.df = subset(china.df, AREA > 0.005)

map = ggplot(china.df, aes(x=long, y=lat, group=group)) + 
             geom_polygon(fill="white") +
             geom_path(color="gray", size=0.2) +
             coord_map()

# Count the frequency of each city, remove duplicates
city_df = raw_df[c("city", "lat", "long")]
city_df = unique(ddply(city_df,.(city, lat, long),nrow))
colnames(city_df) = c("city", "lat", "long", "count")
head(city_df)

city_df

# Standard plot the map, then save to a png file
plot1 = map + geom_point(data = city_df, aes(x=long, y=lat, size=count), 
                         inherit.aes=FALSE, shape=21, stroke=0, 
                         colour="white", fill="red", alpha=0.3) +
              scale_size_continuous(name = "Counts", range = c(1, 25)) + 
              # labs(title =None ) +
              xlab("Longitude") + ylab("Latitude") + 
              theme(plot.title = element_text(size=22),
                    axis.title = element_text(size=18),
                    legend.title = element_text(size=18))
ggsave("plottest1.png", plot = plot1, device = "png",
        scale = 1, width = 9, height = 9, units = "in")
ggsave("plot1.png", plot = plot1, device = "png",
        scale = 1, width = 9, height = 9, units = "in", dpi = 300)

# Plot without background and legends, etc.
plot2 = ggplot(china.df, aes(x=long, y=lat, group=group)) + 
        geom_polygon(fill="white") +
        geom_path(color="gray", size=0.3) +
        coord_map() +
        geom_point(data = city_df, aes(x=long, y=lat, size=count), 
                         inherit.aes=FALSE, shape=21, stroke=0, 
                         colour="white", fill="red", alpha=0.3) +
        scale_size_continuous(name = "Counts", range = c(1, 25)) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
ggsave("plottest2.png", plot = plot2, device = "png",
       scale = 1, width = 9, height = 9, units = "in")

#getting the coordinate of target location
#selection of the cities of non-zero centrality 
library(devtools)
install_github('lchiffon/REmap')
library(REmap)
get_city_coord("北京")
lalon<-get_geo_position(c("北京","上海","成都","重庆","深圳","长沙","河北","烟台", 
                  "金华","常州" ,"福州" ,"雅安" ,"绵阳" , "杭州","广西" ,"哈尔滨",
                   "河南" , "娄底","合肥" ,  "广州" , "江西","南京" ,"厦门","温州", 
                   "苏州", "乐山","太原","汕尾","河源","荆州","襄阳","贵阳",  
                   "柳州", "黄冈","常德","湘潭","邵阳","石家庄","惠州","江苏",  
                   "遵义","湖北","山东","泉州","青海", "嘉兴","亳州", "汕头",  
                   "海口","清远","韶关", "湛江","铜仁","防城港","滁州","邯郸", 
                  "上饶","九江","淮北", "中山", "昆明","晋城" ,"阳泉", "驻马店",
                   "黄石", "岳阳","张家口", "徐汇","信阳","武汉","海南","潍坊",
                   "内蒙古","宿州"))

nrow(lalon)
nrow(kat)
lalon
names(kat) <- c("city", "value")

merge<-merge(x = kat, y = lalon, by = "city", all = TRUE)
merge
kat <- read.csv(file="CentralPlot.csv", header=TRUE, sep=",")
listK<-kat['kCentral']
listK

as.factor(merge$count)
class(merge$count)

merge$long<- as.numeric(as.character(merge$long))
merge$count <-as.numeric(as.character(merge$count))
merge$lat<- as.numeric(as.character(merge$lat))



ggplot(china.df, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_path(color="gray", size=0.3) +
  coord_map() +
  geom_point(data = merge, aes(x=long, y=lat, size=count*2,label=city), 
             inherit.aes=FALSE, shape=21, stroke=0, 
             colour="white", fill="red", alpha=0.3) +
  geom_text()+
  scale_size_continuous(name = "value", range = c(1, 25)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

ggsave("plottest4.png", plot = plot4, device = "png",
       scale = 1, width = 9, height = 9, units = "in")

#directly use merge table if imported
write.table(merge,file="mergePointPlot.csv", quote=F,sep=",",row.names=T)

