# loading the library
library(rgdal)  
library(ggplot2)
require(maptools)
require(plyr)
install.packages('gdtools')
library(gdtools)
devtools::install_github("hadley/svglite")
require(svglite)
library(data.table)


# First read in the shapefile
setwd('/<setting your path>')
china <- readOGR("china_shapefile/CHN_adm2.shp",layer="CHN_adm2")
plot(chinaB)
china@data
class(china)
ggplot(china)
plot(china)
# china@data
# Clean the data, then fortify to a df
library(rvest)
install.packages("leafletCN")
library("leafletCN")

mergeCity <- read.csv(file="../dfMergeCity.csv", header=TRUE, sep=",")
mergeCity
df2<-read.csv(file="../df2focusCity.csv", header=TRUE, sep=",")
nrow(df2)

final<-read.csv(file="../df2focusCity.csv", header=TRUE, sep=",")
pop<-read.csv(file='../citypop2.csv', encoding='utf8',header=TRUE, sep=",")
df3 <- merge(final, pop, by.x = "Loc1", by.y = "City")
head(df3,3)

cityLevelMap<-read.csv(file="../cityLevelMap.csv", header=TRUE, sep=",")

names(cityLevelMap)

# 1. calculate the density with pop standralization

#1.1 chi-sq exam
mergeCity2<-mergeCity
mergeCity2['density']<-(mergeCity2['Gender'])^2/(mergeCity2['Pop']*100)
mergeCity3<-mergeCity2[c('City','density','cityLevel')]
mergeCity4<-mergeCity2[c('City','cityLevel')]

summary(mergeCity3$density)
mergeCity3

newdata <- mergeCity3[order(mergeCity3['density']),] 
newdata
#mapping to different catagroy
newvar<-0
recode<-function(variable,Xhigh,high,medium,low){
  newvar[variable<=Xhigh]<-"Xhigh"
  newvar[variable<=high]<-"High"
  newvar[variable<=medium]<-"Medium"
  newvar[variable<=low]<-"Low"
  
  return(newvar)
}

summary(mergeCity3$density)
mergeCity3$densityRecode <- recode(mergeCity3$density,8.3,6,1,0.048370)
chisq<-chisq.test(mergeCity3$densityRecode,mergeCity3$cityLevel)
chisq$observed
chisq$expected
chisq$statistic
chisq$p.value
nrow(mergeCity3)
write.csv(mergeCity3, file = "Q1.csv")
mergeCity3

# 1.2 plot the interactive density view
png("rplot.png", width = 350, height = "350")

names(mergeCity3)

if(require(leaflet)){
  dat = data.frame(regionNames("city"),
                   runif(384))

  map = leafletGeo("city", mergeCity3)
  pal <- colorNumeric(
    # palette = "RdPu",
    palette = "Reds",
    domain = map$value)
  leaflet(map) %>% 
     # addTiles() %>% 
     # addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addPolygons(stroke = FALSE,
                smoothFactor = 2,
                fillOpacity = 0.5,
                weight = 1,
                color = ~pal(value),
                popup = ~htmltools::htmlEscape(popup)
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "User Density",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)
}
dev.copy(png,'myplot.png')
dev.off()

dat=data.frame(city = mergeCity3['City'], density= mergeCity3['density'])
geojsonMap(dat, "city",
            dat%>% addTiles() %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
           stroke = T,
           fillOpacity = 0.7,
           popup =  paste0(dat$city,":",dat$density),
           palette = "Reds", legendTitle = "Density")

m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)

#2. proximity preference and network analysis
#2.2 network analysis for across city flow 
network<-read.csv(file='../igraphData.csv', encoding='utf8',header=TRUE, sep=",")
network
networkdf<-network[c('From','To','Weight')]
networkdf

#Katz_centrality
# Code reference to the doc:
# https://networkx.github.io/documentation/networkx-1.10/reference/algorithms.html
KCentral<-nx.katz_centrality(gd, directed = TRUE, weights=E(gd)$weight)$vector
kCentral<-as.data.frame(eCentral)
nrow(kCentral)


eCentral2<-eigen_centrality(gd, directed = TRUE, scale = TRUE, weights =E(gd)$weight)
eCentral2

#write the centrality result in csv file
write.table(kCentral,file="kCentral1.csv", quote=F,sep=",",row.names=T)

class(eCentral)
list(eCentral)
toList(eCentral, keepNA = TRUE)

write.list(z, eCentral, t.name = NULL, row.names = FALSE)


install.packages('centiserve')
library(centiserve)
katzcent(gd, vids = V(gd), alpha = 0.1)

data("zachary")
class(zachary)
class(gd)
zachary
gd

#network visualidation
library(igraph)  
gd <- graph.data.frame(networkdf) 
length(gd)
plot(gd, vertex.shape='sphere',vertex.size=6, edge.arrow.size=.2,edge.curved=.1,edge.arrow.width=.5,
     vertex.label=NA,layout=layout.fruchterman.reingold,edge.width=E(gd)$route/6) 

gd

mat=as.matrix(networkdf)
mat
net=graph.adjacency(mat,mode="directed",weighted=TRUE,diag=FALSE) 

plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/3, edge.arrow.size=1.5)


warnings()
library(magrittr)
install.packages('visNetwork')
library(magrittr)

library(visNetwork)
library(data.table)

graph <- graph.data.frame(networkdf, directed=T)
graph <- simplify(graph)
graph

V(graph)$indegree <- centr_degree(graph, mode = "in")$res
V(graph)$indegree <- centr_degree(graph, mode = "in")$res

nodes <- get.data.frame(graph, what="vertices")
nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$indegree, indegree = nodes$indegree)
setnames(nodes, "indegree", "in-degree centrality")
nodes <- nodes[order(nodes$id, decreasing = F),]
graph
edges <- get.data.frame(graph, what="edges")[1:2]

plot(graph, vertex.shape='sphere',vertex.size=6, edge.arrow.size=.2,edge.curved=.1,edge.arrow.width=.5,
     vertex.label=NA) 


#question2.1 log regression
dataClean<-read.csv(file='../dataClean.csv', encoding='utf8',header=TRUE, sep=",")
dataClean['StatusCoded']
nrow(dataClean)
head(dataClean,8)
cleanTest<-dataClean
head(cleanTest,1)

#coded the explainary variables
cleanTest$SalaryCoded<- factor(cleanTest$Salary,levels =c('5000～10000元', '10000～20000元', '2000～5000元', '2000元以下',
                                '20000～50000元', '50000元以上' ),
                      labels = c(1,2,3,4,5,6))

cleanTest$EduCoded<-factor(cleanTest$Education,levels =c('高中中专及以下', '本科', '硕士', '大专', '双学士', '博士'),
                           labels = c(1,2,3,4,5,6))

cleanTest$PicRCoded<-factor(cleanTest$PicR,levels =c('不限', '有照片', '有照片\xa0*'),
                           labels = c(1,2,3))


cleanTest$StatusCoded<-factor(cleanTest$Status,levels =c('未婚', '离异,有小孩归对方', '离异', 
                                                         '离异,有小孩归自己', '丧偶', '丧偶,有小孩归自己', '离异,无小孩',
                                                         '丧偶,有小孩归对方', '丧偶,无小孩'),
                            labels = c(1,4,5,6,7,8,9,10,11))
cleanTest$StatusCoded<-as.character(cleanTest$StatusCoded)
# labels = c(1,3,2,3,2,3,2,3,2))

cleanTest$StatusCoded[cleanTest$StatusCoded =='4']<-3
cleanTest$StatusCoded[cleanTest$StatusCoded =='5']<-2
cleanTest$StatusCoded[cleanTest$StatusCoded =='6']<-3
cleanTest$StatusCoded[cleanTest$StatusCoded =='7']<-2
cleanTest$StatusCoded[cleanTest$StatusCoded =='8']<-3
cleanTest$StatusCoded[cleanTest$StatusCoded =='9']<-2
cleanTest$StatusCoded[cleanTest$StatusCoded =='10']<-3
cleanTest$StatusCoded[cleanTest$StatusCoded =='11']<-2

#changing to factor after grouping
cleanTest$StatusCoded<-as.factor(cleanTest$StatusCoded)

cleanTest['StatusCoded']

junk$nm[junk$nm == "B"] <- "b"

head(cleanTest,3)
logTable<-cleanTest[c('Age','Gender','Car','House','cityLevel','StatusCoded',
                      'SalaryCoded','EduCoded','PicRCoded','Proximity')]


logTable$Proximity.summary()
install.packages("Zelig", lib = "~/Library/R/library")
library(Zelig)
install.packages("zeligverse")
library(zeligverse)

#log test run here
logicModel<- zelig(as.factor(Proximity)~Age+Gender+Car+House+cityLevel+StatusCoded+SalaryCoded+EduCoded+PicRCoded,
                   model = "ologit", data =logTable)
summ<-summary(logicModel)
coef(summ)[5:6,4] 
# calcualte the p-value
summ<- coef(summ)[]


ctable<-coef(logicModel)
ctable

#ctable<-logicModel$coefficients
ctable
p<- pnorm(abs(ctable[ ,"t value"]), lower.tail = FALSE)*2
(ctable<- cbind(ctable, "p value"=p))

#potential example
#perform ordinal logistic regression
as.factor(logTable$cityLevel)

logTable$cityLevelCoded<-factor(logTable$cityLevel,levels =c('1', '2','3','4' ),
                              labels = c(1,2,3,4))
z<-polr(as.factor(Proximity)~Age+Gender+Car+House+cityLevelCoded+StatusCoded+SalaryCoded+EduCoded+PicRCoded, data = logTable, Hess=TRUE, method="logistic")
summary(z)
#present results with no p-value
(ctable <- coef(summary(z)))
#Get p-values
p<-pnorm(abs(ctable[,"t value"]), lower.tail=FALSE)*2
(ctable<-cbind(ctable, "pvalue" = p))
ctable
