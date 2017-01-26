#### Finland distribution of population ####

#### Libraries ####
library(ggplot2)
#library(ggmap)
library(XML)
library(igraph)

#### Read borders of municipalities (kml-file, gps-data) ####
# https://raw.githubusercontent.com/tomimick/mapcolorizer/master/data-finland/data-raw-geo/kunnat.kml
download.file('https://raw.githubusercontent.com/tomimick/mapcolorizer/master/data-finland/data-raw-geo/kunnat.kml','kunnat.kml.xml')
xml_data <- xmlParse('kunnat.kml.xml')
xml_data <- xmlToList(xml_data)
#xml_data <-xmlParse('data/kunnat.xml')

#### modify xml_data to more simple ####

source("functions/xmlHandler.R")

counties <- xmlToData(xml_data)
counties <- data.frame(counties)

#### Merge population data to border data ####
popul <- read.csv('data/kunnat_vakiluvut2014.csv',sep=';')

countiesPop <- merge(counties,popul,by='name')

#### County data to data-frame with lat lon coordinates border points for each county ####
source('functions/coordinateHandler.R')

countiesD <- countiesToFrame(countiesPop)
countiesD$divider <- as.character(0)
#### Center points of counties ####
source('functions/helpers.R')

c <- countiesPop[!duplicated(countiesPop$name),]
countiesWithCenter <- addCenterPoints(c)

#### Distances between centers ####

distM <- countDistMatrix(countiesWithCenter)
popu <- sum(popul$popul)

getPlotFin <- function(countiesBorders, countiesWithC, divider,county=NA){
  
  popula <- sum(countiesWithC$popul)
  divida <- takeToRekPop(countiesWithC, kunta0=county,divid=divider,popu = popula)
  
  div <- addDivider2(countiesBorders,divida)
  
  p <- ggplot() + geom_polygon(data = div, aes(x=long,y=lat,group=group,fill=divider)) +
    coord_fixed(1.3)
  
  return(p)
}

####Plots ####
p6 <- getPlotFin(countiesD,countiesWithCenter,6)

p6 + theme_nothing() +  scale_fill_manual(values=c("green","red4","red1","royalblue3","royalblue1","green3"))
#893800
#901089
#937774
#912388
#897409
#857540


p8 <- getPlotFin(countiesD,countiesWithCenter,8)

p8 + theme_nothing() +  scale_fill_manual(values=c("green","red4","red1","royalblue3","royalblue1","gold3","gold","green3"))
# 612664
# 702368
# 683338
# 734293
# 689975
# 677613
# 677699
# popu - summa(yot) = n.622000

####Testing below this####
dividedSuomi3b <- takeToRekPop(countiesWithCenter,divid = 3,popu = popu)

divByJoensuu <- takeToRekPop(countiesWithCenter,kunta0="Joensuu",divid = 3,popu=popu)
divByTurku <- takeToRekPop(countiesWithCenter,kunta0="Turku",divid = 3,popu=popu)

div3b <- addDivider2(countiesD,dividedSuomi3b)
div3Jns <- addDivider2(countiesD,divByJoensuu)
div3Turku <- addDivider2(countiesD,divByTurku)

ggplot() + geom_polygon(data=div3b,aes(x=long,y=lat,group=group,fill=divider)) +
  coord_fixed(1.3) + theme_nothing() +  scale_fill_manual(values=c("green","red","blue"))

ggplot() + geom_polygon(data=div3Jns,aes(x=long,y=lat,group=group,fill=divider)) +
  coord_fixed(1.3) + theme_nothing() +  scale_fill_manual(values=c("green","red","blue"))

ggplot() + geom_polygon(data=div3Turku,aes(x=long,y=lat,group=group,fill=divider)) +
  coord_fixed(1.3) + theme_nothing() +  scale_fill_manual(values=c("green","red","blue"))

#### Neigbouring counties ####
source('functions/helpers2.R')

neighboursCsv <- read.csv('data/naapurit_valmis.csv',sep=';',header = F)

neighboursMatrix <- makeNaapuriMat(neighboursCsv)

singles <- singleNeighbours(neighboursMatrix)



dividedSuomi3 <- takeToPop(counties0 = popul,divid=3)
dividedSuomi3 <- as.data.frame(dividedSuomi3)

div3 <- addDivider(dat = countiesD,dividedSuomi3)


#ggplot() + geom_polygon(data=div3,aes(x=long,y=lat,group=group,fill=divider)) +
#  coord_fixed(1.3) + theme_nothing() + scale_fill_continuous(low="green",high = "red")

ggplot() + geom_polygon(data=div3,aes(x=long,y=lat,group=group,fill=divider)) +
  coord_fixed(1.3) + theme_nothing() +  scale_fill_manual(values=c("green","red","blue"))
