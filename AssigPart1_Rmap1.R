library(maptools) 
library(RColorBrewer) 
library(classInt) 
library(OpenStreetMap) 
library(sp) 
library(rgeos) 
library(tmap) 
library(tmaptools) 
library(sf) 
library(rgdal) 
library(geojsonio)
library(ggplot2)

##https://www.gl-li.com/2018/01/17/create-pie-plots-on-a-map-in-r/ last accessed 28/10/18

install.packages("scatterpie")
install.packages("dplyr")
install.packages("GISTools")
install.packages("tidyverse")

library(tidyverse)
library(GISTools)
library(scatterpie)
library(dplyr)

IOWDataMTW <- read.csv("Part1Data\\mtwiow_csv.csv")

PopCentroidMSOAsSF <- read_shape("Part1Data\\MSOA_2011_EW_PWC.shp", as.sf=TRUE)

qtm(PopCentroidMSOAsSF)

IOWDataMapJoined <- PopCentroidMSOAsSF %>% right_join(IOWDataMTW, by = c("MSOA11CD" = "MSOA_2011_CD"))
qtm(IOWDataMapJoined)

## right join just joins where there is data left join keeps the values and centroids without data joined

iow_osm <- read_osm(IOWDataMapJoined, type = "esri", zoom = NULL)  #osm-bw should be type for BandW
qtm (iow_osm)

qtm(iow_osm) +
  tm_shape(IOWDataMapJoined)+
  tm_bubbles(size = "PeopleTravellingToWork", col = NA, shape = 21, scale = 4/3,
           legend.max.symbol.size = 1) +
  tm_compass(position = c("left", "bottom"),type = "arrow") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Population Travelling to Work", legend.position = c("right", "bottom"))


##??tm_dots 
  