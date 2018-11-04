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

#df <- PopCentroidMSOAsSF

## need to get the xy added to the table so we have xy for the scatterpie  
## had to prepare the extra columns in Arc for now... could not get separate to work or other function
## to work with adding further columns like an index to join

PopCentroidMSOAsArcXYSF <- read_shape("Part1Data\\MSOA_2011_EW_PWC_ArcXY.shp", as.sf=TRUE)


#df <- df %>%
#  separate(col = geometry, into = c('X', 'Y'), sep = '\\,')

qtm(PopCentroidMSOAsSF)

IOWDataMapJoined <- PopCentroidMSOAsSF %>% right_join(IOWDataMTW, by = c("MSOA11CD" = "MSOA_2011_CD"))
qtm(IOWDataMapJoined)

## do data join with IOW Map with xy
IOWDataMapJoinedXY <- PopCentroidMSOAsArcXYSF %>% right_join(IOWDataMTW, by = c("MSOA11CD" = "MSOA_2011_CD"))
qtm(IOWDataMapJoinedXY)

## right join just joins where there is data left join keeps the values and centroids without data joined

iow_osm <- read_osm(IOWDataMapJoinedXY, type = "esri", zoom = NULL)  #osm-bw should be type for BandW
qtm (iow_osm)

qtm(iow_osm) +
  tm_shape(IOWDataMapJoinedXY)+
  tm_bubbles(size = "PeopleTravellingToWork", col = NA, shape = 21, scale = 4/3,
           legend.max.symbol.size = 1) +
  tm_compass(position = c("left", "bottom"),type = "arrow") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Population Travelling to Work", legend.position = c("right", "bottom"))

##tm_view()
##tm_view adjust the zoom level - a bit complicated can't just zoom out a bit... 
  ##maybe set the zoom extent if the shape was a polygon around the Isle of Wight.. or IOW county, 
  ##cant easily decide your bounding box
  
##??tm_dots 
  
  ##now to attempt the scatter pie... I have the data in and joined.
  ##https://www.gl-li.com/2018/01/17/create-pie-plots-on-a-map-in-r/
  
  head(IOWDataMapJoined)
  
  ##WFH TubeLightRail Train Bus Taxi Motorbike CarVan Passenger Bicycle OnFoot OtherMTW
  
  ## I need to get the geometry column as x and y - SEE ABOVE WITH SEPARATE... neither working
  ##https://github.com/r-spatial/sf/issues/231
  
  ##MSOA_coords <- do.call(rbind, st_geometry(PopCentroidMSOAsSF)) %>% 
    ##as_tibble() %>% setNames(c("X","Y"))
  
  
  selection <- IOWDataMapJoinedXY[,c(3,4,8,9,10,11,12,13,14,15,16,17,18,19,20)]
  class(selection)
  
  sapply(selection, mode)
  
  nogeom = st_set_geometry(selection, NULL)  ## the data frame could only have numerics no list remove geom to work
  sapply(nogeom, mode)
  
  # This chunk works
  # ggplot() + ##data = largest  ##IOWDataMapJoinedXY
  #   geom_scatterpie(data=nogeom, 
  #                   aes(x=X,y=Y, r = PeopleTravellingToWork/5), ## had to be a bit experimental of the r
  #                   cols = c("Tube.LightRail", "Train", "Bus", "Taxi", "Motorbike", "CarVan", "Passenger", "Bicycle", "OnFoot", "OtherMTW"))+
  #                   coord_fixed() 
  
 ## https://boundingbox.klokantech.com/ i want a bounding box slightly bigger than my msoa data
 ## https://www.linkedin.com/pulse/plot-over-openstreetmap-ggplot2-abel-tortosa-andreu/
   LON1 = -1.594502 ; LON2 = -1.062719 
   LAT1 = 50.574677 ; LAT2 = 50.779883
  
  
  map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL,
                 type = c("stamen-toner")[1],
                 mergeTiles = TRUE)
  
#change projection to BNG to align with other data
  map.bng <- openproj(map,projection = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy")
  
  #OSMMAPBNG <- autoplot(map.bng)
  
  #OSMMAPBNG
  #autoplot()
  
  #ggplot() +
  
  ##autoplot can work  if you remove the ggplot() object
  
  MyPlot <- 
    autoplot(map.bng) +
    geom_scatterpie(data=nogeom, 
                    aes(x=X,y=Y, r = PeopleTravellingToWork/5), ## had to be a bit experimental of the r
                    cols = c("CarVan", "Passenger", "Motorbike", "Taxi", "Train", "Tube.LightRail", "Bus", "Bicycle", "OnFoot", "OtherMTW"))+
                    # coord_fixed() #+ alpha = 0.5 
    geom_scatterpie_legend((nogeom$PeopleTravellingToWork/5),n=2, x=430000, y= 96400) +
    scale_fill_manual(
      breaks = c("CarVan","Passenger", "Motorbike", "Taxi", "Train", "Tube.LightRail",  "Bus",  "Bicycle", "OnFoot", "OtherMTW"),
      labels = c("Car/Van","Passenger", "Motorbike", "Taxi", "Train", "Tube Light Rail", "Bus", "Bicycle", "On Foot", "Other MTW"),
      values = c("CarVan" = "red",
                 "Passenger" = "pink",
                 "Motorbike" = "orange",
                 "Taxi" = "yellow",
                 "Train" = "darkgreen",
                 "Tube.LightRail" = "mediumaquamarine",
                 "Bus"= "royalblue1",
                 "Bicycle" = "plum",
                 "OnFoot" = "mediumorchid4",
                 "OtherMTW" = "black")
  
  ## key step find an R color chart ## http://research.stowers.org/mcm/efg/R/Color/Chart/
  
    ) +
  
    labs(title = "Population Travelling to Work from Isle of Wight",
          subtitle = "Pie Charts are located at MSOA Population Weighted Centroids and proportioned by Population Travelling to Work",
          caption = "Source: OSM 2018, ONS Census 2011, Crown Copyright 2011",
          fill = NULL) +
    coord_fixed() +
    theme_bw() +
    theme( ##legend.position = c(0.96, 0.02),
       legend.justification = c(1, 0),
       panel.grid = element_blank(),
       panel.border = element_blank(),
       axis.title = element_blank(),
       axis.text = element_blank(),
       axis.ticks = element_blank())

  
  MyPlot
  
  ##difficulty to place the legend near the rest of the categories or in white to show up
  ## was able to add a legend for the proportions as well as the categories, not as tidy as GUI
  
  