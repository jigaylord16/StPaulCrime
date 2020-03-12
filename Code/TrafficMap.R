setwd("~/Desktop/DataVisualization/FinalProject")
library(flexdashboard)
library(shiny)
library(tidyverse)
library(leaflet)
library(rgdal)
library(htmltools)
library(lubridate)
library(ggplot2)
library(ggmap)

traffic_data <- read.csv("Traffic_Stop_Dataset.csv")
crash_data <- read.csv("Pedestrian_And_Bike_Crash_Data_-_Dataset.csv")

police_grids <- readOGR("st_paul_police_grids.shp")
police_grids <- spTransform(police_grids,
                            CRS("+proj=longlat +datum=WGS84 +no_defs"))

police_grids@data$gridnum <- as.numeric(as.character(police_grids@data$gridnum))

traffic_stops <- traffic_data %>%
  filter(YEAR.OF.STOP == 2018) %>%
  group_by(gridnum = POLICE.GRID.NUMBER) %>%
  summarise(stops = n())

police_grids@data$gridnum <- as.numeric(as.character(police_grids@data$gridnum))

police_grids@data <- police_grids@data %>%
  left_join(traffic_stops)

bins <- c(1, 25, 50, 75, 125, 175, 250, 325, 550, 750, 1295)
pal <- colorBin("YlOrRd", domain = police_grids@data$stops, bins = bins)

labels <- sprintf("<strong>%s</strong><br/>%g Stops", 
                  police_grids@data$gridnum, police_grids@data$stops) %>% 
  lapply(htmltools::HTML)

leaflet(police_grids) %>%
  setView(-93.11, 44.94, zoom = 12) %>%
  addProviderTiles(providers$CartoDB.DarkMatter, 
                   options = providerTileOptions(minZoom = 12)) %>%
  addPolygons(fillColor = ~pal(stops),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.9,
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>%
  addLegend(pal = pal, values = ~count, opacity = 0.7, title = "Traffic Stops in 2018",
            position = "bottomright")