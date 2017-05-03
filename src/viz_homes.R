library(tigris)
library(acs)
library(dplyr)
library(leaflet)
library(htmlwidgets)


df <- read.csv('~/Dropbox/Harvard/Senior Spring/CS227/Final Project/homes.csv', header=TRUE)
# work <- read.csv('~/Dropbox/Harvard/Senior Spring/CS227/Final Project/work.csv', header=TRUE)

popup <- paste0("GEOID: ", df$id, "<br>", "Num. Homes: ", df$count)
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = df$count
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df, 
              fillColor = ~pal(count), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df$count, 
            position = "bottomright", 
            title = "Num. Homes",
            labFormat = labelFormat(suffix = "%")) 

map3