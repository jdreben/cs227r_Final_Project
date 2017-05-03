library(tigris)
library(acs)
library(dplyr)
library(leaflet)
library(htmlwidgets)

counties <- c(5, 47, 61, 81, 85)
tracts <- tracts(state = 'NY', county = c(5, 47, 61, 81, 85), cb=TRUE)
api.key.install(key="d4342ba4887a54a48cd099555fb7a4df0764c62e")
geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81, 85), tract="*")

# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

income<-acs.fetch(endyear = 2012, span = 5, geography = geo,
                table.number = "B19001", col.names = "pretty")

# Shape income_df with home. and work columns

# convert to a data.frame for merging
income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                             str_pad(income@geography$county, 3, "left", pad="0"), 
                             str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("Household Income: Total:",
"Household Income: $200,000 or more")], 
                        stringsAsFactors = FALSE)

income_df <- select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)

income_merged<- geo_join(tracts, income_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
income_merged <- income_merged[income_merged$ALAND>0,]

popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = income_merged$percent
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = income_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = income_merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 


# saveWidget(map1, file="map1.html", selfcontained=FALSE)
# saveWidget(map2, file="map2.html", selfcontained=FALSE)
# saveWidget(map3, file="map3.html", selfcontained=FALSE)