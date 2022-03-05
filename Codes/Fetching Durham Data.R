library(ggmap)
library(ggplot2)
library(rjson)
library(jsonlite)
library(sf)
library(RCurl)
library(leaflet)
library(tidyverse)

#install.packages("sf")

# Fetching All Durham Data
#1. Zoning Data
base_url1= "https://opendata.arcgis.com/datasets/3dbb7dea6cc14544ad302061809df597_12.geojson"
zoning<-st_read("https://opendata.arcgis.com/datasets/3dbb7dea6cc14544ad302061809df597_12.geojson")
plot(zoning["ZONE_CODE"])
ggplot() + 
  #geom_sf(data = res) + 
  geom_sf(data=zoning, aes(fill=ZONE_CODE),alpha=I(0.5))

#2. TIER
#https://live-durhamnc.opendata.arcgis.com/datasets/development-tiers?geometry=-79.731%2C35.857%2C-77.997%2C36.245&selectedAttribute=TYPE
base_url2 = "https://opendata.arcgis.com/datasets/02e611b671f64310b7b2a420e67238c3_5.geojson"
dev_tiers <- st_read("https://opendata.arcgis.com/datasets/02e611b671f64310b7b2a420e67238c3_5.geojson")
plot(dev_tiers["TYPE"])
ggplot() + 
  #geom_sf(data = res) + 
  geom_sf(data=dev_tiers, aes(fill=TYPE),alpha=I(0.5))#+
  #geom_sf(data = tract, fill = NA, color = "red")

#notes:cn stands for The Compact Neighborhood Tier 


#3. Building Permits
#https://durham.municipal.codes/UDO/4.1.1
#https://live-durhamnc.opendata.arcgis.com/datasets/all-building-permits-1/data?geometry=-79.725%2C35.858%2C-77.991%2C36.246

base_url3="https://opendata.arcgis.com/datasets/147b91c0ff5c4c03931e9b3580026065_12.geojson"

#4.Parcels
#https://live-durhamnc.opendata.arcgis.com/datasets/parcels
base_url4="https://opendata.arcgis.com/datasets/9cde87b4bac348faa1332997093654bb_0.geojson"
