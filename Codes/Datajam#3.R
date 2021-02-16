
### Datajam night 2.15.2021 
#Task 3

# Zoning datafile from Durham city website 
# Merge the map to ACS tract data/block groups data
# To get a better understanding of the zoning data
#   1.Only show Zone_code starts with R
#   1.Different abbreviates stand for? Maybe make them into 3-4 categories for better analysis?
#   2.Other variables on the file
#   3.Nicer map
#   3.Crosstab with city tier

#1. Zoning Data
#https://live-durhamnc.opendata.arcgis.com/datasets/zoning
base_url1= "https://opendata.arcgis.com/datasets/3dbb7dea6cc14544ad302061809df597_12.geojson"
zoning<-st_read("https://opendata.arcgis.com/datasets/3dbb7dea6cc14544ad302061809df597_12.geojson")
plot(zoning["ZONE_CODE"])
# note: zoning starts with R

# Get tract map
tract<-tracts(state = 'NC', county = c('Durham'))
##Initial map
ggplot() + 
  geom_sf(data = tract) + 
  geom_sf(data=zoning, aes(fill=ZONE_CODE),alpha=I(0.5))




##For reference
#2. TIER
#https://live-durhamnc.opendata.arcgis.com/datasets/development-tiers?geometry=-79.731%2C35.857%2C-77.997%2C36.245&selectedAttribute=TYPE
base_url2 = "https://opendata.arcgis.com/datasets/02e611b671f64310b7b2a420e67238c3_5.geojson"
dev_tiers <- st_read("https://opendata.arcgis.com/datasets/02e611b671f64310b7b2a420e67238c3_5.geojson")
plot(dev_tiers["TYPE"])
ggplot() + 
  geom_sf(data=dev_tiers, aes(fill=TYPE),alpha=I(0.5))+
  geom_sf(data = tract, fill = NA, color = "red")


