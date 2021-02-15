
###
library(tidycensus)
library(tidyverse)
library(httr)
library(jsonlite)
library(mapview)
library(ggplot2)
library(tigris)

###First Get Durham City Boundary
#load library
library(sf)
#load geojson from url
res = st_read("https://opendata.arcgis.com/datasets/07a70e16e76c4bc987f47fdb762367f9_1.geojson")
#quick view to see what we're dealing with
mapview::mapview(res)

##Get Census Shape File
dfw <- blocks(state = 'NC', county = c('Durham'))
mapview::mapview(dfw)
tract<-tracts(state = 'NC', county = c('Durham'))
ggplot(tract) + geom_sf()

ggplot() + 
  geom_sf(data = res) + 
  geom_sf(data = tract, fill = NA, color = "red")

####Find other statistical areas
cb <- core_based_statistical_areas(cb = TRUE)
pdx <- filter(cb, grepl("Durham-Chapel Hill", NAME))
ggplot(pdx) + geom_sf()
p1 <- tract[pdx,]
ggplot() + 
  geom_sf(data = p1) + 
  geom_sf(data = pdx, fill = NA, color = "red")
# Core base statistical area is too big,even though Durham city bourndary doesn't correspond perfectly to the census tract. I have to go with it. 




