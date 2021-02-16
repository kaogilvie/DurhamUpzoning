#Datajam 2.16.2021
#Task2
#Analysis with ACS Data
# get ACS 5-year data (2015-2019) and produce Durham county estimates by block groups
# Suggest variables: Median house prices and rents, race/ethnicity, economic and demographic characterstics)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(viridis)
library(scales)
library(knitr)
library(getCensusApi)

#https://api.census.gov/data/key_signup.html
## Not run:
census_api_key("8d719353bc2b141c03e4b35c9453b14636463bce", install = TRUE)
# First time, reload your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")

# ACS website
#https://www.census.gov/data/developers/data-sets/acs-5year.html

#To find geographic level
# https://api.census.gov/data/2019/acs/acs5/geography.html
#To find table
#https://api.census.gov/data/2019/acs/acs5/variables.html

##Get all census block groups for one variable
temp.df <- get_acs(geography = "block group", variables = vars,state = "NC", county = 063, geometry = FALSE)
head(temp.df)