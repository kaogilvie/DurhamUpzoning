# DurhamUpzoning
### Overview
This study will examine zoning reforms implemented on October 1, 2019 in Durham, North Carolina, a city with a population of roughly 280,000. The zoning changes were part of the Expanded Housing Choices Initiative (EHC), which amended the city’s Unified Development Ordinance (UDO) to upzone neighborhoods located in the urban tier to promote increased densities, construction of accessory dwelling units, and housing development on smaller lots. The primary goals of the revised UDO are to reduce pressure on low-income neighborhoods of color in Durham that have become hot spots for development due to the city’s economic growth and to increase housing affordability, density, and racial and economic diversity in wealthier, whiter neighborhoods. This study proposes to examine the effects of upzoning in selected Durham neighborhoods. Three primary research questions will be addressed in this study. Below we discuss the research questions, hypotheses, and the gaps in knowledge they fill.

Large-scale upzoning in cities is rare. According to Gabbe (2018) when New York City experienced a period of dramatic rezoning between 2003-2009, only 5% of city lots were upzoned. Similarly, in Los Angeles between 2002-2014, only 1.1% of the land area was upzoned. Only a few studies have evaluated the effects of zoning changes on property markets (Thorson, 1997; Atkinson-Palombo, 2010; Freemark, 2020.) Most studies that explore the effects of zoning compare the restrictiveness of zoning between regions and the effects on housing markets. These studies are cross-sectional and have severe limitations due to possible endogeneity – does zoning affect housing market outcomes or vice versa? While an increasing number of places – the city of Minneapolis and all cities in the state of Oregon – are adopting expansive zoning reforms by eliminating single-family zoning, evaluations of the impacts of these reforms have not been conducted. Answering Research Question 1, below, will advance our understanding of the housing and neighborhood effects resulting from large-scale upzoning.
  
  ----
  ### Problem Statement
  I want to use Durham property and open source data to determine the effects of a zoning changes so that Durham’s planning department, UNC, and the communities in Durham can understand whether large-scale upzoning is effective in relieving gentrification pressure.
  
  ---
  ### Research Questions
  This research study will explore three main questions as described below:
  - **Research Question 1**: What are the effects of upzoning in selected neighborhoods on the following: housing unit affordability (house prices and rents), housing unit turnover (resale rates, demolition/reconstruction rates), economic and demographic composition (household income and race), and structural composition (type, size, and number of housing units)?
  - **Research Question 2**: How does the physical and social character of upzoned neighborhoods change in the short-term?
  - **Research Question 3**: Are developers more motivated to build or renovate homes in the upzoned neighborhoods due to the relaxing of zoning regulations
  
  ---
  ### Data
  The main sources are:
- Parcel level data (tax assessment?)
    - Lot size, features (bed/bath), size, sales price
- GIS layers of areas that have been upzoned
- Demographics data from ACS

Other sources:
AirBnB, craigslist rental data?

Main need: it would be really great if we had access to all home sale and rental data, up to the present. This should all be public (?), but what is/are the best sources to get this in bulk?

- Data Dictionary

  
  --- 
  ### Team
Project Partner Leads: Mai (UNC), Scott (Durham Planning Dept), Michael (Durham Planning Dept)

DKDC Chapter Leader: William and Rich

Data Ambassador(s): Clare, Armel

Volunteer Team Members:

---
### Project Materials Locations

Project Summary and background reading - Google Drive: https://drive.google.com/drive/folders/1dTV7840Mdw_kOgU99-wcNTWkng3FWun1

Data on Google Drive Data Folder: https://github.com/DataKind-DC/DurhamUpzoning

Code on https://github.com/DataKind-DC/DurhamUpzoning

Project Communication - #durham channel on DKDC Slack: http://dkdc.herokuapp.com/

---
### Volunteers Involvement
  There are four sub-tasks. Based on your skills/experience, you can decide to help in one or more of the components:
  - Task 1: initially merged the city tier to census tract level and found that boundaries don't overlap perfectly
       Maybe we can try blockgroups level data?
  - Task 2: Analysis with ACS Data.
  - Get ACS 5-year data (2015-2019) and produce Durham county estimates by block groups
  - Task 3: Zoning datafile from Durham city website
  Merge the map to ACS tract data/block groups data
 To get a better understanding of the zoning data
    Only show Zone_code starts with R
       1.Different abbreviates stand for? Maybe make them into 3-4 categories for better analysis?
       2.Other variables on the file
       3.Nicer map
      4.Crosstab with city tier

- Task 4: Descriptive statistics of the variables
    can we join these two datasets?
    Building Permits
https://durham.municipal.codes/UDO/4.1.1
https://live-durhamnc.opendata.arcgis.com/datasets/all-building-permits-1/data?geometry=-79.725%2C35.858%2C-77.991%2C36.246

  
  ---
  ### Deliverables
Current Next Steps
1. Durham/UNC sends data and background reading. DKDC CLs and DAs review.

Suggested Other Next Steps (running list)

Background Reading and References

Other possibilities/ideas

---
### References
Project Summary and background reading on Google Drive at https://drive.google.com/drive/folders/1dTV7840Mdw_kOgU99-wcNTWkng3FWun1

Data on the Google Drive Data Folder at https://github.com/DataKind-DC/DurhamUpzoning

Code on https://github.com/DataKind-DC/DurhamUpzoning

City of Durham's Open Data site: https://live-durhamnc.opendata.arcgis.com/

Site for parcels dataset can be found at https://live-durhamnc.opendata.arcgis.com/datasets/parcels

Site for building dataset can be found at https://live-durhamnc.opendata.arcgis.com/datasets/all-building-permits-1?geometry=-79.725%2C35.858%2C-77.991%2C36.246
