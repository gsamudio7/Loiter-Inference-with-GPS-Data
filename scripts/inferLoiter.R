
# Set up ####
library(data.table)
library(bigmemory)
library(DT)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(geosphere)
library(geohashTools)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(leafem)
library(comprehenr)
library(lubridate)
library(plotly)
library(ggplot2)
library(htmlwidgets)
library(formattable)
library(utils)
library(reticulate)
use_python("/Users/developer/opt/anaconda3/bin/python", required=TRUE)


source_python("scripts/pythonSupport.py")





# Data ####
data <- fread("data/Boutin Alberta Grey Wolf.csv",
              select=c("study-local-timestamp",
                       "tag-local-identifier",
                       "location-long",
                       "location-lat"),

# Make sure data.table does not automatically generate factor columns
              stringsAsFactors = FALSE) %>% 
  
  # Omit NAs in the data. Familiarization with how the data was collected is 
  # necessary to consider retaining these values and making them useful
  na.omit()

# Set the column names for convenience
setnames(data, 
         
         # Vector of old names that we want to change
         old=c("study-local-timestamp",
               "tag-local-identifier",
               "location-long",
               "location-lat"),
         
         # Vector of new more convenient names
         new=c("dtg", # date time group
               "cid", # component ID
               "lon", # longitude
               "lat")) # latitude



# Use data.table indexing to determine to wolf with the most data
# wolf.of.Interest <- data[,.(count=.N),by="cid"][count==max(count)]$cid # The additional [] gives us which cid has the maximum count


dt <- data
(m <- nrow(dt))

# Create datetime objects from dtg character strings
dt[,"dtg" := dtg %>% as.POSIXct(format="%Y-%m-%d %H:%M:%S")]

# Order the data sequentially by date time group
dt = dt[with(dt,order(cid,dtg))] #setorder(dt,dtg)

# Set inter-obs time interval column
dt[,"timeInt" := dtg %>% difftime(shift(dtg),unit="secs")]
dt[,"timeInt" := ifelse(timeInt <= 24*3600,timeInt,NA)]

# Use lubridate package to get the weekday from the date objects
dt[,"Weekday" := dtg %>% lubridate::wday(label=TRUE)]

# Get the hour from the date objects
dt[,"Hour" := lubridate::hour(dtg)]

# Get the time of day for each dtg
dt[,"time" := as.ITime(dtg)]


# Epsilon Spatial Distance ####
getSpatNN = function(points) {
  distances = geodist(points,points,measure="haversine") 
  diag(distances) = NA
  return(apply(distances,2,min,na.rm=TRUE))}
distVec = c()
for (u in unique(dt$cid)) {
  distances = getSpatNN(dt[cid==u,c("lon","lat")])
  distVec = c(distVec,distances)
}
distances <- geodist::geodist(dt[,c("lon","lat")],measure="haversine") 

# Get the distances for the nearest neighbors and sort
distances[distances==0] <- Inf
distVec <- apply(distances,2,min) %>% sort()
