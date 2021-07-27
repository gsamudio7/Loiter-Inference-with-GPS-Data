
# Imports ####
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
library(geodist)
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
library(base64enc)
library(h3)
library(mgrs)

use_python("/Users/developer/opt/anaconda3/bin/python", required=TRUE)
source_python("scripts/supportFunctions.py")

# Read in defined support functions
source("scripts/supportFunctions.R")

# Initial Map ####

# Get the data
data <- fread("data/ABoVE_ Boutin Alberta Grey Wolf.csv",
              select=c("study-local-timestamp",
                       "tag-local-identifier",
                       "location-long",
                       "location-lat"),
              stringsAsFactors = FALSE) %>% 
  na.omit()


setnames(data, 
         old=c("study-local-timestamp",
               "tag-local-identifier",
               "location-long",
               "location-lat"),
         new=c("dtg", 
               "cid", 
               "lon", 
               "lat")) 

wolf.of.Interest <- data[,.(count=.N),by="cid"][count==max(count)]$cid  
dt <- data[cid==wolf.of.Interest]

# Plot H3 density map of wolf population and wolf of interest
initMap <- baseLeaf(overGroups=c("Wolf Population","Wolf of Interest")) %>%
  
  plotH3(pts=data[,c("lon","lat")],
         h3Resolution=8,
         groupName="Wolf Population",
         colPal="Reds",
         H3_labels=FALSE) %>%
  
  plotH3(pts=dt[,c("lon","lat")],
         h3Resolution=8,
         groupName="Wolf of Interest",
         colPal="Blues")

# Save output for faster rendering
save(initMap, file="products/initMap.RData")


# Results map ####

# Read in processed data
load("products/loiterData.RData")

plotHDB <- function(map,frame,loiterData,colPal) {
  
  # Organize parameters
  m <- dim(frame)[1]
  frame$clusVec <- frame[[3]]
  pal <- colorNumeric(palette=colPal,reverse=TRUE,
                      domain=loiterData[,avgLoiterTime])
  
  # Plot every cluster
  for (k in frame[,unique(clusVec)]) {
    
    # Generate spatial polygons for each cluster
    toPlot <- suppressWarnings(
      frame[clusVec==k,c(1,2)] %>% 
        SpatialPoints() %>%
        gConvexHull()
    )
    if (class(toPlot) != "SpatialPoints" & class(toPlot) != "SpatialLines") {
      
      # Plot polygon on map
      for (g in unique(frame[clusVec==k,group])) {
        map <- map %>%
          addPolygons(data = toPlot,weight=2,opacity=1, fillOpacity=0.6,
                      color = loiterData[clusVec==k,avgLoiterTime] %>% pal(),
                      group = g,
                      highlightOptions = highlightOptions(color="white", 
                                                          weight=2.5,
                                                          bringToFront=TRUE),
                      popup = leafpop::popupGraph(radHeat(frame[clusVec==k,c("Hour","Weekday")])),
                      label = HTML(paste0("<b>Avg Loiter Time: </b>",
                                          round(loiterData[clusVec==k,avgLoiterTime]) %>%
                                            seconds_to_period(),"<br>",
                                          "<b>Avg Activity per Visit: </b>",
                                          loiterData[clusVec==k,`Avg Activity per Visit`],"<br>",
                                          "<b>Visit Count: </b>",
                                          loiterData[clusVec==k,`Visit Count`],"<br>",
                                          "<b>Obs Count: </b>",
                                          loiterData[clusVec==k,count],"<br>",
                                          "<b>Data Proportion: </b>",
                                          round(loiterData[clusVec==k,count]/m,3))),
                      labelOptions = labelOptions(opacity=0.85,
                                                  style=list("background-color"="#333",
                                                             "color"="#FFF"))
          )
      }
    } 
  }
  return(map %>% clearBounds())
}


numGroups <- processed_dt[,uniqueN(group)]
resultMap <- baseLeaf(groups2hide=c("Observations",processed_dt[,unique(group)] %>% 
                                      sort() %>% 
                                      tail(numGroups - 1) %>% as.character()),
                      overGroups=c("Observations",processed_dt[,unique(group)] %>% sort() %>% as.character())) %>%
  
  plotHDB(frame=processed_dt[clus!="0",c("lon","lat","clus","group","Hour","Weekday")],
          loiterData=loiterData,
          colPal="Blues") %>%
  
  plotH3(pts=processed_dt[,c("lon","lat")],
         h3Resolution=12,
         groupName="Observations",
         colPal="inferno",
         reverseColorPalette=FALSE,
         H3_labels=FALSE)

save(resultMap, file="products/resultMap.RData")






# Data ####
data <- fread("data/ABoVE_ Boutin Alberta Grey Wolf.csv",
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
