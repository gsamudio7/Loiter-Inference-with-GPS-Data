
# Imports ####
rm(list=ls())
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

# On laptop:
Sys.setenv(RETICULATE_PYTHON = "/Users/developer/opt/anaconda3/bin/python")
use_python("/Users/developer/opt/anaconda3/bin/python", required=TRUE)

# On desktop:
# Sys.setenv(RETICULATE_PYTHON = "/Users/gms/opt/anaconda3/bin/python")
# use_python("/Users/gms/opt/anaconda3/bin/python", required=TRUE)

# Source python functions
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
initMap <- baseLeaf(pts=data[,c("lon","lat")],
                    initZoom=4,
                    overGroups=c("Wolf Population","Wolf of Interest")) %>%
  
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

load("products/initMap.RData")
initMap


# Results map ####

# Read in processed data
load("products/loiterData.RData")
processed_dt$Hour <- processed_dt$Hour %>% Vectorize(military_time)() %>% factor(levels=Vectorize(military_time)(0:23))
resultMap <- baseLeaf(pts=processed_dt[,c("lon","lat")],
                      initZoom = 9,
                      miniMapOffset=-8,
                      groups2hide=setdiff(
                        processed_dt[,unique(group)] %>% as.character(),
                        "1200 - 1500"),
                      overGroups=c("Observations",processed_dt[,unique(group)] %>% 
                                   sort() %>% as.character())) %>%

  plotH3(pts=processed_dt[,c("lon","lat")],
         h3Resolution=8,
         groupName="Observations",
         colPal="Blues",
         reverseColorPalette=TRUE,
         H3_labels=FALSE) %>%
  
  plotHDBSCAN(frame=processed_dt[clus!="0",c("lon","lat","clus","group","Hour","Weekday")],
              loiterData=loiterData,
              colPal="Oranges") 
  
  

save(resultMap, file="products/resultMap.RData")

load("products/resultMap.RData")

# Save as html
htmlwidgets::saveWidget(resultMap,file="products/resultMap.html")

# Plots ####
# # Generate a column of date objects 
dt_init[,"date" := lubridate::date(dtg)]
# 
# # Accumulate the observation counts for each day
dayCount <- dt_init[,.(count=.N),by=date]
# 
# # Plot
daily_volume <- plot_ly(x=dayCount[,date],
        y=dayCount[,count],
        type="scatter",mode="markers",
        marker=list(color="#2359c4",size=5,opacity=0.65)) %>%
  layout(title="<b>Daily Activity from\n18MAR12 to 18MAY14\n",
         xaxis = list(title="Month"),
         yaxis = list(title="Average Activity")) %>% plotly_build()
save(daily_volume,file="products/daily_volume.RData")

# dt_init$Hour <- dt_init$Hour %>% Vectorize(military_time)() %>% factor(levels=Vectorize(military_time)(1:24))
# 
# # Plot
# weekday_and_hour_volume <- plot_ly(dt_init, x = ~Hour, y = ~Weekday,
#         colors=c("gray15","#990000"),
#         name=" ",
#         hovertemplate = paste('<b>Weekday: </b>%{y}',
#                               '<br><b>Hour: </b>%{x}',
#                               '<br><b>Count:</b> %{z}')) %>%
#   layout(title = "<b>Activity Count by Hour and Weekday</b>") %>% add_histogram2d() %>% plotly_build()
# save(weekday_and_hour_volume, file="products/weekday_and_hour_volume.RData")

weekday_and_hour_volume <- activity_heat(dt_init,dark=FALSE)
save(weekday_and_hour_volume,file="products/weekday_and_hour_volume.RData")

# # Create a vector of densities
qt.95 <- dt_init[,timeInt/60] %>% na.omit() %>% quantile(.95) %>% round()
dens <- density(dt_init[,timeInt/60] %>% na.omit() %>% as.double() %>% log())

# Plot
inter_obs_time <- plot_ly(x=~dens$x,
                          y=~dens$y,
                          type = "scatter",
                          mode="lines",
                          color=I("#2359c4")) %>%
  layout(
    yaxis = list(title="Density"),
    xaxis = list(title="Inter-observation Time (minutes)",
                 tickvals = c(2.335666,2.707814,5.2,seq(0,7,2)),
                 ticktext = c(2.335666,2.707814,5.2,seq(0,7,2)) %>% exp() %>% round()),
    title = "<b>Inter-Observation Time</b><br>",
    shapes = list(type ="line",
                  line = list(color="black"),
                  x0 = log(qt.95), x1 = log(qt.95),
                  y0 = 0, y1 = 70),
    annotations = list(text = paste("<b>.95 Quantile: </b>",round(qt.95,2),"minutes"),
                       x = log(qt.95) + 1, y = 72, showarrow=FALSE)) %>% plotly_build()

save(inter_obs_time,file="products/inter_obs_time.RData")





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
