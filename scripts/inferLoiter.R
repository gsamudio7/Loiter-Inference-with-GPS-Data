
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
use_python("/Users/developer/opt/anaconda3/bin/python", required=TRUE)


source_python("scripts/supportFunctions.py")

# First chunk testing ####
baseLeaf <- function(
  centerLat=55,
  centerLon=-105,
  initZoom=4,
  logoLocation="https://github.com/gsamudio7/Loiter-Inference-with-GPS-Data/blob/main/assets/images/NGATitle.png?raw=true",
  logoWidth=350,logoHeight=60,
  gitRepo="https://github.com/gsamudio7/Loiter-Inference-with-GPS-Data",
  gitRepoTitle="Loiter Inference with GPS Data",
  groups2hide=NULL,
  overGroups=NULL) {
      return(
        leaflet() %>% 
          addProviderTiles(
            group="NGA Imagery",
            provider=providers$Esri.WorldImagery,
            options=tileOptions(
              attribution=toString(tags$a(href = paste0(gitRepo),
                                          gitRepoTitle)))) %>%
          addProviderTiles(
            provider=providers$CartoDB.DarkMatter,
            group="NGA Slate",
            options=tileOptions(
              attribution=toString(tags$a(href = paste0(gitRepo),
                                          gitRepoTitle)))) %>%
          addLogo(img=logoLocation,
                  width=logoWidth,height=logoHeight,url=gitRepo) %>%
          addEasyButton(
            easyButton(
              icon = "ion-arrow-shrink", 
              title = "Reset View", 
              onClick = JS(
                "function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"
              )
            )
          ) %>% 
          htmlwidgets::onRender(
            JS(
              "
          function(el, x){ 
            var map = this; 
            map.whenReady(function(){
              map._initialCenter = map.getCenter(); 
              map._initialZoom = map.getZoom();
            });
          }"
            )
          ) %>%
          addMouseCoordinates() %>%
          setView(lng=centerLon,lat=centerLat,zoom=initZoom) %>%
          addMiniMap(
            tiles='Esri.WorldImagery',
            zoomLevelOffset = -10,
            toggleDisplay = TRUE, 
            position="bottomleft") %>%
          addLayersControl(
            baseGroups = c("NGA Slate","NGA Imagery"),
            overlayGroups = overGroups,
            options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c("NGA Imagery",groups2hide)) %>%
          
          addControl(
            className=NULL,
            html=
              "<style>
              .leaflet-control-layers-expanded {
                  padding: 6px 10px 6px 6px;
                  color: #fff;
                  background: #333;
              }
              
              .info {
                padding: 6px 8px;
                font: 14px/16px Arial, Helvetica, sans-serif;
                background: #333;
              }
              
              .legend svg text {
                fill: #fff;
              }
              
              .legend {
                color: #fff;
              }
              
              .legend svg line {
                stroke: #fff;
              }
              
              .leaflet-bar a, .leaflet-bar a:hover {
                background-color: #333;
                border-bottom: 1px solid #ccc;
                text-align: center;
                text-decoration: none;
                color: #fff;
              }
              
              .leaflet-bar button, .leaflet-bar button:hover {
                background-color: #333;
                text-align: center;
                text-decoration: none;
                color: #fff;
              }
              
              </style>")
      )}




# Screw geohash --> Use hexbins!
hexFrame_2_sf = function(hexFrame) {
  bdry = h3_to_geo_boundary_sf(hexFrame$H3)
  return(st_as_sf(hexFrame, bdry$geometry))
}

plotH3 <- function(map,pts,h3Resolution,groupName,colPal,h3_labels=NULL) {
  
  # Set H3 column
  pts[,"H3" := geo_to_h3(pts[,c("lat","lon")],res=h3Resolution)]
  
  # Set count data.table and make sf object
  hexFrame <- pts[,.(count=.N),by=H3] %>% hexFrame_2_sf()
  m <- sum(hexFrame$count)
  
  # Set color palette
  pal <- colorNumeric(palette=colPal,reverse=TRUE,
                      domain=log10(hexFrame$count))
  
  # Map it
  return(map %>% 
           addPolygons(
              data=hexFrame,
              color=~count %>% log10() %>% pal(),
              weight=0.5,opacity=1, fillOpacity=0.35,
              highlight = highlightOptions(
                weight = 1,
                color="white",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label=h3_labels,
              group = groupName
          ) %>% 
           addLegend(position="bottomright",
                     group=groupName,
                     pal=pal,opacity=8,
                     values=log10(hexFrame$count),
                     labFormat=labelFormat(transform = function(x) {10^x %>% round()}),
                     title="<b>Obs Count</b>") %>%
           
           clearBounds()
  )
  
}

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
baseLeaf(overGroups=c("Wolf Population","Wolf of Interest")) %>% 
  
  plotH3(pts=data[,c("lon","lat")],
         h3Resolution=8,
         groupName="Wolf Population",
         colPal="Reds") %>%
  
  plotH3(pts=dt[,c("lon","lat")],
         h3Resolution=8,
         groupName="Wolf of Interest",
         colPal="Blues")
  









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
