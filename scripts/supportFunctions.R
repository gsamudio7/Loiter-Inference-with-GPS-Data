
baseLeaf <- function(
  pts,
  initZoom,
  miniMapOffset=-5,
  logoLocation="https://github.com/gsamudio7/Loiter-Inference-with-GPS-Data/blob/main/assets/images/NGATitle.png?raw=true",
  logoSource="remote",
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
      addLogo(img=logoLocation,src=logoSource,
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
      setView(lng=mean(pts$lon),lat=mean(pts$lat),zoom=initZoom) %>%
      addMiniMap(
        tiles='Esri.WorldImagery',
        zoomLevelOffset = miniMapOffset,
        toggleDisplay = TRUE,
        position="bottomleft") %>%
      addLayersControl(
        baseGroups = c("NGA Slate","NGA Imagery"),
        overlayGroups = overGroups,
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(groups2hide) %>%
      
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



hexFrame_2_sf = function(hexFrame) {
  bdry = h3_to_geo_boundary_sf(hexFrame$H3)
  return(st_as_sf(hexFrame, bdry$geometry))
}

h3_to_mgrs <- function(H3_vector) {
  latlng_matrix <- H3_vector %>% h3::h3_to_geo() 
  return(Vectorize(mgrs::latlng_to_mgrs)(latlng_matrix[,1],latlng_matrix[,2]))
}


plotH3 <- function(map,pts,h3Resolution,groupName,colPal,reverseColorPalette=TRUE,H3_labels=TRUE) {
  
  # Set H3 column
  pts[,"H3" := geo_to_h3(pts[,c("lat","lon")],res=h3Resolution)]
  
  # Set count data.table and make sf object
  hexFrame <- pts[,.(count=.N),by=H3] %>% hexFrame_2_sf()
  m <- sum(hexFrame$count)
  
  # Set labels
  if (H3_labels==TRUE) {
    labels = sprintf(
      "<b>MGRS:</b> %s<br>
       <b>Obs Count:</b> %s<br>
       <b>Data Proportion:</b> %s",
      hexFrame$H3 %>% h3_to_mgrs, hexFrame$count, round(hexFrame$count/m,3)) %>% lapply(htmltools::HTML)
  } else {
    labels=NULL
  }
  
  # Set color palette
  pal <- colorNumeric(palette=colPal,reverse=reverseColorPalette,
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
             label=labels,
             labelOptions = labelOptions(opacity=0.85,
                                         style=list("background-color"="#333",
                                                    "color"="#FFF")),
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

# Function to retrieve loiter time information
getTimeLoiter <- function(k,frame) {
  clusList <- seqle(which(frame$clus==k))
  if (clusList$lengths %>% length() > 1) {
    idx <- to_vec(for (i in 1:length(clusList$lengths)) {
      seq(from=clusList$values[i], 
          to=clusList$values[i] + clusList$lengths[i])
    })
    clusDT <- data.table(
      # Time interval column where the traveler is in cluster k
      time=frame[idx,timeInt],
      
      # Artificial label each instance in cluster k
      instance=rep(1:length(clusList$values),times=clusList$lengths + 1))
    
    timeLoiterDT <- clusDT[time <= 15*60,.(`Time Loitering`=sum(time,na.rm=TRUE)),by=instance] 
    times <- timeLoiterDT[,`Time Loitering`]
    return(c(times %>% mean(),
             length(clusList$lengths),
             round(mean(clusList$lengths))))
  } else {return(c(NA,NA,NA))}
}


processResults <- function(frame,resVector,minLoiterTime,minVisCount,clusterColumnName) {
  
  # Set the cluster labels
  frame[,"clusVec" := resVector]
  
  # Initialize data.table to store loiter info
  loiterDT <- data.table(clusVec=unique(frame[clusVec!=0,clusVec])) %>% 
    cbind(t(sapply(unique(frame[clusVec!=0,clusVec]),getTimeLoiter,frame))) %>% na.omit()
  setnames(loiterDT,c("V1","V2","V3"),c("avgLoiterTime","Visit Count","Avg Activity per Visit"))
  
  # Remove clusters with only one visit
  frame[,"clusVec" := ifelse(clusVec %in% loiterDT$clusVec,clusVec,0)]
  
  # Check number of visits
  toRemove <- loiterDT[`Visit Count` < minVisCount,clusVec]
  frame[,"clusVec" := ifelse(clusVec %in% toRemove,0,clusVec)]
  loiterDT <- loiterDT[`Visit Count` >= minVisCount]
  
  # Check loiter times
  toRemove <- loiterDT[avgLoiterTime < minLoiterTime,clusVec]
  frame[,"clusVec" := ifelse(clusVec %in% toRemove,0,clusVec)]
  loiterDT <- loiterDT[avgLoiterTime >= minLoiterTime]
  
  # Relabel column explicitly (for flexibility in case frame has more than one column of cluster labels)
  frame[[clusterColumnName]] <- frame[,clusVec]
  
  return(list("frame"=frame[,-"clusVec"],
              "loiterDT"=frame[clusVec!=0,.(count=.N),by=clusVec] %>% 
                merge(loiterDT,by="clusVec")))
}


seqle <- function(x,incr=1) { 
  if(!is.numeric(x)) x <- as.numeric(x) 
  n <- length(x)  
  y <- x[-1L] != x[-n] + incr 
  i <- c(which(y|is.na(y)),n) 
  temp <- list(lengths = diff(c(0L,i)),
               values = x[head(c(0L,i)+1L,-1L)]) 
  return(list(lengths=temp$lengths[temp$lengths > 1] - 1,
              values=temp$values[temp$lengths > 1]))
} 


military_time <- function(hour_integer) {
  if (nchar(hour_integer)==1) {
    return(paste0("0",hour_integer,"00"))
  } else {return(paste0(hour_integer,"00"))}
}


activity_heat <- function(fr,dark=TRUE) {
  frame <- rbindlist(list(fr,
                     data.table("Hour"=rep(levels(fr$Hour),times=length(levels(fr$Weekday))),
                            "Weekday"=rep(levels(fr$Weekday),each=length(levels(fr$Hour))))),
                     fill=TRUE)
  countDT <- frame[,.(Activity=.N),by=c("Hour","Weekday")] %>% na.omit()
  chart <- ggplot(countDT,aes(x=Hour,y=Weekday)) + 
    geom_tile(data=countDT,aes(fill=Activity)) +
    coord_polar(theta = "x",clip="off",start=-pi/24,direction=1) 
  if (dark==TRUE) {
    return(chart +
           theme(axis.text.x = element_text(color="#FFFFFF"),
                 axis.text.y = element_text(color="#FFFFFF"),
                 axis.title = element_text(color="#FFFFFF"),
                 axis.line = element_blank(),
                 panel.grid.major = element_blank(),
                 plot.background = element_rect(fill = "#333333"),
                 panel.background = element_blank(),
                 legend.background = element_rect(fill = "#333333"),
                 legend.text = element_text(color="#FFFFFF"),
                 legend.title = element_text(color="#FFFFFF")))} else {
                   return(chart +
                          theme(axis.line = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.major = element_blank()))}
}



plotHDBSCAN <- function(map,frame,loiterData,colPal) {
  
  # Organize parameters
  m <- dim(frame)[1]
  frame$clusVec <- frame[[3]]
  pal <- colorNumeric(palette=colPal,reverse=TRUE,
                      domain=loiterData[,avgLoiterTime])
  
  # Plot every cluster
  for (k in frame[,unique(clusVec)]) {
    
    # Generate sf object for each cluster
    toPlot <- frame[clusVec==k,c("lon","lat")] %>% as.matrix() %>%
      sf::st_multipoint() %>%
      sf::st_convex_hull()
    
    centroid <- toPlot %>% sf::st_centroid() %>% sf::st_coordinates()
    centroid_mgrs <- mgrs::latlng_to_mgrs(longitude=centroid[,1],
                                          latitude=centroid[,2])
    # Plot polygon on map
    for (g in unique(frame[clusVec==k,group])) {
      map <- map %>%
        addPolygons(data = toPlot,weight=2,opacity=1, fillOpacity=0.6,
                    color = loiterData[clusVec==k,avgLoiterTime] %>% pal(),
                    group = g,
                    highlightOptions = highlightOptions(color="white", 
                                                        weight=2.5,
                                                        bringToFront=TRUE),
                    popup = leafpop::popupGraph(
                      activity_heat(fr=frame[clusVec==k,c("Hour","Weekday")])),
                    label = HTML(paste0("<b>MGRS: </b>", centroid_mgrs,
                                        "<br><b>Avg Loiter Time: </b>",
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
  return(map %>% 
           addLegend(position="bottomright",
                     pal=pal,opacity=8,
                     values=loiterData[,avgLoiterTime],
                     labFormat=labelFormat(transform = function(x) {round(x) %>%
                         seconds_to_period()}),
                     title="<b>Avg Loiter Time</b>") %>%
           clearBounds())
}