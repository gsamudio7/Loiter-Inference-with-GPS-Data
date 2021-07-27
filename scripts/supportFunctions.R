


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