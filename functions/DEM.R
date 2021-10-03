library(sf)
library(sp)
library(raster)
library(leaflet)

# itm: EPSG:2039 Israel 1993 / Israeli TM Grid
itm<-"+init=epsg:2039"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84"


getAGLFromDEM<-function(atlas.df=NULL,
                        demPath="data/DEM/"){
 
  if(is.null(atlas.df)){
    message("getAGLFromDEM(): atlas.df should not be null")
    return()
  }
  
  #--------------------------------------
  # convert coordinates to WGS
  #--------------------------------------
  atlas.spdf<-convertSpatial.ITM2WGS84(atlas.df,
                      xyColNames=c("X","Y"))
  
  #--------------------------------------
  # read rasters
  #--------------------------------------
  str_names_lst=list.files(demPath)
  imported_raster_lst <-lapply(sprintf("%s/%s",demPath,str_names_lst),  raster)
  
  assertthat::assert_that(length(imported_raster_lst)>0,
                          msg=sprintf("getAGLFromDEM(): didn't success to load DEM file from %s ",demPath))
  #--------------------------------------
  # assign ASL from DEM to each antena
  #--------------------------------------
  atlas.spdf$AGL<-NA
  for (i in 1:length(imported_raster_lst)){
    imported_raster<-imported_raster_lst[[i]]
    Alt.above.ground <- extract(imported_raster,atlas.spdf)
    atlas.spdf@data$AGL<-ifelse(is.na( atlas.spdf@data$AGL), Alt.above.ground,  atlas.spdf@data$AGL)
  }
  
  atlas.df<- atlas.spdf%>%
    as.data.frame()%>%
    dplyr::select(-LON,-LAT)
  
  return(list("raster_lst"=imported_raster_lst,
              "atlas.df"=atlas.df)) 
}



AGL_ll<-function(AGL.df){
  AGL.sf<-convert.sf.ITM2WGS84(AGL.df,  xyColNames  = c("X","Y"))
  AGL.sf$shortTag<- as.numeric(AGL.sf$TAG)%%10000
  
  tagsList<-unique(AGL.sf$shortTag)
  
  # Create a continuous palette function
  pal <- colorNumeric(
    palette = "Blues",
    domain = AGL.sf$AGL)
  
  
  llix<-leaflet() %>% 
    addTiles(options=tileOptions(opacity=0.5))
  
  layersList<-c()
  for (tg in tagsList){
    layerName<-sprintf("%d", tg)
    AGL.subset.sf<-AGL.sf%>%filter(shortTag==tg)
    
    llix<-llix%>%addCircleMarkers(
      data = AGL.subset.sf,  #  points data from spdf
      radius = 2,  # cycle radios
      opacity = 1,  # cycle transparency
      fillOpacity = 0.7,
      color = ~pal(AGL),  # point colour
      stroke = FALSE,
      popup = ~(sprintf("%d DateTime=%s:<br>ASL=%0.0f [m]<br>AGL=%0.0f [m]",
                        tg,
                        dateTime,
                        ASL, AGL)),
      group=layerName)
      layersList<-c(layersList, layerName)
  }
  # add layers controller
  llix<-llix%>%addLayersControl(
    overlayGroups = layersList,
    options = layersControlOptions(collapsed = FALSE, autoZIndex=TRUE)
  ) %>%
    # add scale bar
    addScaleBar(position = c("bottomleft"),
                options = scaleBarOptions(imperial=FALSE,maxWidth=200)) 
  return(llix)
}
