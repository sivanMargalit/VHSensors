library(dplyr)
library(ggplot2)
library(xlsx)



get.ISM.data<-function(easting=257000,
                       northing=780000,
                       fromTime,
                       toTime,
                       ims.token,
                       ISM.dataFile='data/metadata10minutesIMS_IDENVISTA080218_1.xlsx'){
  # download data from the Israel Meteorological Service via their API.
  # documentation: https://ims.gov.il/en/ObservationDataAPI
  # dates are always UTC+2 (Israel standard or winter time).
  
  toTime<-max(toTime, fromTime + 24*60*60)
  toTime   <- toTime   + 24*60*60;
  fromTime <- fromTime - 24*60*60;
  
  # posix time is utc and IMS uses Israel standard time, UTC+02:00
  if (is.double(fromTime)) {
    fromTime <- as.POSIXct(fromTime, origin="1970-01-01");
  }
  
  if (is.double(toTime)){
    toTime <-as.POSIXct(toTime, origin="1970-01-01");
  }
  
  if (is.null(ISM.dataFile)){
    ISM.dataFile<-'data/metadata10minutesIMS_IDENVISTA080218_1.xlsx'
    print('Reading station metadata from IMS...');
    rslt<-download.file(url='https://ims.data.gov.il/sites/default/files/metadata10minutesIMS_IDENVISTA080218_1.xlsx', 
                        destfile=ISM.dataFile,
                        mode='wb')
    if (rslt==0){
      print(sprintf('complete download file %s station metadata from IMS...',
                    ISM.dataFile));
    }else{
      print(sprintf('failed to download IMS data. return errr code %d',
                    rslt));
    }
  }
  
  raw.ISM.1<-NULL
  if (file.exists(ISM.dataFile)){
    raw.ISM.1<-read.xlsx(ISM.dataFile, 
                         sheetIndex=1, # מטה-דטה
                         encoding="UTF-8",
                         startRow =2)
  } else{
    er <- errorCondition(sprintf("get.ISM.data: did not find file %s", 
                                 ISM.dataFile))
    stop(er)
  }
  
  if (!is.null(raw.ISM.1)){
    if (nrow(raw.ISM.1)>0) {
      col.names<-colnames(raw.ISM.1)
      col.names[1]<-"no"
      col.names[2]<-"stationId"
      col.names[3]<-"He station Name"
      col.names[4]<-"En station Name"
      col.names[5:9]<-c("east.ITM","north.ITM", "LON", "LAT","stationHsl")
      col.names[10]<-"open Date"
      col.names[11]<-"Variables"
      colnames(raw.ISM.1)<-col.names
      
      ISM.1<-raw.ISM.1%>%
        mutate("targetDistance"=sqrt((east.ITM-easting)^2+(north.ITM-northing)^2))
      hasPressureData<-grep("לחץ", ISM.1$Variables)

      ISM.1<-ISM.1[hasPressureData,]
      closer.ix<-which.min(ISM.1$targetDistance)
      targetName = ISM.1$`En station Name`[closer.ix]
      targetId   = ISM.1$stationId[closer.ix]
      targetHsl  = ISM.1$stationHsl[closer.ix]
    } 
  }
  
  ims.stations.URL<-'https://api.ims.gov.il/v1/envista/stations'
  
  parsed_stations<-getStructuredData(URL=ims.stations.URL,
                                     ApiToken = ims.token, 
                                     timeOut=40)
  
  
  # find station in parsed_stations list 
  #--------------------------------------
  station_data<-NULL
  i<-0
  while((is.null(station_data)) & (i<length(parsed_stations))){
    i<-i+1
    if (parsed_stations[[i]]$stationId==targetId){
      station_data<-parsed_stations[[i]]  
    }
  }
  
  if (is.null(station_data)){
    er <- errorCondition(sprintf("get.ISM.data: did not find stationId %d (%s)", 
                                 targetId, targetName))
    stop(er)
  }

  # find barometric & Temperatur  Monitor channel in station_data object 
  #--------------------------------------------------------------------
  i<-0
  BarometricMonior<-NULL
  TemperatureMonior<-NULL
  while((is.null(BarometricMonior) | is.null(TemperatureMonior)) &
        (i<length(station_data$monitors))){
    i<-i+1
    if (station_data$monitors[[i]]$name=='BP'){
      BarometricMonior<-station_data$monitors[[i]]  
      BP.channelId = station_data$monitors[[i]]$channelId;
      BP.channelActive = station_data$monitors[[i]]$active;
      BP.channelUnits  = station_data$monitors[[i]]$units;
    }
    if (station_data$monitors[[i]]$name=='TD'){
      TemperatureMonior<-station_data$monitors[[i]]  
      TD.channelId = station_data$monitors[[i]]$channelId;
      TD.channelActive = station_data$monitors[[i]]$active;
      TD.channelUnits  = station_data$monitors[[i]]$units;
    }
  }

  if (is.null(BarometricMonior)){
    er <- errorCondition(sprintf("get.ISM.data: did not find BarometricMonior in station '%s'", 
                                 targetName))
    stop(er)
  } else  if (BP.channelActive==FALSE){
    er <- errorCondition(sprintf("get.ISM.data: BP channel is not active in station '%s'", 
                                 targetName))
    stop(er)
  }
  
  if (is.null(TemperatureMonior)){
    er <- errorCondition(sprintf("get.ISM.data: did not find temperature channel in station '%s'", 
                                 targetName))
    stop(er)
  } else  if (TD.channelActive==FALSE){
    er <- errorCondition(sprintf("get.ISM.data: TD (Temperature Dry) channel is not active in station '%s'", 
                                 targetName))
    stop(er)
  }
  # Get barometric data during the period
  #----------------------------------------
  station.Bar.URL<-sprintf('https://api.ims.gov.il/v1/envista/stations/%d/data/%d?from=%s&to=%s',
                           targetId,BP.channelId,
                           strftime(fromTime, format="%Y/%m/%d"),
                           strftime(toTime, format="%Y/%m/%d"))
  
  bp.data<-getStructuredData(URL=station.Bar.URL,
                             ApiToken = ims.token, 
                             timeOut=30)
  
  # data is an array with stuctures that contains two fileds, datestime in this format '2020-12-06T23:50:00+02:00'
  # and channels, an array of structs with fields value and valid (and some other)
  # The datetime values seem to be in ISO 8601 and R should know how to
  # deal with them.
  
  times<-sapply(bp.data$data, 
                function(x) as.POSIXct(x$datetime, format="%Y-%m-%dT%H:%M:%OS"))
  barValues<-sapply(bp.data$data, 
                    function(x) x$channels[[1]]$value)
  ism.pressure.df<-data.frame("TIME"=as.numeric(as.POSIXct(times,origin='1970-01-01')),
                              "value"=barValues)
  
  # Get temperatue from IMS station, during the period
  #----------------------------------------------------
  station.temp.URL<-sprintf('https://api.ims.gov.il/v1/envista/stations/%d/data/%d?from=%s&to=%s',
                           targetId,TD.channelId,
                           strftime(fromTime, format="%Y/%m/%d"),
                           strftime(toTime, format="%Y/%m/%d"))
  
  td.data<-getStructuredData(URL=station.temp.URL,
                             ApiToken = ims.token, 
                             timeOut=30)
  
  # data is an array with stuctures that contains two fileds, datestime in this format '2020-12-06T23:50:00+02:00'
  # and channels, an array of structs with fields value and valid (and some other)
  # The datetime values seem to be in ISO 8601 and R should know how to
  # deal with them.
  
  times<-sapply(td.data$data, 
                function(x) as.POSIXct(x$datetime, format="%Y-%m-%dT%H:%M:%OS"))
  TemperatureValues<-sapply(td.data$data, 
                    function(x) x$channels[[1]]$value)
  
  ism.temp.df<-data.frame("TIME"=as.numeric(as.POSIXct(times,origin='1970-01-01')),
                          "value"=TemperatureValues)
  
  targetHsl<-ifelse (is.factor(targetHsl), levels(targetHsl)[targetHsl], targetHsl)
  return(list("hsl"=as.numeric(targetHsl),
              "pressure"=ism.pressure.df,
              "temeratures"=ism.temp.df))
}


sensorsPressureToAltitude<-function(pressure,      # sensors pressure
                                    refPressure,   # reference pressure from IMS
                                    refTemperature=20, # reference temperature from IMS
                                    refAltitude){   # station altitude (hsl)
  # interpulation of expected pressure of tags time stamp, based of ims data
  Interpulate.pressure<-data.frame(approx(refPressure$TIME, refPressure$value,  
                                 xout = pressure$TIME,
                                 rule = 2, method = "linear", ties = mean))

  # if refTemperature is array from IMS - using interpolation to estimate temperature on sensrsors measurements
  if (is.data.frame(refTemperature)){
    Interpulate.temperature<-data.frame(approx(refTemperature$TIME, refTemperature$value,  
                                               xout = pressure$TIME,
                                               rule = 2, method = "linear", ties = mean))
  }else{ #if refTemperature is single value - create psaudo-dataframe with this value
    Interpulate.temperature<-data.frame("TIME"=pressure$TIME,
                                        "value"=rep(refTemperature, length(pressure$TIME)))
  }
  
  
  colnames(Interpulate.pressure)<-c("TIME","value")
  colnames(Interpulate.temperature)<-c("TIME","value")
  
  alts = refAltitude + (1/0.0065)*( (Interpulate.pressure$value / pressure$Pressure) ^ (1/5.255)-1) * (273.15+Interpulate.temperature$value)
  
  alts.df<-cbind(pressure_df , "ASL"=alts)
  
  return(alts.df)
  
  
}


plot_BME<-function(bme.df,
                   reference.df=NULL,
                   fromTime=NULL,
                   toTime=NULL) {
  
  if (is.null(fromTime)){
    fromTime=min(bme.df$TIME)
  }
  if (is.null(toTime)){
    toTime=max(bme.df$TIME)
  }
  
  bme.df.1<-bme.df%>%
    filter(between(TIME, fromTime,toTime))%>%
    mutate("dateTime"=as.POSIXct(TIME, tz="UTC", "1970-01-01"))%>%
    rename("ID"=TAG)%>%
    dplyr::select(ID, dateTime, Pressure)
  
  if (! is.null(reference.df)){
    bme.df.1<-rbind(bme.df.1, 
                    reference.df%>%
                      filter(between(TIME, fromTime,toTime))%>%
                      mutate("dateTime"=as.POSIXct(TIME, tz="UTC", "1970-01-01"),
                             "ID"="reference")%>%
                      rename("Pressure"=value)%>%
                      dplyr::select(ID, dateTime, Pressure))
  }
  gg<-ggplot(bme.df.1,
             aes(x=dateTime, y=Pressure))+
    geom_point(aes(colour = factor(ID)), size=0.5)+
    theme_bw()+
    theme(legend.title = element_blank())
  
  return(gg)  
}