createHourlyObsFile <-
function(MSC, ObsFile, station.name, station.number, quiet){
  # writes MSC hourly data to .obs file
  
  ObsData <- subset(MSC, select=c(Date.Time, Temp...C., Rel.Hum...., Wind.Spd..km.h. ))
    
  # convert time string to POSIX
  ObsData$Date.Time<- as.POSIXct(as.character(ObsData$Date.Time), 
                                 format='%Y-%m-%d %H:%M', tz='')
  names(ObsData) <- c('datetime','t','rh','u')
  
  # convert u from km/h to m/s
  ObsData$u <- ObsData$u / 3.6
  
  # sort obs data
  ObsData <- ObsData[order(ObsData$datetime),]
  
  # check for repeated datetimes
  if (anyDuplicated(ObsData$datetime)){
    dupes <- duplicated(ObsData$datetime)
    ObsData <- ObsData[!dupes,]
  }
  
# output to logfile
  if (!quiet){
    cat('Hourly met data\n')
    file.info <- CRHM_summary(ObsData)
    print(file.info)
  }
 # convert date/time to CRHM obs format
  ObsData$datetime <- format(ObsData$datetime, format='%Y %m %d %H %M')

  comment1 <- paste('MSC hourly data for station: ', station.name, sep='')

  cat(comment1, win.eol(),'t 1 (C)',win.eol(),'rh 1 (%)',win.eol(),'u 1 (m/s)',win.eol(),'$ea ea(t, rh)',win.eol(),
        '################\tt.1\trh.1\tu.1',win.eol(), file=ObsFile, sep='')
  write.table(ObsData, file=ObsFile, sep='\t',col.names=FALSE, row.names=FALSE, 
                quote=FALSE, eol = win.eol(), append=TRUE)
}
