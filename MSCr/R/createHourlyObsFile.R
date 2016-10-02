createHourlyObsFile <-
function(MSC, ObsFile, station.name, station.number,  timezone, quiet, logfile){
  # writes MSC hourly data to .obs file
  
  ObsData <- MSC[,c('Date/Time', 'Temp (degC)', 'Rel Hum (%)', 'Wind Spd (km/h)')]
    
  # convert time string to POSIX
  ObsData$'Date/Time' <- as.POSIXct(as.character(ObsData$'Date/Time'), 
                                 format='%Y-%m-%d %H:%M', tz=timezone)
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
  
   result <- CRHMr::writeObsFile(obs=ObsData, obsfile=ObsFile, quiet=quiet, logfile=logfile)
}
