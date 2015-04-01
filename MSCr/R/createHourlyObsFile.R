createHourlyObsFile <-
function(MSC, ObsFile, station.name, station.number,  timezone, quiet, logfile){
  # writes MSC hourly data to .obs file
  
  ObsData <- subset(MSC, select=c(Date.Time, Temp...C., Rel.Hum...., Wind.Spd..km.h. ))
    
  # convert time string to POSIX
  ObsData$Date.Time<- as.POSIXct(as.character(ObsData$Date.Time), 
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
  
writeObsFile(obs.dataframe=ObsData,  obsfile=ObsFile, obsname='HourlyObs',
             quiet=quiet, logfile=logfile)
}
