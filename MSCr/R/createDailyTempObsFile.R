createDailyTempObsFile <-
function(MSC, ObsFile, station.name, station.number, quiet, logfile){
  # writes dataframe of MSC daily temps to .obs file
  ObsData <- MSC[, c('Date.Time', 'Max.Temp...C.', 'Min.Temp...C.', 'Mean.Temp...C.')]
  names(ObsData) <- c('datetime','tmax', 'tmin', 'tmean')
  ObsData$datetime <- as.POSIXct(as.character(ObsData$datetime), format='%Y-%m-%d', tz='')
  ObsData$tmax <- as.numeric(ObsData$tmax)
  ObsData$tmin <- as.numeric(ObsData$tmin)  
  ObsData$tmean <- as.numeric(ObsData$tmean)
  
  # sort obs data
  ObsData <- ObsData[order(ObsData$datetime),]
  if (anyDuplicated(ObsData$datetime)){
    dupes <- duplicated(ObsData$datetime)
    ObsData <- ObsData[!dupes,]
  }
  # output info to screen and write to log file

  CRHMr::writeObsFile(obs=ObsData,  obsfile=ObsFile, quiet=quiet, logfile=logfile)
  
}
