createDailyPrecipObsFile <-
function(MSC, ObsFile, station.name, station.number, quiet, logfile){
  # writes MSC daily precips to .obs file
  ObsData <- MSC[,c('Date/Time', 'Total Precip (mm)')]
  names(ObsData) <- c('datetime', 'ppt')
  ObsData$datetime <- as.POSIXct(as.character(ObsData$datetime), format='%Y-%m-%d', tz='')
 
  # sort obs data
  ObsData <- ObsData[order(ObsData$datetime),]
  if (anyDuplicated(ObsData$datetime)){
    dupes <- duplicated(ObsData$datetime)
    ObsData <- ObsData[!dupes,]
  }
  
  # output info to screen and write to log file
  CRHMr::writeObsFile(obs=ObsData, obsfile=ObsFile, quiet=quiet, logfile=logfile)
}
