createDailyTempObsFile <-
function(MSC, ObsFile, station.name, station.number, quiet){
  # writes dataframe of MSC daily temps to .obs file
  ObsData <- subset(MSC, select=c(Date.Time, Max.Temp...C., Min.Temp...C., Mean.Temp...C.))
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
  if (!quiet){
    cat('Daily temperatures\n')
    file.info <- CRHM_summary(ObsData)
    print(file.info) 
  }

  comment1 <- paste('MSC daily tmax, tmean, tmin data for station: ', station.name, sep='')
  ObsData$datetime <- paste(format(ObsData$datetime, format='%Y %m %d'),' 01 0',sep='')
  cat(comment1,win.eol(),'tmax 1 (C)',win.eol(),'tmin 1 (C)',win.eol(),'tmean 1 (C)',win.eol(),
      '###############\ttmax.1\ttmin.1\ttmean.1',win.eol(), file=ObsFile, sep='')
  write.table(ObsData, file=ObsFile, sep='\t',col.names=FALSE, row.names=FALSE, 
              quote=FALSE, eol = win.eol(), append=TRUE) 
  
}
