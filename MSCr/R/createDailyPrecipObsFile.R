createDailyPrecipObsFile <-
function(MSC, ObsFile, station.name, station.number, quiet){
  # writes MSC daily precips to .obs file
  ObsData <- subset(MSC, select=c(Date.Time, Total.Precip..mm.))
  names(ObsData) <- c('datetime', 'ppt')
  ObsData$datetime <- as.POSIXct(as.character(ObsData$datetime), format='%Y-%m-%d', tz='')
 
  # sort obs data
  ObsData <- ObsData[order(ObsData$datetime),]
  if (anyDuplicated(ObsData$datetime)){
    dupes <- duplicated(ObsData$datetime)
    ObsData <- ObsData[!dupes,]
  }
  
  # output info to screen and write to log file
  if (!quiet){
    cat('Daily precipitation\n')
    file.info <- CRHM_summary(ObsData)
    print(file.info)   
  }
 
  
  comment1 <- paste('MSC daily precip data for station: ', station.name, sep='')    
  ObsData$datetime <- paste(format(ObsData$datetime, format='%Y %m %d'),' 01 0',sep='')
  cat(comment1,win.eol(),'ppt 1 (mm/d)',win.eol(),'#################\tppt.1',win.eol(), file=ObsFile, sep='')
  write.table(ObsData, file=ObsFile, sep='\t',col.names=FALSE, row.names=FALSE, 
              quote=FALSE, eol = win.eol(), append=TRUE) 
}
