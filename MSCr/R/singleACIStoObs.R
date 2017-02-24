singleACIStoObs <- function(ACISfile='', outDir='', timezone='etc/GMT+7'){
   if (ACISfile==''){
    cat('Error: ACIS file missing\n')
    return(FALSE)
  }
  
  if(outDir==''){
    outDir <- dirname(ACISfile)
  }
  
  header <- read.table(file=ACISfile, nrows=1, encoding = 'UTF8', sep=',', stringsAsFactors = FALSE)
  ACIS <- read.csv(file=ACISfile, skip=1, stringsAsFactors = FALSE, header = FALSE, fill=TRUE)
  
  # get station names
  stationNames <- unique(ACIS[,1])
  
  # convert datetimes to standard
  datetime <- as.POSIXct(ACIS[,2], format='%d-%B-%Y %H:%M', tz=timezone)
  
  # figure out variable names
  varcount <- floor((ncol(ACIS) -2) / 3)
  varCols <- seq(from=1, to=varcount, by=3)
  varCols <- varCols + 2
  varNames <- header[varCols]
  # remove non-ascii chars
  varNames <- str_replace_all(varNames,'\xb0','')
  varNames <- stringr::str_trim(stringr::str_replace_all(varNames,' ', '_'))
  
  varValues <- ACIS[,varCols]
  obs <- data.frame(datetime, varValues)
  names(obs) <- c('datetime', varNames)
  
  # get source flags and comments
  sourceCols <- varCols +1
  commentCols <- sourceCols +1
  ord1 <- 2*(1:length(sourceCols))-1
  ord2 <- 2*(1:length(commentCols))
  allCommentCols <- c(v1,v2)[order(c(ord1,ord2))]
  comments <- ACIS[,allCommentCols]
  commentNames <- header[allCommentCols]
  commentNames <- str_replace_all(commentNames,'\xb0','')
  names(comments) <- stringr::str_trim(stringr::str_replace_all(commentNames,' ', '_'))

  # p.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                     stringr::fixed('precip.')))
  # qi.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                      stringr::fixed('incoming solar rad.'))) 
  # u10.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                       stringr::fixed('wind speed 10')))
  # u2.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                      stringr::fixed('wind speed 2')))
  # t.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                     stringr::fixed('air temp.')))
  # rh.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                      stringr::fixed('humidity')))
  # SnowDepth.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                             stringr::fixed('snow depth')))
  # SoilMoisture005.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                                  stringr::fixed('soil moisture 005')))
  # SoilMoisture020.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                                  stringr::fixed('soil moisture 020')))
  # SoilMoisture100.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                                   stringr::fixed('soil moisture 100')))
  # SoilTemp005.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                                   stringr::fixed('soil temp. 005')))
  # SoilTemp020.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                                   stringr::fixed('soil temp. 020')))
  # SoilTemp100.cols <- which(stringr::str_detect(stringr::str_to_lower(varNames), 
  #                                                   stringr::fixed('soil temp. 100')))
  # 
  # # assemble dataframe
  # obs <- data.frame(datetime)
  # 
  # p.length <- length(p.cols)
  # if (p.length > 0){
  #   p.df <- data.frame(varValues[,p.cols])
  #   p.nums <- seq(from=1, to=p.length )
  #   names(p.df) <- paste('p.', p.nums, sep='')
  #   obs <- cbind(obs, p.df)
  # }
  # 
  # t.length <- length(t.cols)
  # if (t.length > 0){
  #   t.df <- data.frame(varValues[,t.cols])
  #   t.nums <- seq(from=1, to=t.length )
  #   names(t.df) <- paste('t.', t.nums, sep='')
  #   obs <- cbind(obs, t.df)
  # }
  # 
  # u10.length <- length(u10.cols)
  # if (u10.length > 0){
  #   u10.df <- data.frame(varValues[,u10.cols])
  #   u10.nums <- seq(from=1, to=u10.length )
  #   names(u10.df) <- paste('u10.', u10.nums, sep='')
  #   obs <- cbind(obs, u10.df)
  # }
  # 
  # u2.length <- length(u2.cols)
  # if (u2.length > 0){
  #   u2.df <- data.frame(varValues[,u2.cols])
  #   u2.nums <- seq(from=1, to=u2.length )
  #   names(u2.df) <- paste('u2.', u2.nums, sep='')
  #   obs <- cbind(obs, u10.df)
  # }
  # 
  # rh.length <- length(rh.cols)
  # if (rh.length > 0){
  #   rh.df <- data.frame(varValues[,rh.cols])
  #   rh.nums <- seq(from=1, to=rh.length )
  #   names(rh.df) <- paste('rh.', rh.nums, sep='')
  #   obs <- cbind(obs, rh.df)
  # }
  # 
  # qi.length <- length(qi.cols)
  # if (qi.length > 0){
  #   qi.df <- data.frame(varValues[,qi.cols])
  #   qi.nums <- seq(from=1, to=qi.length )
  #   names(qi.df) <- paste('qi.', qi.nums, sep='')
  #   obs <- cbind(obs, qi.df)
  # }
  # 
  # SnowDepth.length <- length(SnowDepth.cols)
  # if (SnowDepth.length > 0){
  #   SnowDepth.df <- data.frame(varValues[,SnowDepth.cols])
  #   SnowDepth.nums <- seq(from=1, to=SnowDepth.length )
  #   names(SnowDepth.df) <- paste('SnowDepth.', SnowDepth.nums, sep='')
  #   obs <- cbind(obs, SnowDepth.df)
  # }
  # 
  # SoilMoisture005.length <- length(SoilMoisture005.cols)
  # if (SoilMoisture005.length > 0){
  #   SoilMoisture005.df <- data.frame(varValues[,SoilMoisture005.cols])
  #   SoilMoisture005.nums <- seq(from=1, to=SoilMoisture005.length )
  #   names(SoilMoisture005.df) <- paste('SoilMoisture005.', SoilMoisture005.nums, sep='')
  #   obs <- cbind(obs, SoilMoisture005.df)
  # }
  # 
  # SoilMoisture020.length <- length(SoilMoisture020.cols)
  # if (SoilMoisture020.length > 0){
  #   SoilMoisture020.df <- data.frame(varValues[,SoilMoisture020.cols])
  #   SoilMoisture020.nums <- seq(from=1, to=SoilMoisture020.length )
  #   names(SoilMoisture020.df) <- paste('SoilMoisture020.', SoilMoisture020.nums, sep='')
  #   obs <- cbind(obs, SoilMoisture020.df)
  # }
  # 
  # SoilMoisture100.length <- length(SoilMoisture100.cols)
  # if (SoilMoisture100.length > 0){
  #   SoilMoisture100.df <- data.frame(varValues[,SoilMoisture100.cols])
  #   SoilMoisture100.nums <- seq(from=1, to=SoilMoisture100.length )
  #   names(SoilMoisture100.df) <- paste('SoilMoisture100.', SoilMoisture100.nums, sep='')
  #   obs <- cbind(obs, SoilMoisture100.df)
  # }
  # 
  # SoilTemp005.length <- length(SoilTemp005.cols)
  # if (SoilTemp005.length > 0){
  #   SoilTemp005.df <- data.frame(varValues[,SoilTemp005.cols])
  #   SoilTemp005.nums <- seq(from=1, to=SoilTemp005.length )
  #   names(SoilTemp005.df) <- paste('SoilTemp005.', SoilTemp005.nums, sep='')
  #   obs <- cbind(obs, SoilTemp005.df)
  # }
  # 
  # SoilTemp020.length <- length(SoilTemp020.cols)
  # if (SoilTemp020.length > 0){
  #   SoilTemp020.df <- data.frame(varValues[,SoilTemp020.cols])
  #   SoilTemp020.nums <- seq(from=1, to=SoilTemp020.length )
  #   names(SoilTemp020.df) <- paste('SoilTemp020.', SoilTemp020.nums, sep='')
  #   obs <- cbind(obs, SoilTemp020.df)
  # }
  # 
  # SoilTemp100.length <- length(SoilTemp100.cols)
  # if (SoilTemp100.length > 0){
  #   SoilTemp100.df <- data.frame(varValues[,SoilTemp100.cols])
  #   SoilTemp100.nums <- seq(from=1, to=SoilTemp100.length )
  #   names(SoilTemp100.df) <- paste('SoilTemp100.', SoilTemp100.nums, sep='')
  #   obs <- cbind(obs, SoilTemp100.df)
  # }
  
  # subdivide by station and output
  stationCount <- length(stationNames)
  
  for (i in 1:stationCount){
    stationLocs <- which(ACIS[,1] == stationNames[i])
    stationObs <- obs[stationLocs,]
    # change space to underscore in station names
    stationNameClean <- stringr::str_trim(stringr::str_replace(stationNames[i],' ', '_'))
    
    # write output file name
    obsFile <- paste(outDir,'/', stationNameClean, '.obs', sep='')
    CRHMr::writeObsFile(stationObs, obsFile, comment='ACIS hourly data')
    
    # write variable information
    commentFile <- paste(outDir,'/', stationNameClean, '_comments.csv', sep='')
    write.csv(comments, file=commentFile, row.names=FALSE)
  }
  
}