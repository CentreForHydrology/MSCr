# reads individual MSC files and creates obs files


readHourlyWind <- function(directory, timezone){
  library(CRHMr)
  
  # reads all hourly wind data and assembles a .obs file
  setwd(directory)
  filespec <- 'WIND*'
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
    # set up widths to read
    header <- c(2,2,2)
    header.classes <- c('numeric','numeric','numeric')
    
    cols <- rep.int(c(2,2),24)
    cols.classes <- rep.int(c('character', 'numeric'), 24)
    all <- c(header,cols,5)
    all.classes <- c(header.classes, cols.classes, 'character')
    
    # read data
    raw <- read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes) 
    row.count <- nrow(raw)
    dir.cols <- seq(4,50,2)
    data.cols  <- dir.cols + 1
    
    years <- raw[,1] + 1900
    months <- raw[,2]
    days <- raw[,3]
    
    # now unstack time series
    data.values <-  raw[,data.cols]
    dir.codes <-  raw[,dir.cols]
    
    # transpose data
    data.values.t <- data.frame(t(data.values))
    
    # now stack data frames to vectors
    data.values.vector <- stack(data.values.t)
    
    # replicate days, months, years
    hours <- seq(1:24)
    all.hours <- rep(hours, row.count)
    all.days <- rep(days, each=24)      
    all.months <- rep(months, each=24)
    all.years <- rep(years, each=24)
    
    # create dates
    datestrings <- paste(all.years,'-', all.months,'-', all.days,' ', all.hours,':00', sep='')
    datetime <- as.POSIXct(datestrings, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all.data <- data.frame(datetime, data.values.vector[,1])
    names(all.data) <- c('datetime', 'u')
    
    # replace missing values
    all.data[(all.data[,2] <= -1), 2] <- NA_real_
    
    # stack time series
    if (i == 1)
      obs <- all.data
    else
      obs <- rbind(obs, all.data)
  }
  # sort by data
  obs <- obs[order(obs$datetime),]
  obs.name <- 'u.obs'
  
  # convert from km/h to m/s
  obs$u <- obs$u / 3.6
  result <- writeObsFile(obs, obs.name, 'obs')
}

readHourlyRH <- function(directory, timezone){
  library(CRHMr)
  
  # reads all hourly RH data and assembles a .obs file
  setwd(directory)
  filespec <- 'RH*'
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
    # set up widths to read
    header <- c(2,2,2)
    header.classes <- c('numeric','numeric','numeric')
    
    cols <- rep.int(5, 24)
    cols.classes <- rep.int(c('numeric'), 24)
    all <- c(header, cols)
    all.classes <- c(header.classes, cols.classes)
    
    # read data
    raw <- read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes) 
    row.count <- nrow(raw)
    data.cols <- seq(4,27)
    
    years <- raw[,3] + 1900
    months <- raw[,2]
    days <- raw[,1]
    
    # now unstack time series
    data.values <-  raw[,data.cols]
    
    # transpose data
    data.values.t <- data.frame(t(data.values))
    
    # now stack data frames to vectors
    data.values.vector <- stack(data.values.t)
    
    # replicate days, months, years
    hours <- seq(1:24)
    all.hours <- rep(hours, row.count)
    all.days <- rep(days, each=24)      
    all.months <- rep(months, each=24)
    all.years <- rep(years, each=24)
    
    # create dates
    datestrings <- paste(all.years,'-', all.months,'-', all.days,' ', all.hours,':00', sep='')
    datetime <- as.POSIXct(datestrings, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all.data <- data.frame(datetime, data.values.vector[,1])
    names(all.data) <- c('datetime', 'rh')
    
    # replace missing values
    all.data[(all.data[,2] > 100), 2] <- NA_real_
    
    # stack time series
    if (i == 1)
      obs <- all.data
    else
      obs <- rbind(obs, all.data)
  }
  # sort by data
  obs <- obs[order(obs$datetime),]
  obs.name <- 'rh.obs'
  result <- writeObsFile(obs, obs.name, 'obs')
}

readHourlyT <- function(directory, timezone){
  library(CRHMr)
  
  # reads all hourly air temp data and assembles a .obs file
  setwd(directory)
  filespec <- 'TE*'
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
    # set up widths to read
    header <- c(2,2,2)
    header.classes <- c('numeric','numeric','numeric')
    
    cols <- rep.int(5, 24)
    cols.classes <- rep.int(c('numeric'), 24)
    all <- c(header, cols)
    all.classes <- c(header.classes, cols.classes)
    
    # read data
    raw <- read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes) 
    row.count <- nrow(raw)
    data.cols <- seq(4,27)
    
    years <- raw[,3] + 1900
    months <- raw[,2]
    days <- raw[,1]
    
    # now unstack time series
    data.values <-  raw[,data.cols]
    
    # transpose data
    data.values.t <- data.frame(t(data.values))
    
    # now stack data frames to vectors
    data.values.vector <- stack(data.values.t)
    
    # replicate days, months, years
    hours <- seq(1:24)
    all.hours <- rep(hours, row.count)
    all.days <- rep(days, each=24)      
    all.months <- rep(months, each=24)
    all.years <- rep(years, each=24)
    
    # create dates
    datestrings <- paste(all.years,'-', all.months,'-', all.days,' ', all.hours,':00', sep='')
    datetime <- as.POSIXct(datestrings, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all.data <- data.frame(datetime, data.values.vector[,1])
    names(all.data) <- c('datetime', 't')
    
    # replace missing values
    all.data[(all.data[,2] > 100), 2] <- NA_real_
    
    # stack time series
    if (i == 1)
      obs <- all.data
    else
      obs <- rbind(obs, all.data)
  }
  # sort by data
  obs <- obs[order(obs$datetime),]
  obs.name <- 't.obs'
  result <- writeObsFile(obs, obs.name, 'obs')
}

readDailyP <- function(directory, timezone=''){
  library(CRHMr)
  
  # reads all daily precipdata and assembles a .obs file
  # assumption is that only days with precipitaion were recorded
  
  setwd(directory)
  filespec <- 'A0O*'
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
     
    # read data
    raw <- read.table(file=infile, header=FALSE, skip=7, stringsAsFactors=FALSE)
    
    # collect req'd data
    raw <- raw[,c(1, 5, 6, 9)]
    names(raw) <- c( 'year', 'month', 'day', 'ppt')
    raw$year <- as.numeric(raw$year)
    raw$month <- as.numeric(raw$month)
    raw$day <- as.numeric(raw$day)
    raw$ppt <- as.numeric(raw$ppt)
    
    # check for the last date being Jan 1 of next year - set to Dec 31
    last.row <- nrow(raw)
    if((raw$month[last.row] == 1) & (raw$day[last.row] == 1) & 
         (raw$year[last.row] == raw$year[last.row-1])){  
      raw$year[last.row] == raw$year[last.row] + 1
      last.date <- as.Date(paste((raw$year[last.row]+1900), '-01-01', sep=''), format='%Y-%m-%d')       
    }
    else
      last.date <- as.Date(paste((raw$year[last.row]+1900), '-12-31', sep=''), format='%Y-%m-%d') 
    
    raw$date <- as.Date(paste((raw$year+1900), '-', raw$month, '-', raw$day, sep=''),
                        format='%Y-%m-%d')
    
    # create annual date/time series
    first.date <- as.Date(paste((raw$year[2]+1900), '-01-01', sep=''), format='%Y-%m-%d')
   
    all <- data.frame(seq(from=first.date, to=last.date, by=1))
    names(all) <- 'date'

    raw <- subset(raw, select=c(date, ppt))
    # merge
    all <- merge(all, raw, by='date', all.x=TRUE)
    all$date <- format(all$date, format='%Y-%m-%d')
    all$datetime <- paste(all$date, ' 01:00', sep='')
    
    # create datetimes
    all$datetime <- as.POSIXct(all$datetime, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all <- subset(all, select=c(datetime, ppt))

    # replace missing values
    all[is.na(all[,2]), 2] <- 0
    
    # stack time series
    if (i == 1)
      obs <- all
    else
      obs <- rbind(obs, all)
  }
  
  # sort by data
  obs <- obs[order(obs$datetime),]
  obs.name <- 'ppt.obs'
  result <- writeObsFile(obs, obs.name, 'obs')
}

readDailyTminTmax <- function(directory, filespec, timezone=''){
  library(CRHMr)
  
  # reads all daily tmin/tmax and assembles a .obs file
  # assumption is that only days with precipitaion were recorded
  
  setwd(directory)
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  # extract first 2 charcaters from filespec
  first2 <- substr(filespec, 1, 2)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
    
    # read data
    raw <- read.table(file=infile, header=FALSE, skip=3, stringsAsFactors=FALSE)
    last.row <- nrow(raw)
    
    # collect req'd data
    raw <- raw[,c(1, 2, 3, 18, 19)]
    names(raw) <- c( 'year', 'month', 'day', 'tmax', 'tmin')
    raw$year <- as.numeric(raw$year) + 1900
    raw$month <- as.numeric(raw$month)
    raw$day <- as.numeric(raw$day)
    raw$tmax <- as.numeric(raw$tmax)
    raw$tmin <- as.numeric(raw$tmin)
    
    
    raw$date <- as.Date(paste(raw$year, '-', raw$month, '-', raw$day, sep=''),
                        format='%Y-%m-%d')
    
    # create annual date/time series
    first.date <- as.Date(paste(raw$year[1], '-', raw$month[1], '-', raw$day[1], sep=''), 
                          format='%Y-%m-%d')
    last.date <- as.Date(paste(raw$year[last.row], '-', raw$month[last.row], '-', 
                               raw$day[last.row], sep=''), format='%Y-%m-%d')
    
    all <- data.frame(seq(from=first.date, to=last.date, by=1))
    names(all) <- 'date'   
    raw <- subset(raw, select=c(date, tmin, tmax))
    
    # merge
    all <- merge(all, raw, by='date', all.x=TRUE)
    all$date <- format(all$date, format='%Y-%m-%d')
    all$datetime <- paste(all$date, ' 01:00', sep='')
    
    # create datetimes
    all$datetime <- as.POSIXct(all$datetime, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all <- subset(all, select=c(datetime, tmin, tmax))
    
    # replace missing values with NA
    all[((all[, 2] < -98) | (all[, 2] > 98)), 2] <- NA_real_
    all[((all[, 3] < -98) | (all[, 3] > 98)), 3] <- NA_real_
  
    
    
    # stack time series
    if (i == 1)
      obs <- all
    else
      obs <- rbind(obs, all)
  }
  
  # sort by data
  obs <- obs[order(obs$datetime),]
  obs.name <- paste(first2, '_tminmax.obs', sep='')
  result <- writeObsFile(obs, obs.name, 'obs')
}
