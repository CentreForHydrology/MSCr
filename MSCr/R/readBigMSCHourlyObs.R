readBigMSCHourlyObs <-
function(infile, timezone=''){
  library(CRHMr)
  # reads large MSC files holding hourly values of several values for serveral locations
  # and exports an hourly obs data file for each site
  
  met.codes <- c('076', '123','080', '086', '091', '078')
  met.code.names <- c('u', 'p' ,'rh', 'rain', 'snow', 't' )
  
  # set up widths to read
  header <- c(7,4,2,2,3)
  header.classes <- c('character','numeric','numeric','numeric','character')
  
  cols <- rep.int(c(6,1),24)
  cols.classes <- rep.int(c('numeric', 'character'), 24)
  all <- c(header,cols)
  all.classes <- c(header.classes, cols.classes)
  
  # read data
  raw <- read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes)
  data.cols  <- seq(6,52,2)
  code.cols <- data.cols + 1
  
  # subset data by station and type
  station <- raw[,1]
  names(raw)[1] <- 'station'
  all.stations <- unique(station)
  stations.count <- length(all.stations)
  for (station.num in 1:stations.count){
    each.station <- all.stations[station.num]
    cat('station = ', each.station, '\n', sep='')
    station.data <- subset(raw, station == each.station)
    names(station.data)[5] <- 'type'
    
    # find min and max dates for the station
    station.dates <- as.Date(paste(station.data[,2],'-',station.data[,3],'-', 
                                   station.data[,4], sep=''), format='%Y-%m-%d')
    min.date <- min(station.dates)
    max.date <- max(station.dates)
    first.time <- paste(format(min.date, format='%Y-%m-%d'), ' 1:00', sep='')
    last.time <- paste(format(max.date, format='%Y-%m-%d'), ' 23:00', sep='')
    first.time <- as.POSIXct(first.time, format='%Y-%m-%d %H:%M', tz=timezone)
    last.time <- as.POSIXct(last.time, format='%Y-%m-%d %H:%M', tz=timezone)
    
    time.seq <- seq(from=first.time, to=last.time, by=3600)
    
    
    for (codenum in 1:6){
      each.code <- met.codes[codenum]
      each.code.name <- met.code.names[codenum]
      station.data.type <- subset(station.data, type == each.code)
      row.count <- nrow(station.data.type)
      if (row.count > 0){
        cat('code = ', each.code, ' name = ', each.code.name, '\n', sep='')
        years <- station.data.type[,2]
        months <- station.data.type[,3]
        days <- station.data.type[,4]
        
        # now unstack time series
        data.values <-  station.data.type[,data.cols]
        data.codes <-  station.data.type[,code.cols]
        
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
        names(all.data) <- c('datetime', each.code.name)
        
        # replace missing values
        all.data[(all.data[,2] <= -999), 2] <- NA_real_
      
        # remove all missing values
        all.data <- na.omit(all.data)
        names(all.data) <- c('datetime', each.code.name)
        
       
        # aggregate, to allow for their being more than 1 value per day
        
        all.data.agg <- aggregate(all.data[,2], by=list(all.data[,1]), FUN='mean')
        names(all.data.agg) <- c('datetime', each.code.name)
        

        # merge with complete dataset containing all NA values
        # to fill in missing values
        
        complete <- data.frame(time.seq)
        names(complete) <- c('datetime') 
        complete <- merge(complete, all.data.agg, all.x=TRUE)
        names(complete) <- c('datetime', each.code.name)     
        
        if (!exists('obs'))
          obs <- complete
        else{
          # merge then cbind
          complete <- data.frame(complete[,2])
          names(complete) <- each.code.name
          obs <- cbind(obs, complete)
        }          
      }  
    }
    
    obs.vars <- names(obs)
    if (exists('obs')){
      # do unit conversions and write to file
      if ('p' %in% obs.vars){
        obs$p <- obs$p * 0.1  # convert to mm        
        if (exists('rain') & exists('snow')){
          obs <- subset(obs, drop=c(rain, snow))
        }
      }    
      else if (('rain' %in% obs.vars) & ('snow' %in% obs.vars)){
        obs$p <- obs$rain * 0.1 + obs$snow
        obs <- subset(obs, drop=c(rain, snow))
      }
      
      if ('t' %in% obs.vars)      
        obs$t <- obs$t * 0.1  # convert to C     
      
      if ('u' %in% obs.vars)        
       obs$u <- obs$u / 3.6  # convert from km/h to m/s

      obs.name <- paste(each.station, '_hourly.obs', sep='')
      result <- writeObsFile(obs, obs.name, 'obs')
      rm(obs)
    }
  }
}
