#' Reads large MSC files of daily values
#'
#' @description Reads large MSC files holding daily values of several variables at several sites and exports an hourly CRHM \code{obs} data file for each site. The obs files are of the form \option{<sitenumber>_daily.obs'}
#' @param infile Required. Name of the file to be read.
#' @param quiet Optional.Suppresses display of messages, except for errors. If you are calling this function in an R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional.  Optional. Name of the file to be used for logging the action. Normally not used.
#' @return If successful, returns TRUE. If unsuccessful, returns the value FALSE.
#' @author Kevin Shook
#' @seealso  \code{\link{bigMSChourlyToObs}}
#' @examples
#' \dontrun{
#'bigMSCdailyToObs('GRPextractor_PHW_Bad_Lake_dlyv2_21032015_155647.txt', quiet=FALSE)}
#' @export

bigMSCdailyToObs <- function(infile, quiet=TRUE, logfile=''){
  # reads large MSC files holding hourly values of several values for serveral locations
  # and exports an hourly obs data file for each site
  
  if (infile==''){
    cat('Error: infile missing\n')
    return(FALSE)
  }
  
  met.codes <- c('001', '002','003', '012', '152')
  met.code.names <- c('tmax', 'tmin' ,'tmean', 'ppt', 'windrun' )
  
    
  # set up widths to read
  header <- c(7,4,2,3)
  header.classes <- c('character','numeric','numeric','character')
  
  cols <- rep.int(c(6,1),31)
  cols.classes <- rep.int(c('numeric', 'character'), 31)
  all <- c(header,cols)
  all.classes <- c(header.classes, cols.classes)
  
  # read data
  raw <- utils::read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes)
  data.cols  <- seq(5,65,2)
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
    names(station.data)[4] <- 'type'
    
    # find min and max dates for the station
    #station.dates <- as.Date(paste(station.data[,2],'-',station.data[,3],'-', 
    #                               station.data[,4], sep=''), format='%Y-%m-%d')
    min.date <- as.Date(paste(station.data[1,2],'-',station.data[1,3],'-01', sep=''),
                        format='%Y-%m-%d')
    last.row <- nrow(station.data)
    last.month <- station.data[last.row,3]
    last.month.start.date <- as.Date(paste(station.data[last.row,2],'-',station.data[last.row,3],
                                           '-01', sep=''), format='%Y-%m-%d')
    last.month.days <- lubridate::days_in_month(last.month.start.date)
    
    max.date <- last.month.start.date + last.month.days -1
    date.seq <- seq(from=min.date, to=max.date, by=1)   
    
    for (codenum in 1:5){
      each.code <- met.codes[codenum]
      each.code.name <- met.code.names[codenum]
      station.data.type <- station.data[station.data$type == each.code, ]
      row.count <- nrow(station.data.type)
      if (row.count > 0){
        cat('code = ', each.code, ' name = ', each.code.name, '\n', sep='')
        years <- station.data.type[,2]
        months <- station.data.type[,3]
        
        # now unstack time series
        data.values <-  station.data.type[,data.cols]
        data.codes <-  station.data.type[,code.cols]
        
        # transpose data
        data.values.t <- data.frame(t(data.values))
        
        # now stack data frames to vectors
        data.values.vector <- utils::stack(data.values.t)
        
        # replicate days, months, years
        days <- seq(1:31)
        all.days <- rep(days, row.count)
        all.months <- rep(months, each=24)
        all.years <- rep(years, each=24)
        
        # create dates
        datestrings <- paste(all.years,'-', all.months,'-', all.days, sep='')
        datetime <- as.Date(datestrings, format='%Y-%m-%d')
        
        # assemble data sets
        all.data <- data.frame(datetime, data.values.vector[,1])
        names(all.data) <- c('datetime', each.code.name)
        
        # remove bad dates
        bad.dates <- is.na(all.data$datetime)
        all.data <- all.data[!bad.dates,]    
        
        # replace missing values
        all.data[(all.data[,2] <= -999), 2] <- NA_real_
        
        # remove all missing values
        all.data <- stats::na.omit(all.data)
        names(all.data) <- c('datetime', each.code.name)
        
        
        # aggregate, to allow for their being more than 1 value per day
        
        all.data.agg <- stats::aggregate(all.data[,2], by=list(all.data[,1]), FUN='mean')
        names(all.data.agg) <- c('datetime', each.code.name)
        
        
        # merge with complete dataset containing all NA values
        # to fill in missing values
        
        complete <- data.frame(date.seq)
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
    

    if (exists('obs')){
      obs.vars <- names(obs)
      # do unit conversions and write to file

      if ('ppt' %in% obs.vars){
        obs$ppt <- obs$p * 0.1  # convert to mm        
      }  
      if ('t' %in% obs.vars)      
        obs$t <- obs$t * 0.1  # convert to C 
      
      if ('tmin' %in% obs.vars)      
        obs$tmin <- obs$tmin * 0.1  # convert to C 
      if ('tmax' %in% obs.vars)      
        obs$tmax <- obs$tmax * 0.1  # convert to C
      if ('tmean' %in% obs.vars)      
        obs$tmean <- obs$tmean * 0.1  # convert to C 
      
      if ('u' %in% obs.vars)        
        obs$u <- obs$u / 3.6  # convert from km/h to m/s
      
      if ('windrun' %in% obs.vars)        
        obs$u <- (obs$u * 1000) / (3600 * 24)  # convert from km/d to m/s
      
      obs.name <- paste(each.station, '_daily.obs', sep='')
      
      # add hour to datetime
      
      obs$datetime <- paste(obs$datetime, ' 01:00', sep='')
      obs$datetime <- as.POSIXct(obs$datetime, format='%Y-%m-%d %H:%M')
      obs.name <- paste(each.station, '_daily.obs', sep='')
      result <- CRHMr::writeObsFile(obs=obs, obsfile=obs.name, quiet=quiet, logfile=logfile)
      rm(obs)
    }
  }
}
