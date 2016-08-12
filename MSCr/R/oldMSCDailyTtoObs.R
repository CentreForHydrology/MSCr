#' Creates an obs file from all MSC old-style daily temperature files in a directory 
#' @description Reads all files of old-style MSC daily soil, min and max air temp. data from a specified directory and assembles them. The values are organized in columns, 1 year per file. Writes a obs file called \option{xx_tminmax.obs}, where \option{xx} is the first 2 characters of \option{filespec}.
#' @param directory Optional. Directory containing all temperature files. The default is the current directory.
#' @param filespec Optional. File specification for all files. Default is \option{A1*}.
#' @param timezone Required.  Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#'
#' @return If successful returns \code{TRUE}. If unsuccessful returns \code{FALSE}.
#' @export
#'
#' @examples \dontrun{
#' oldMSCHourlyUtoObs(timezone='Etc/GMT+7')
#' }
oldMSCdailyTtoObs <- function(directory='.', filespec='A1*', timezone=''){
  # reads all daily tmin/tmax and assembles a .obs file
  # assumption is that only days with precipitaion were recorded
  
  setwd(directory)
  FilePattern <- utils::glob2rx(filespec)
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
    raw <- raw[, c('date', 'tmin', 'tmax')]
    
    # merge
    all <- merge(all, raw, by='date', all.x=TRUE)
    all$date <- format(all$date, format='%Y-%m-%d')
    all$datetime <- paste(all$date, ' 01:00', sep='')
    
    # create datetimes
    all$datetime <- as.POSIXct(all$datetime, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all <- all[, c('datetime', 'tmin', 'tmax')]
    
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
  result <- CRHMr::writeObsFile(obs, obs.name, 'obs')
  return(result)
}
