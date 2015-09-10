#' Reads individual AES files of daily tmin and tmax data in a directory and creates obs files.
#' @description Reads very old Atmospheric Environment Service (AES) files of daily minimum and maximum air temperatures. The air temperatures may be interpolated to hourly values by reading in the .obs file using the functions \code{readObsFile} and \code{tMinMaxToHourly} in \pkg{CRHMr}.
#' @param directory Optional. Directory containing AES data files. If not specified, defaults to current directory. Note that this is an R path, which uses the \code{'/'} symbol on ALL operating systems.
#' @param filespec Optional. File specification (including wildcards) of the tmin data. Default is \option{'A2*'}.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}
#' @author Kevin Shook
#' @seealso  \code{\link{readAEShourlyRH}} \code{\link{readAEShourlyWind}} \code{\link{readAEShourlyT}} \code{\link{readAESdailyP}}
#' @export
#'
#' @examples
#' \dontrun{
#' readAESdailyTminTmax('./HistoricalData', timezone='etc/GMT+6')
#' }

readAESdailyTminTmax <- function(directory, filespec, timezone=''){
  
  # reads all daily tmin/tmax and assembles a .obs file
  # assumption is that only days with precipitaion were recorded
  
  # check parameters
  if (timezone == ''){
    cat('Error: must specify a timezone\n')
    return(FALSE)
  }
  current.dir <- getwd()
  if (file.dir != '')
    setwd(directory)
  else
    cat('Error: must specify a directory\n')

  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  
  if (NumFiles == ''){
    cat('Error: no files found\n')
    return(FALSE)
  }
  
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
  
  # return to current directory
  setwd(current.dir)
  return(result)
}
