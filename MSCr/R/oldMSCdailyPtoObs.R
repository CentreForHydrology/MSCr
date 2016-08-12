#' Creates an obs file from all MSC old-style precipitation files in a directory 
#' @description Reads all files of old-style MSC daily precipitation data from a specified directory and assembles them. It is assumed is that only days with precipitaion were recorded. Writes a obs file called \option{ppt.obs}.
#' @param directory Optional. Directory containing all precipitation files. File names must begin with \option{A0O}. The default is the current directory.
#' @param timezone Required.  Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#'
#' @return If successful returns \code{TRUE}. If unsuccessful returns \code{FALSE}.
#' @export
#'
#' @examples \dontrun{
#' oldMSCdailyPtoObs(timezone='Etc/GMT+7')
#' }

oldMSCdailyPtoObs <- function(directory='.', timezone=''){
  setwd(directory)
  filespec <- 'A0O*'
  FilePattern <- utils::glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
    
    # read data
    raw <- utils::read.table(file=infile, header=FALSE, skip=7, stringsAsFactors=FALSE)
    
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
    raw <- raw[, c('date', 'ppt')]
    
    # merge
    all <- merge(all, raw, by='date', all.x=TRUE)
    all$date <- format(all$date, format='%Y-%m-%d')
    all$datetime <- paste(all$date, ' 01:00', sep='')
    
    # create datetimes
    all$datetime <- as.POSIXct(all$datetime, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all <- all[, c('datetime', 'ppt')]
    
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
  result <- CRHMr::writeObsFile(obs, obs.name, 'obs')
  return(result)
}