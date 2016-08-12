#' Creates an obs file from all MSC old-style wind files in a directory 
#' @description Reads all files of old-style MSC hourly wind speed data from a specified directory and assembles them. The values are organized day x hour, 1 year per file. Writes a obs file called \option{u.obs}.
#' @param directory Optional. Directory containing all wind files. File names must begin with \option{WIND}. The default is the current directory.
#' @param timezone Required.  Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#'
#' @return If successful returns \code{TRUE}. If unsuccessful returns \code{FALSE}.
#' @export
#'
#' @examples \dontrun{
#' oldMSCHourlyUtoObs(timezone='Etc/GMT+7')
#' }
oldMSCHourlyUtoObs <- function(directory='.', timezone=''){

  # reads all hourly wind data and assembles a .obs file
  setwd(directory)
  filespec <- 'WIND*'
  FilePattern <- utils::glob2rx(filespec)
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
    raw <- utils::read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes) 
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
  result <- CRHMr::writeObsFile(obs, obs.name, 'obs')
  return(result)
}
