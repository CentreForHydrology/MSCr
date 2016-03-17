#' Reads hourly precipitation data
#'
#' @description Reads hourly precipitation values from a file in the MSC Datamart format (day x hour).
#' @param infile Required. File to read precipitation from.
#' @param timezone Optional. Timezone of the orginal data. If not specified, your timezone will be used.
#'
#' @return Returns a data frame consisting of the variables 'datetime', 'precip' and 'code'. The precipitation is in mm. The code is the data quality code from the original dataset. Missing values coded as -9999 in the original file are set to \code{NA_real_}.
#' @export
#'
#' @examples \dontrun{
#' precip <- readDatamartHourlyPrecip('L1012710.262')}
readDatamartHourlyPrecip <- function(infile, timezone=''){
  hours <- seq(0, 23)
  
  # set up homes for data
  datetime <- c(0)
  
  # check parameters
  
  if (infile==''){
    cat('Error: infile missing\n')
    return(FALSE)
  }
  
  # set up widths to read
  # line header=station year month day code
  header <- c(7,4,2,2,3)
  header.classes <- c('character','numeric','numeric','numeric','character')
  
  cols <- rep.int(c(6,1),24)
  cols.classes <- rep.int(c('numeric', 'character'), 24)
  all <- c(header,cols)
  all.classes <- c(header.classes, cols.classes)
  
  # read data
  raw <- read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes, skip=0)
  row.count <- nrow(raw)
  
  # now unstack data
  data.cols  <- seq(6,52,2)
  code.cols <- data.cols + 1
  
  year.num <- as.numeric(raw[,2])
  month.num <- as.numeric(raw[,3])
  day.num <- as.numeric(raw[,4])
  
  data.values <- raw[,data.cols]
  data.codes <- raw[,code.cols]
  data.values[data.values<=-999] <- NA_real_
  
  # transpose data
  data.values.t <- t(data.values)
  data.codes.t <- t(data.codes)
  
  # now stack data frames to vectors
  data.values.vector <- as.vector(data.values.t, mode='numeric')
  data.codes.vector <- as.character(as.vector(data.codes.t, mode='character'))
  
  # replicate days, months and years
  all.days <- rep(day.num, each=24)
  all.months <- rep(month.num, each=24)
  all.years <- rep(year.num, each=24)
  
  # create times
  all.hours <- rep(hours, times=row.count)
  all.times <- paste(all.hours, ':00:00', sep='')
  
  # create dates
  datestrings <- paste(all.years,'-', all.months,'-', all.days, ' ', all.times, sep='')
  datetime <- as.POSIXct(datestrings, format='%Y-%m-%d %H:%M:%S', tz=timezone)
  
  # assemble data sets
  all.data <- data.frame(datetime, data.values.vector / 10, data.codes.vector)
  names(all.data) <- c('datetime', 'precip', 'code')
  
  return(all.data)

}