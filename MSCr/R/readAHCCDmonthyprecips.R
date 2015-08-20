#' Reads AHCCD monthly precipitation values.
#'
#' @description This program reads Adjusted and Homogenized Canadian Climate Data (AHCCD) data in a month x day data file of rainfall, snowfall or total precipitation.
#' @param infile Required. Name of the file to be read.
#' @return If successful, returns the values in a dataframe, consisting of the date, the value and the data code. If unsuccessful, returns the value FALSE.
#' @author Kevin Shook
#' @note The AHCCD are often used for statistical analysis, so this function returns a data quality code, and does not create a CRHM obs file.
#' @references 
#'Monthly AHCCD data are available from \url{http://www.ec.gc.ca/dccha-ahccd}. Any use of the precipitation data must cite \cite{Mekis, E and L.A. Vincent, 2011: An overview of the second generation adjusted daily precipitation dataset for trend analysis in Canada. Atmosphere-Ocean, 49 (2), 163-177.}
#' @examples
#' \dontrun{
#'stoon.monthly.total <- readAHCCDdailytemps('mt4057120.txt')}
#' @export

readAHCCDmonthlyprecips <- function(infile){
  # set up homes for data
  value <- c(0)
  code <- c(0)
  date <- c(0)
  
  # set up constants
  twodigitnums <- c('01','02','03','04','05','06','07','08','09','10','11','12')
  
  if (infile==''){
    cat('Error: infile missing\n')
    return(FALSE)
  }
  
  # strip off file name from path and extension to figure out data type
  base <- basename(infile) # remove path
  split <- str_split_fixed(base, fixed('.'), 2)
  filename <- split[1]
  # check for precip type
  if (str_detect(filename, ignore.case('mt'))){
    precip.type <- 'total'
  }
  else if (str_detect(filename, ignore.case('mr'))){
    precip.type <- 'rain'
  }
  else if (str_detect(filename, ignore.case('ms'))){
    precip.type <- 'snow'
  }
  else{
    cat('Error: wrong file type\n')
    return(FALSE)
  }
  
  # read data
  raw <- read.csv(file=infile, header=FALSE, skip=4)
  row.count <- nrow(raw)
  # now unstack data
  data.row <- 0
  data.cols  <- seq(2,24,2)
  code.cols <- data.cols + 1
  
  year.num <- as.numeric(raw[,1])
  
  data.values <- raw[,data.cols]
  data.codes <- raw[,code.cols]
  data.values[data.values<=-999] <- NA_real_
  
  # now unstack data
  data.row <- 0
  
  # transpose data
  data.values.t <- t(data.values)
  data.codes.t <- t(data.codes)
  
  # now stack data frames to vectors
  data.values.vector <- as.vector(data.values.t, mode='numeric')
  data.codes.vector <- as.character(as.vector(data.codes.t, mode='character'))
  
  # replicate months
  all.months <- rep.int(twodigitnums, row.count)
  # replicate years
  all.years <- rep(year.num, each=12)
  
  # find bad date values
  bad.date.loc <- is.na(date)
  good.date.loc <- !bad.date.loc
  
  # assemble data sets
  all.data <- data.frame(all.years, all.months, data.values.vector, data.codes.vector)
  
  # get good dates only
  good.data <- all.data[good.date.loc,]
  names(good.data) <- c('year','month', precip.type, 'code')
  return(good.data)
}