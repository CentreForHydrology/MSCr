#' Reads AHCCD daily temperature values.
#'
#' @description This program reads Adjusted and Homogenized Canadian Climate Data (AHCCD) data in a month x day data file of min max or mean air temperatures.
#' @param infile Required. Name of the file to be read.
#' @return If successful, returns the values in a dataframe, consisting of the date, the value and the data code. If unsuccessful, returns the value FALSE.
#' @author Kevin Shook
#' @note The AHCCD are often used for statistical analysis, so this function returns a data quality code, and does not create a CRHM obs files.
#' @references Monthly AHCCD data are available from \url{http://www.ec.gc.ca/dccha-ahccd}. Daily values must be requested. Any use of the temperature data must cite \cite{Vincent, L. A., X. L. Wang, E. J. Milewska, H. Wan, F. Yang, and V. Swail , 2012. A second generation of homogenized Canadian monthly surface air temperature for climate trend analysis, J. Geophys. Res., 117, D18110, doi:10.1029/2012JD017859.}
#' @examples
#' \dontrun{
#'stoon.tmax <- readAHCCDdailytemps('dx40657120.txt')}
#' @export

readAHCCDdailytemps <- 
  function(infile){
    if (infile==''){
      cat('Error: infile missing\n')
      return(FALSE)
    }
  
    # strip off file name from path and extension to figure out data type
    base <- basename(infile) # remove path
    split <- str_split_fixed(base, fixed('.'), 2)
    filename <- split[1]
    # check for precip type
    if (str_detect(tolower(filename), 'dx')){
      temp.type <- 'tmax'
    }
    else if (str_detect(tolower(filename), 'dm')){
      temp.type <- 'tmean'
    }
    else if (str_detect(tolower(filename), 'dn')){
      temp.type <- 'tmin'
    }
    else{
      cat('Error: wrong file type\n')
      return(FALSE)
    }
    
    
  # set up homes for data
  value <- c(0)
  code <- c(0)
  date <- c(0)
  
  # set up constants
  monthdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  monthdays.leapyear<- c(31,29,31,30,31,30,31,31,30,31,30,31)
  twodigitnums <- c('01','02','03','04','05','06','07','08','09','10','11','12',
                    '13','14','15','16','17','18','19','20','21','22','23','24',
                    '25','26','27','28','29','30','31')
  
  
  # figure out header info
  # read 1st 10 lines
  con <- file(infile, "r", blocking = FALSE, encoding="ISO_8859-2")
  input <- readLines(con, n=10)
  close(con)
  
  # find header lines
  # first find number of lines containing file info
  input <- tolower(input)
  LineNum <- str_detect(input, fixed(','))
  fileHeaderLines <- sum(LineNum)
  
  # now find number of lines containing column titles
  englishLineNum <- str_detect(input, fixed("year"))
  englishLineCount <- sum(englishLineNum)
  frenchLineNum <- str_detect(input, fixed("annee"))
  frenchLineCount <- sum(frenchLineNum)
  columnHeaderLines <- sum(englishLineCount) + sum(frenchLineCount)
  
  totalSkipLines <- fileHeaderLines + columnHeaderLines
  
  # check for with of first field - is there a leading space
  firstChar <- substr(input[totalSkipLines + 1], 1, 1)
  
  if (firstChar == ' ')
    yearWidth <- 5
  else
    yearWidth <- 4
  
  # set up widths to read
  header <- c(yearWidth,3,1)
  header.classes <- c('numeric','numeric','character')
  
  cols <- rep.int(c(8,1),31)
  cols.classes <- rep.int(c('numeric', 'character'), 31)
  all <- c(header,cols)
  all.classes <- c(header.classes, cols.classes)
  
  
  # set up widths to read
  header <- c(5,3,1)
  header.classes <- c('numeric','numeric','character')
  
  cols <- rep.int(c(7,1),31)
  cols.classes <- rep.int(c('numeric', 'character'), 31)
  all <- c(header,cols)
  all.classes <- c(header.classes, cols.classes)
  
  # read data
  raw <- read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes, skip=totalSkipLines)
  row.count <- nrow(raw)
  # now unstack data
  data.row <- 0
  data.cols  <- seq(4,64,2)
  code.cols <- data.cols + 1
  
  year.num <- as.numeric(raw[,1])
  month.num <- as.numeric(raw[,2])
  
  data.values <- raw[,data.cols]
  data.codes <- raw[,code.cols]
  data.values[data.values<=-999] <- NA_real_
  
  month.str <- twodigitnums[month.num]
  
  
  # now unstack data
  data.row <- 0
  
  # transpose data
  data.values.t <- t(data.values)
  data.codes.t <- t(data.codes)
  
  # now stack data frames to vectors
  data.values.vector <- as.vector(data.values.t, mode='numeric')
  data.codes.vector <- as.character(as.vector(data.codes.t, mode='character'))
  
  # replicate months
  all.days <- rep.int(twodigitnums, row.count)
  
  # replicate days
  all.months <- rep(month.str, each=31)
  
  # replicate years
  all.years <- rep(year.num, each=31)
  
  # create dates
  datestrings <- paste(all.years,'-', all.months,'-', all.days, sep='')
  date <- as.Date(datestrings, format='%Y-%m-%d')
  
  # find bad date values
  bad.date.loc <- is.na(date)
  good.date.loc <- !bad.date.loc
  
  # assemble data sets
  all.data <- data.frame(date, data.values.vector, data.codes.vector)
  
  # get good dates only
  good.data <- all.data[good.date.loc,]
  names(good.data) <- c('date', temp.type, 'code')
  return(good.data)
}
