#' Read NOAA file of precipitation data
#'
#' @param NOAAfile Required. File of NOAA precipitation data. Contains PRECIP and/or SNOWFALL variables. 
#' May contain data for any number of locations. 
#' @param outLoc Optional. Location for output files. Each variable for each location will be written to a separate \pkg{CRHMr} obs file.
#' @param timezone Required. The name of the timezone of the data as a character string. This should be the timezone of your data, 
#' but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.

#'
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' @export
#'
#' @examples \dontrun{result <- readNOAAprecip(NOAAfile = "4553527558696dat.txt", timezone = "MST")
#' }
readNOAAprecip <- function(NOAAfile = "", outLoc = "./", timezone = "", quiet = TRUE, logfile = "") {
  # check parameters
  if (NOAAfile == '') {
    cat('Error: missing NOAA file name\n')
    return(FALSE)
  }
  
  if (outLoc == '') {
    cat('Error: missing output location\n')
    return(FALSE)
  }

  
  # open file and parse header
  con <- file(NOAAfile, "r", blocking = FALSE, encoding = "ISO_8859-2")
  header <- readLines(con, n = 2)
  close(con)
  
  header1 <- stringr::str_split(header[1], pattern = " ")
  header2 <- stringr::str_split(header[2], pattern = " ")

 
  
  values <- read.csv(NOAAfile, header = FALSE, skip = 2, 
                     stringsAsFactors = FALSE)
  
  # assemble names of columns
  header1 <- unlist(header1)[-1]
  header2 <- unlist(header2)
  header1 <- header1[header1 != ""]
  header2 <- header2[header2 != ""]
  
  num_cols <- ncol(values)
  val_types <- unique(header1)
  num_vals <- length(val_types)
  
  # select columns
  IDcols <- c(1,2,4,5)
  
  datacols <- c(0:(num_vals - 1)) * 4 * 4 + 1
  all_cols <- c(IDcols, (datacols + 9))
  data <- values[, all_cols]
  
  
  # break up
  site_nums <- unique(data[,2])
  
  num_sites <- length(site_nums)
  
  for (i in 1:num_sites) {
    
    site_num <- site_nums[i]
    selected <- data[data[,2] == site_num,]
    site_name <- stringr::str_trim(selected[1,1], "both")
    site_num <- stringr::str_trim(selected[1,2], "both")
    
    selected <- selected[,-(1:2)]
    names(selected) <- c("date", "time", val_types)
    
    # add datetime
    hour <- selected$time / 100
    datetime <- paste(selected$date, " ", hour, sep = "")
    datetime <- as.POSIXct(datetime, format = "%Y%m%d %H", tz = timezone)
    
    # set missing variables to be NA_real
    for (val in (1:num_vals)) {
      val_col <- 2 + val
      selected[selected[, val_col] > 99, val_col] <- NA_real_
  
      # create output data frame

      variable <- val_types[val]
      output <- data.frame(datetime, selected[, val_col] )
      names(output) <- c("datetime", "p.1")
      
      # distribute precip
      distributed <- CRHMr::distributeP(output, p.cols = 1,
                                        timestep = 1)
      
      outfile <- paste(outLoc, site_name, "_",
                       site_num, "_", variable, ".obs", sep = "")
      result <- CRHMr::writeObsFile(obs = distributed, obsfile = outfile,
                                    comment = "NOAA hourly")
    }
  }
  
  if (result) {
    comment <- paste('readNOAAprecip NOAAfile:', NOAAfile, sep = '')
    result <- CRHMr::logAction(comment, logfile)
    
  }
  return(result)

}