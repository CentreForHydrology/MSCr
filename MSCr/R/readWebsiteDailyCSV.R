#' Reads file of daily values produced by MSC website
#'
#' @param websiteCSV Required. A .csv file of daily values as produced by \url{http://climate.weather.gc.ca/historical_data/search_historic_data_e.html}.
#' @param metaData Optional. If \code{TRUE} (the default) the meta data are returned.
#'
#' @return If \code{metadata = TRUE}, a list of data frames with the names \code{header}
#'  (station info), \code{legend} (meaning of the flags) and \code{values}
#'  daily values are returned. If \code{metadata = FALSE}, then only
#'  a data frame of the values is returned. In both cases, the values are a time
#'  series - the first column is a standard {R} \code{Date}.
#
#' @export
#' @author Kevin Shook
#' @examples \dontrun{vals <- readWebsiteDailyCSV("eng-daily-01012016-12312016.csv", FALSE)}
readWebsiteDailyCSV <- function(websiteCSV, metaData = TRUE) {
  
  # check for input file
  if ( is.null(websiteCSV) | websiteCSV == "") {
    cat("Error: missing file name\n")
    return(FALSE)
  }
  
  # read in entire file
  con <- file(websiteCSV)
  header_lines <- readLines(con)
  close(con)
  
  # now parse file to find header
  legend_lines <- stringr::str_locate(header_lines, "Legend")
  legend_line <- min(which(!is.na(legend_lines[, 1])))
  
  last_header_line <- legend_line - 2
  
  header <- read.table(websiteCSV, header = FALSE, sep = ",",
                       nrows = last_header_line)
  names(header) <- c("ID", "Value")
  
  # now find beginning of data
  date_lines <- stringr::str_locate(header_lines, "Date")
  date_line <- max(which(!is.na(date_lines[, 1])))
  
  legend_skip <- legend_line
  legend_num_lines <- (date_line - 2) - legend_skip
  
  # get the legend and column header
  legend <- read.table(websiteCSV, header = FALSE, sep = ",", 
                       skip = legend_skip,
                       nrows = legend_num_lines)
  names(legend) <- c("Value", "Meaning")
  
  col_names <-  read.table(websiteCSV, header = FALSE, sep = ",", 
                           skip = (date_line - 1),
                           nrows = 1)
  col_names <- t(as.vector(col_names))

  # read values
  values <- read.table(websiteCSV, header = FALSE, sep = ",", 
                       skip = (date_line), 
                       stringsAsFactors = FALSE)
  
  names(values) <- col_names
  date_loc <- stringr::str_locate(col_names, "Date")
  date_loc <- max(which(!is.na(date_loc[, 1])))
  
  if (date_loc == 1) {
    names(values)[1] <- "Date"
    drops <- c("Year", "Month", "Day")
    values$Date <- as.Date(values$Date, format = "%Y-%m-%d")
    values <-  values[ , !(names(values) %in% drops)]
  }
  

  if (metaData) {
    ret_data <- list(header = header, legend = legend, values = values)
    return(ret_data)
  } else {
    return(values)
  }
  
  
}