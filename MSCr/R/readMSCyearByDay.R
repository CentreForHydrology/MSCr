#' Reads MSC precipitation arranged year by day
#'
#' @param MSCfile Required. String containing file name.
#' @param station Optional. A string containing. the ID of a single station. The default is an empty string.
#'
#' @return If successful, returns a data frame consisting of the \code{station ID}, \code{latitude}, \code{longitude}, 
#' \code{year}, \code{date}, and \code{precip}. If the \code{station} is specified, then only the values for that station
#' will be returned. Otherwise all values are returned. If unsuccessful, returns \code{FALSE}.
#' @export
#' @author Kevin Shook
#'
#' @examples \dontrun{vals <- readMSCyearByDay("Prnational1950.txt")}
readMSCyearByDay <- function(MSCfile="", station="") {
  if (MSCfile == "") {
    cat("Error: infile missing\n")
    return(FALSE)
  }

  # read fixed-width file
  formats <- c("1X", "A7", "1X", "I4", "2X", "F8", "1X", "F9", "F8", "366F8")

  # read data
  raw <- utils::read.fortran(file = MSCfile, format = formats)
  names(raw)[1:5] <- c("station", "year", "latitude", "longitude", "total")
  names(raw)[6:371] <- seq(1:366)
  raw <- raw[, -5]
  # now melt
  melted <- reshape2::melt(raw, id = c(1:4), measure = c(5:370))

  # get date
  melted$date <- paste(melted$year, "-", melted$variable, sep = "")
  melted$date <- as.Date(melted$date, format = "%Y-%j")

  # remove impossible dates
  melted <- melted[!is.na(melted$date), ]
  returned <- data.frame(
    melted$station, melted$latitude, melted$longitude,
    melted$year, melted$date, melted$value
  )
  names(returned) <- c("station", "latitude", "longitude", "year", "date", "precip")
  returned$station <- as.character(returned$station)

  if (station != "") {
    returned <- returned[returned$station == station, ]
    returned <- returned[order(returned$date),]
  } else {
    # sort by station and date
    returned <- returned[order(returned$station, returned$date),]
  }
  
  # set missing values to NA_real_
  returned$precip[returned$precip < -9] <- NA_real_


  return(returned)
}
