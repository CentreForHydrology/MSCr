#' Reads current set of MSC stations from server
#'
#' @param stationURL Optional. URL of file containing stations. If not specified then the file at "client_climate@ftp.tor.ec.gc.ca" will be used.
#' @param outfile Optional. If specified, then the stations will be written to a comma delimited file.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}. Note that setting \code{quiet=FALSE} shows the downloading progress bar.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used.
#'
#' @return If unsuccessful, returns \code{FALSE}. If successful, returns a \pkg{CRHMr} data frame containing the station names and their metadata.
#' @author Kevin Shook
#' @note The station data frame contains the variables: Name, Province, Climate ID, Station ID, WMO ID, TC ID, Latitude (Decimal Degrees), Longitude (Decimal Degrees), Latitude, Longitude, Elevation (m), First Year, Last Year, HLY First Year, HLY Last Year, DLY First Year, DLY Last Year, MLY First Year, MLY Last Year.
#' @seealso \code{\link{downloadMSCobs}}
#' @export
#'
#' @examples \dontrun{
#' MSCstations <- downloadMSCstations(quiet=FALSE)}
downloadMSCstations <- function(stationURL='', outfile='', quiet=TRUE, logfile=''){
  
  # check parameters
  if (stationURL == '')
    stationURL <- 'ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv'
  
  if(outfile == '')
    outfile <- 'temp.crp'
  
  dload <- utils::download.file(stationURL, destfile = outfile, quiet = quiet)
  if(dload == 0){
    stations <- read.csv(file=outfile, header=FALSE, skip=4, stringsAsFactors = FALSE)
    headers <- read.table(file=outfile, header=FALSE, sep=',', skip=3, nrows=1, colClasses = 'character')
    names(stations) <- headers
  }
  else{
    cat('Downloading failed\n')
    return(FALSE)
  }
  # delete .csv file if not specified
  if (outfile == 'temp.crp')
    file.remove(outfile)
  
  # write to logfile
  comment <- paste('downloadMSCstations url:', stationURL, sep='')
  result <- CRHMr::logAction(comment, logfile)
  
  if(result)
    return(stations)
  else
    return(result)
  
}