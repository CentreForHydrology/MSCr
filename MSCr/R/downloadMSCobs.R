#' Downloads monthly and daily MSC data and creates CRHM .obs files.
#'
#' @description Downloads MSC hourly and daily data, one month at a time. The downloaded data are stored as .csv files, which are erased after the function terminates. Information about the .obs files is displayed on the screen and is also written to log files in the working directory. The obs files are: 1) hourly t, rh, and u  2) daily tmin, tmax, tmean and 3) daily precipitation. Because Environment Canada's web data is not consistent in any way, it is possble that using this function will generate warning messages.
#' @param station.name Name of the station as a text string. This is used to create a directory that will hold the downloaded files and the created .obs files. It is also the basis for the names of the .obs files: <station.name>Hourly.obs, <station.name>DailyTemps.obs, and <station.name>DailyPrecips.obs.
#' @param station.number Required, 
#' @param startyear Optional. First year for downloading. Default is 1900.
#' @param endyear Optional Last year for downloading. Default is 2000.
#' @param timezone Optional. The name of the timezone of the data as a character string. This should be YOUR timezone. The default value is "", which is your timezone. You may find a difference in the seconds between using "" and the your timezone. Note that the timezone code is specific to your OS. Under Windows, the code for Central Standard Time is 'America/Regina'. Under Linux, it is 'CST'.
#' @param quiet Optional. Suppresses display of messages, except for errors. Note that the default is FALSE. If you are calling this function in an R script, you will usually leave quiet=TRUE (i.e. the default). If you are working interactively, you will probably want to set quiet=FALSE.
#' @param logfile Optional. Name of the file to be used for logging the action. Normally not used. Default is 'MSCr.log'
#' @return If successful, returns TRUE. If unsuccessful, returns the value FALSE.
#' @references The code for downloading MSC data is taken from \url{http://www.fromthebottomoftheheap.net/2015/01/14/harvesting-canadian-climate-data}. Some modifications were made to remove bad characters, and to download daily values.
#' @author Kevin Shook
#' @examples
# This will download data for the Vegreville station for the period 1995-1996
# It creates a folder \\Vegreville in the working directory which will hold
# the files VegrevilleHourly.obs, VegrevilleDailyTemps.obs, and VegrevilleDailyPrecips.obs
# It will also write to the file 'CRHMr.log' in the current directory describing the data sources.
#'downloadMSCobs('Vegreville', 1977, 1995, 1996)
#' @export

downloadMSCobs <-
function(station.name='', station.number='', startyear=1900, endyear=2000, timezone='', quiet=TRUE, logfile='MSCr.log'){
  # creates hourly and daily obs files by dowloading data from MSC server
  
  # check parameters
  if (station.name == ''){
    cat('Error: missing station name\n')
    return(FALSE)
  }
  if (station.number == ''){
    cat('Error: missing station number\n')
    return(FALSE)
  }
  
  # build dataframe to hold station data
  stations <- data.frame(StationID = c(station.number),
                         start = rep(startyear,1),
                         end = rep(endyear,1))
  folder <- paste('./',station.name,sep='')
  
  
  comment <- paste('downloadMSCobs ',
                   ' MSC station:', station.name,'',
                   ' MSC_web_site_number:', station.number,
                   ' from:',startyear,
                   ' to:', endyear,
                   sep='')  
  
  result <- logAction(comment, logfile)
  
  # get hourly values
  hourly <- getData(stations, folder, verbose = !quiet)
  ObsFile <- paste(folder,'/',station.name,'Hourly.obs',sep='')
  
  # check for missing hourly data

  hourly <- do.call("rbind", hourly)
  hourly.t.length <- length(na.omit(as.numeric(hourly$Temp..C)))
  hourly.rh.length <- length(na.omit(as.numeric(hourly$Rel.Hum....)))
  hourly.u.length <- length(na.omit(as.numeric(hourly$Wind.Spd..km.h.)))
  
  if((hourly.t.length==0) & (hourly.rh.length == 0) & (hourly.u.length == 0)){
    if (!quiet)
      cat('No hourly data available\n')    
  }
  else
   createHourlyObsFile(hourly, ObsFile, station.name, station.number, 
                       timezone, quiet, logfile)
  
 # delete .csv files
  unlink(paste(folder,'/*.csv',sep=''))  

  # get daily values
  daily <- getdailyData(stations, folder, verbose = !quiet)
  daily <- do.call("rbind", daily)
  ObsFile <- paste(folder,'/',station.name,'DailyTemps.obs', sep='')
  createDailyTempObsFile(daily, ObsFile, station.name, station.number, quiet, logfile)

  ObsFile <- paste(folder,'/',station.name,'DailyPrecips.obs', sep='')
  createDailyPrecipObsFile(daily, ObsFile, station.name, station.number, quiet, logfile)
 
  # delete .csv files
  unlink(paste(folder,'/*.csv',sep=''))

}
