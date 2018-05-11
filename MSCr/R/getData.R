getData <- function(stations, folder, timeframe = c("hourly", "daily", "monthly"), verbose = TRUE, delete = TRUE) {
  timeframe <- match.arg(timeframe)
  ## form URLS
  urls <- lapply(seq_len(NROW(stations)),
                 function(i, stations, timeframe) {
                   genURLS(stations$StationID[i],
                           stations$start[i],
                           stations$end[i], timeframe = timeframe)
                 }, stations = stations, timeframe = timeframe)
  
  ## check the folder exists and try to create it if not
  if (!file.exists(folder)) {
    warning(paste("Directory:", folder,
                  "doesn't exist. Will create it"))
    fc <- try(dir.create(folder))
    if (inherits(fc, "try-error")) {
      stop("Failed to create directory '", folder,
           "'. Check path and permissions.", sep = "")
    }
  }
  
  ## Extract the data from the URLs generation
  URLS <- unlist(lapply(urls, '[[', "urls"))
  sites <- unlist(lapply(urls, '[[', "ids"))
  years <- unlist(lapply(urls, '[[', "years"))
  months <- unlist(lapply(urls, '[[', "months"))
  
  ## filenames to use to save the data
  fnames <- paste(sites, years, months, "data.csv", sep = "-")
  fnames <- file.path(folder, fnames)
  
  nfiles <- length(fnames)
  
  ## set up a progress bar if being verbose
  if (isTRUE(verbose)) {
    pb <- txtProgressBar(min = 0, max = nfiles, style = 3)
    on.exit(close(pb))
  }
  
  out <- vector(mode = "list", length = nfiles)
  hourlyNames <- c("Date/Time", "Year", "Month","Day", "Time", 
                   "Temp (degC)", "Temp Flag", "Dew Point Temp (degC)",
                   "Dew Point Temp Flag", "Rel Hum (%)", "Rel Hum Flag",
                   "Wind Dir (10s deg)", "Wind Dir Flag", "Wind Spd (km/h)",
                   "Wind Spd Flag", "Visibility (km)", "Visibility Flag",
                   "Stn Press (kPa)", "Stn Press Flag", "Hmdx", "Hmdx Flag",
                   "Wind Chill", "Wind Chill Flag", "Weather")
  dailyNames <- c("Date/Time", "Year", "Month", "Day", "Data Quality", "Max Temp (degC)", "Max Temp Flag",
                  "Min Temp (degC)", "Min Temp Flag", "Mean Temp (degC)", "Mean Temp Flag",
                  "Heat Deg Days (degC)", "Heat Deg Days Flag", "Cool Deg Days (degC)", "Cool Deg Days Flag",
                  "Total Rain (mm)", "Total Rain Flag", "Total Snow (cm)", "Total Snow Flag",
                  "Total Precip (mm)", "Total Precip Flag", "Snow on Grnd (cm)", "Snow on Grnd Flag",
                  "Dir of Max Gust (10s deg)", "Dir of Max Gust Flag", "Spd of Max Gust (10s deg)", "Spd of Max Gust Flag")
  monthlyNames <- c("Date/Time", "Year", "Month",
                    "Mean Max Temp (degC)", "Mean Max Temp Flag",
                    "Mean Min Temp (degC)", "Mean Min Temp Flag",
                    "Mean Temp (degC)", "Mean Temp Flag",
                    "Extr Max Temp (degC)", "Extr Max Temp Flag",
                    "Extr Min Temp (degC)", "Extr Min Temp Flag",
                    "Total Rain (mm)", "Total Rain Flag",
                    "Total Snow (cm)", "Total Snow Flag",
                    "Total Precip (mm)", "Total Precip Flag",
                    "Snow Grnd Last Day (cm)", "Snow Grnd Last Day Flag",
                    "Dir of Max Gust (10s deg)", "Dir of Max Gust Flag",
                    "Spd of Max Gust (10s deg)", "Spd of Max Gust Flag")
  
  cnames <- switch(timeframe, hourly = hourlyNames, daily = dailyNames, monthly = monthlyNames)
  TIMEFRAME <- match(timeframe, c("hourly", "daily", "monthly"))
  SKIP <- c(15, 24, 17)[TIMEFRAME]
  
  for (i in seq_len(nfiles)) {
    curfile <- fnames[i]
    
    ## Have we downloaded the file before?
    if (!file.exists(curfile)) {    # No: download it
      dload <- try(download.file(URLS[i], destfile = curfile, quiet = TRUE))
      if (inherits(dload, "try-error")) { # If problem, store failed URL...
        out[[i]] <- URLS[i]
        if (isTRUE(verbose)) {
          setTxtProgressBar(pb, value = i) # update progress bar...
        }
        next                             # bail out of current iteration
      }
    }
    
    ## Must have downloaded, try to read file
    ## skip first SKIP rows of header stuff
    ## encoding must be latin1 or will fail - may still be problems with character set
    cdata <- try(read.csv(curfile, skip = SKIP, encoding = "latin1", stringsAsFactors = FALSE), silent = TRUE)
    
    ## Did we have a problem reading the data?
    if (inherits(cdata, "try-error")) { # yes handle read problem
      ## try to fix the problem with dodgy characters
      cdata <- readLines(curfile) # read all lines in file
      cdata <- iconv(cdata, from = "latin1", to = "UTF-8")
      writeLines(cdata, curfile)          # write the data back to the file
      ## try to read the file again, if still an error, bail out
      cdata <- try(read.csv(curfile, skip = SKIP, encoding = "UTF-8", stringsAsFactors = FALSE), silent = TRUE)
      if (inherits(cdata, "try-error")) { # yes, still!, handle read problem
        if (delete) {
          file.remove(curfile) # remove file if a problem & deleting
        }
        out[[i]] <- URLS[i]    # record failed URL...
        if (isTRUE(verbose)) {
          setTxtProgressBar(pb, value = i) # update progress bar...
        }
        next                  # bail out of current iteration
      }
    }
    
    ## Must have (eventually) read file OK, add station data
    cdata <- cbind.data.frame(StationID = rep(sites[i], NROW(cdata)),
                              cdata)
    names(cdata)[-1] <- cnames
    out[[i]] <- cdata
    
    if (isTRUE(verbose)) { # Update the progress bar
      setTxtProgressBar(pb, value = i)
    }
  }
  
  out                                 # return
}