getdailyData <-
function(stations, folder, verbose) {
  ## form URLS
  urls <- lapply(seq_len(NROW(stations)),
                 function(i, stations) {
                   genDailyURLS(stations$StationID[i],
                           stations$start[i],
                           stations$end[i])
                 }, stations = stations)
  
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
    
 
    # code added by Kevin Shook to deal with headers having varying numbers of lines
    if ( win.eol() == '\n')  # i.e. ruuning under Windows
      con <- file(curfile, "r", blocking = FALSE)
    else
      con <- file(curfile, "r", blocking = FALSE, encoding="ISO_8859-2")
    input <- readLines(con)
    close(con)
    # find header
    LineNum <- grep("Date/Time", input, fixed=TRUE)
    if ( win.eol() == '\n')  # i.e. ruuning under Windows
      cdata <- try(read.csv(curfile, skip = (LineNum-1), stringsAsFactor=FALSE))
    else  
      cdata <- try(read.csv(curfile, skip = (LineNum-1), fileEncoding="ISO_8859-2", stringsAsFactor=FALSE)) 
    
    ## Did we have a problem reading the data?
    if (inherits(cdata, "try-error")) { # yes hand read problem
      file.remove(cur.file)   # remove file if a problem
      out[[i]] <- URLS[i]     # record failed URL...
      if (isTRUE(verbose)) {
        setTxtProgressBar(pb, value = i) # update progress bar...
      }    
      next                        # bail out of current iteration
    } else {                        # read file OK, add station data
      out[[i]] <- cbind.data.frame(StationID = rep(sites[i], NROW(cdata)),
                                   cdata)
    }
    
    if (isTRUE(verbose)) { # Update the progress bar
      setTxtProgressBar(pb, value = i)
    }    
  }
  
  out                                 # return
}
