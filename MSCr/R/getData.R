getData <-
function(stations, folder, verbose) {
  ## form URLS
  urls <- lapply(seq_len(NROW(stations)),
                 function(i, stations) {
                   genURLS(stations$StationID[i],
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
    pb <- utils::txtProgressBar(min = 0, max = nfiles, style = 3)
    on.exit(close(pb))
  }
  
  out <- vector(mode = "list", length = nfiles)
  
  for (i in seq_len(nfiles)) {
    curfile <- fnames[i]
    
    ## Have we downloaded the file before?
    if (!file.exists(curfile)) {    # No: download it
      # check OS to find method of downloading
      if (stringr::str_detect(.Platform$OS.type, stringr::fixed('win',ignore_case=TRUE)))
        dload <- try(utils::download.file(URLS[i], destfile = curfile, quiet = TRUE, method = 'wininet', 
                                          extra='--content-disposition'))
      else
        dload <- try(utils::download.file(URLS[i], destfile = curfile, quiet = TRUE, method = 'wget', 
                                          extra='--content-disposition'))
      
     
      
      
      if (inherits(dload, "try-error")) { # If problem, store failed URL...
        out[[i]] <- URLS[i]
        if (isTRUE(verbose)) {
          utils::setTxtProgressBar(pb, value = i) # update progress bar...
        }    
        next                             # bail out of current iteration
      }
    }
    
    ## Must have downloaded, try to read file
    ## skip first 16 rows of header stuff
    ## encoding must be latin1 or will fail
    cdata <- try(utils::read.csv(curfile, skip = 16, encoding = "latin1", stringsAsFactor=FALSE))
    
    ## Did we have a problem reading the data?
    if (inherits(cdata, "try-error")) { # yes hand read problem
      file.remove(curfile)   # remove file if a problem
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
