readHourlyWind <-
function(directory, timezone){
  library(CRHMr)
  
  # reads all hourly wind data and assembles a .obs file
  setwd(directory)
  filespec <- 'WIND*'
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
    # set up widths to read
    header <- c(2,2,2)
    header.classes <- c('numeric','numeric','numeric')
    
    cols <- rep.int(c(2,2),24)
    cols.classes <- rep.int(c('character', 'numeric'), 24)
    all <- c(header,cols,5)
    all.classes <- c(header.classes, cols.classes, 'character')
    
    # read data
    raw <- read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes) 
    row.count <- nrow(raw)
    dir.cols <- seq(4,50,2)
    data.cols  <- dir.cols + 1
    
    years <- raw[,1] + 1900
    months <- raw[,2]
    days <- raw[,3]
    
    # now unstack time series
    data.values <-  raw[,data.cols]
    dir.codes <-  raw[,dir.cols]
    
    # transpose data
    data.values.t <- data.frame(t(data.values))
    
    # now stack data frames to vectors
    data.values.vector <- stack(data.values.t)
    
    # replicate days, months, years
    hours <- seq(1:24)
    all.hours <- rep(hours, row.count)
    all.days <- rep(days, each=24)      
    all.months <- rep(months, each=24)
    all.years <- rep(years, each=24)
    
    # create dates
    datestrings <- paste(all.years,'-', all.months,'-', all.days,' ', all.hours,':00', sep='')
    datetime <- as.POSIXct(datestrings, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all.data <- data.frame(datetime, data.values.vector[,1])
    names(all.data) <- c('datetime', 'u')
    
    # replace missing values
    all.data[(all.data[,2] <= -1), 2] <- NA_real_
    
    # stack time series
    if (i == 1)
      obs <- all.data
    else
      obs <- rbind(obs, all.data)
  }
  # sort by data
  obs <- obs[order(obs$datetime),]
  obs.name <- 'u.obs'
  
  # convert from km/h to m/s
  obs$u <- obs$u / 3.6
  result <- writeObsFile(obs, obs.name, 'obs')
}
