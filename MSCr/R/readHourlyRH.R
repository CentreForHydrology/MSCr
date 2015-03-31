readHourlyRH <-
function(directory, timezone){
  library(CRHMr)
  
  # reads all hourly RH data and assembles a .obs file
  setwd(directory)
  filespec <- 'RH*'
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
    # set up widths to read
    header <- c(2,2,2)
    header.classes <- c('numeric','numeric','numeric')
    
    cols <- rep.int(5, 24)
    cols.classes <- rep.int(c('numeric'), 24)
    all <- c(header, cols)
    all.classes <- c(header.classes, cols.classes)
    
    # read data
    raw <- read.fwf(file=infile, widths=all, header=FALSE, colClasses=all.classes) 
    row.count <- nrow(raw)
    data.cols <- seq(4,27)
    
    years <- raw[,3] + 1900
    months <- raw[,2]
    days <- raw[,1]
    
    # now unstack time series
    data.values <-  raw[,data.cols]
    
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
    names(all.data) <- c('datetime', 'rh')
    
    # replace missing values
    all.data[(all.data[,2] > 100), 2] <- NA_real_
    
    # stack time series
    if (i == 1)
      obs <- all.data
    else
      obs <- rbind(obs, all.data)
  }
  # sort by data
  obs <- obs[order(obs$datetime),]
  obs.name <- 'rh.obs'
  result <- writeObsFile(obs, obs.name, 'obs')
}
