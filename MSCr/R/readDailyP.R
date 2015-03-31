readDailyP <-
function(directory, timezone=''){
  library(CRHMr)
  
  # reads all daily precipdata and assembles a .obs file
  # assumption is that only days with precipitaion were recorded
  
  setwd(directory)
  filespec <- 'A0O*'
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)
  
  for (i in 1:NumFiles){ 
    infile <- FileList[i]
    cat(infile, '\n')
     
    # read data
    raw <- read.table(file=infile, header=FALSE, skip=7, stringsAsFactors=FALSE)
    
    # collect req'd data
    raw <- raw[,c(1, 5, 6, 9)]
    names(raw) <- c( 'year', 'month', 'day', 'ppt')
    raw$year <- as.numeric(raw$year)
    raw$month <- as.numeric(raw$month)
    raw$day <- as.numeric(raw$day)
    raw$ppt <- as.numeric(raw$ppt)
    
    # check for the last date being Jan 1 of next year - set to Dec 31
    last.row <- nrow(raw)
    if((raw$month[last.row] == 1) & (raw$day[last.row] == 1) & 
         (raw$year[last.row] == raw$year[last.row-1])){  
      raw$year[last.row] == raw$year[last.row] + 1
      last.date <- as.Date(paste((raw$year[last.row]+1900), '-01-01', sep=''), format='%Y-%m-%d')       
    }
    else
      last.date <- as.Date(paste((raw$year[last.row]+1900), '-12-31', sep=''), format='%Y-%m-%d') 
    
    raw$date <- as.Date(paste((raw$year+1900), '-', raw$month, '-', raw$day, sep=''),
                        format='%Y-%m-%d')
    
    # create annual date/time series
    first.date <- as.Date(paste((raw$year[2]+1900), '-01-01', sep=''), format='%Y-%m-%d')
   
    all <- data.frame(seq(from=first.date, to=last.date, by=1))
    names(all) <- 'date'

    raw <- subset(raw, select=c(date, ppt))
    # merge
    all <- merge(all, raw, by='date', all.x=TRUE)
    all$date <- format(all$date, format='%Y-%m-%d')
    all$datetime <- paste(all$date, ' 01:00', sep='')
    
    # create datetimes
    all$datetime <- as.POSIXct(all$datetime, format='%Y-%m-%d %H:%M', tz=timezone)
    
    # assemble data sets
    all <- subset(all, select=c(datetime, ppt))

    # replace missing values
    all[is.na(all[,2]), 2] <- 0
    
    # stack time series
    if (i == 1)
      obs <- all
    else
      obs <- rbind(obs, all)
  }
  
  # sort by data
  obs <- obs[order(obs$datetime),]
  obs.name <- 'ppt.obs'
  result <- writeObsFile(obs, obs.name, 'obs')
}
