makeRegular <-
function(CRHMdata, timezone){
  # converts time series of Excel times to have proper times
  # rounds time to nearest hour
  
  datetime <- CRHMdata[,1]
  
  # get CRHM time components
  year <- as.numeric(format(datetime, format='%Y'))
  month <- as.numeric(format(datetime, format='%m'))
  day <- as.numeric(format(datetime, format='%d'))
  hour <- as.numeric(format(datetime, format='%H'))  
  minute <- as.numeric(format(datetime, format='%M'))
  
  # get hour
  hour <- hour + minute/60
  
  #round off
  hour <- round(hour, digits=0)
  
  # reassemble dataframe
  datetime.string <- paste(year,'-',month,'-', day,' ',hour,':00',sep='')
  datetime.corrected <- as.POSIXct(datetime.string, format='%Y-%m-%d %H:%M ',tz=timezone)
  
  CRHMdata$datetime <- datetime.corrected
  return(CRHMdata)
}
