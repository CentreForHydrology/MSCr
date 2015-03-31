logAction <-
function(comment='', LogFileName=''){
  # writes comments to a log file
  date.time <- date()
  date.time.formatted <- format(Sys.time(), format='%Y-%m-%d_%H:%M:%S')
  
  if(LogFileName == '')
    LogFileName <- 'CRHMr.log'
  
  if (comment == ''){
    cat('Missing comments \n')
    return(FALSE)
  }
  
  cat(date.time.formatted, comment, win.eol(), sep=' ', file=LogFileName, append=TRUE)   
  return(TRUE)
}
