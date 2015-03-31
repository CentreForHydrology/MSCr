writeLogFile <-
function(LogFileName, data.info, comment1='', comment2='', 
                         comment3='', comment4=''){
  # check for existing file
  
  # writes up to 4 lines of comments to a log file
  date.time <- date()
  date.time.formatted <- format(Sys.time(), format='%Y-%m-%d %H:%M')
  
  if (nrow(data.info) > 0)
    print(data.info)
  else{
    cat('Missing data set information\n')
    return(FALSE)
  }
    
  if (LogFileName != ''){
    LogFileName  <- paste(LogFileName,'_',date.time.compressed, '.log',sep='')  
    cat('Created: ', date.time, win.eol(), sep='', file=LogFileName, append=FALSE)
    if (comment1 !=''){
      cat(comment1, comment2, comment3, comment4, win.eol, sep='', file=LogFileName, append=TRUE)    
    }
    
  
    
    
    write.table(data.info[,2], sep='\t', file=LogFileName, append=TRUE, eol=win.eol, 
                col.names=FALSE, row.names=FALSE, quote=FALSE)
  return(TRUE)
  }
  else{
    cat('Missing log filename\n')
    return(FALSE)
  }
  
}
