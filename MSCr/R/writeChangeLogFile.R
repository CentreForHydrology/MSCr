writeChangeLogFile <-
function(action, original.data.info, changed.data.info, 
                               changed.data.type, comment1='', comment2='', 
                               comment3='', comment4=''){
  
  # creates log file for a changed dataframe
  # writes up to 4 lines of comments to a log file
  date.time <- date()
  date.time.compressed <- format(Sys.time(), format='%Y%m%d%H%M')
  
  if (nrow(original.data.info) > 0){
    cat('Original data\n')
    print(original.data.info)
  }
  else{
    cat('Missing data set information\n')
    return(FALSE)
  }
  
  if (nrow(changed.data.info) > 0){
    cat('Changed data\n')    
    print(changed.data.info)    
  }
  else{
    cat('Missing data set information\n')
    return(FALSE)
  }
 
  LogFileName  <- paste(action,'_',date.time.compressed, '.log',sep='')  
  cat('Created: ', date.time, win.eol(), sep='', file=LogFileName, append=FALSE)
  if (comment1 !=''){
    cat(comment1, win.eol(), sep='', file=LogFileName, append=TRUE)    
  }
  if (comment2 !=''){
    cat(comment2, win.eol(), sep='', file=LogFileName, append=TRUE)    
  }
  if (comment3 !=''){
    cat(comment3, win.eol(), sep='', file=LogFileName, append=TRUE)    
  }
  if (comment4 !=''){
    cat(comment2, win.eol(), sep='', file=LogFileName, append=TRUE)    
  }
  cat('Original data', win.eol(), sep='', file=LogFileName, append=TRUE)
  write.table(original.data.info, sep='\t', file=LogFileName, append=TRUE, eol=win.eol(), 
              col.names=FALSE, row.names=FALSE, quote=FALSE)
  
  cat('New data', win.eol(), sep='', file=LogFileName, append=TRUE)
  write.table(changed.data.info, sep='\t', file=LogFileName, append=TRUE, eol=win.eol(), 
              col.names=FALSE, row.names=FALSE, quote=FALSE) 
  
  cat('New data types', win.eol(), sep='', file=LogFileName, append=TRUE)
  
  # write col names out manually, to prevent a warning
  col.names <- names(changed.data.type)
  cat(col.names, win.eol(), sep='\t', file=LogFileName, append=TRUE)
  write.table(changed.data.type, sep='\t', file=LogFileName, append=TRUE, eol=win.eol(), 
              col.names=FALSE, row.names=FALSE, quote=FALSE) 
  
  return(TRUE)

  
}
