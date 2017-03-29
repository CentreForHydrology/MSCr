#' Title
#'
#' @param ACISfiles Optional. A file path and specification for the ACIS files. Note that because this function creates \code{.csv} files, you should never specify \option{*.csv}, or the function may attempt to read in the output. If not specified, the default file specification is \option{ACIS*.csv} in the current directory.
#' @param outDir Optional. 
#' @param timezone 
#' @param quiet 
#'
#' @return
#' @export
#'
#' @examples
multipleACIStoObs <- function(ACISfiles='', outDir='', timezone='etc/GMT+7', quiet=TRUE){
  # check parameters
  result <- c(0)
  eol.val <- CRHMr::win.eol()
  ACISdir <- dirname(ACISfiles)
  
  if (ACISdir==''){
    ACISdir <- './'
  }
  
  if(outDir==''){
    outDir <- ACISdir
  }
  
  filespec <- basename(ACISfiles)
  if(filespec == '')
    filespec <- 'ACIS*.csv'
  
  # set working directory to be as specified
  originalDir <- getwd()
  setwd(ACISdir)
  
  # get all files
  FilePattern <- glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  numFiles <- length(FileList)
  
  if(numFiles == 0){
    cat('Error: no files matching specified location and/or name\n')
    return(FALSE)
  }
  
  if(!quiet){
    cat('Reading files from: ',ACISdir, ' matching: ', filespec,'\n',sep='')
    cat('Writing to: ', outDir, '\n', sep='')
  }
  
  setwd(outDir)
  for (j in 1:numFiles){ 
    cat(FileList[j],'\n', sep='')
    ACISfile <- paste(ACISdir,'/', FileList[j], sep='')
    
    header <- read.table(file=ACISfile, nrows=1, encoding = 'UTF8', sep=',', stringsAsFactors = FALSE)
    ACIS <- read.csv(file=ACISfile, skip=1, stringsAsFactors = FALSE, header = FALSE, fill=TRUE)
    
    # get station names
    stationNames <- unique(ACIS[,1])
    
    # convert datetimes to standard
    # check to see if the data are daily or hourly
    ACISbasename <- basename(ACISfile)
    dailyVals <- stringr::str_detect(stringr::str_to_lower(ACISbasename), 'daily')
    if (dailyVals){
      datetime <- as.Date(ACIS[,2], format='%d-%B-%Y')
    } else{
      datetime <- as.POSIXct(ACIS[,2], format='%d-%B-%Y %H:%M', tz=timezone)
    }
    
    
    # figure out variable names
    if (dailyVals){
      varcount <- floor((ncol(ACIS) -2) / 3)
    } else {
      varcount <- floor((ncol(ACIS) - 1) / 3)
    }
    
    
    varCols <- seq(from=1, to=varcount) * 3
    # varCols <- varCols + 2
    varNames <- header[varCols]
    
    # remove non-ascii chars, periods and spaces
    varNames <- stringr::str_replace_all(varNames,'\xb0','')
    varNames <- stringr::str_replace_all(varNames,stringr::fixed('.'),'')
    varNames <- stringr::str_trim(stringr::str_replace_all(varNames,' ', '_'))
    
    varValues <- ACIS[,varCols]
    obs <- data.frame(datetime, varValues)
    if (dailyVals)
      names(obs) <- c('date', varNames)
    else
      names(obs) <- c('datetime', varNames)
    
    # get source flags and comments
    sourceCols <- varCols +1
    commentCols <- sourceCols +1
    ord1 <- 2*(1:length(sourceCols))-1
    ord2 <- 2*(1:length(commentCols))
    allCommentCols <- c(sourceCols,commentCols)[order(c(ord1,ord2))]
    comments <- cbind(obs[,1], ACIS[,allCommentCols])
    commentNames <- header[allCommentCols]
    commentNames <- stringr::str_replace_all(commentNames,'\xb0','')
    
    names(comments) <- c(names(obs)[1], stringr::str_trim(stringr::str_replace_all(commentNames,' ', '_')))
    
    # subdivide by station and output
    stationCount <- length(stationNames)
    
    for (i in 1:stationCount){
      stationLocs <- which(ACIS[,1] == stationNames[i])
      stationObs <- obs[stationLocs,]
      stationComments <- comments[stationLocs,]
      # change space to underscore in station names
      stationNameClean <- stringr::str_trim(stringr::str_replace(stationNames[i],' ', '_'))
      if (j == 1){
        # write output file name
        obsFile <- paste(outDir,'/', stationNameClean, '.obs', sep='')
        result[i] <- CRHMr::writeObsFile(stationObs, obsFile, comment='ACIS data', quiet=quiet)
        
        # write variable information
        commentFile <- paste(outDir,'/', stationNameClean, '_comments.csv', sep='')
        utils::write.csv(stationComments, file=commentFile, row.names=FALSE)
      } else{
        # append to existing files
        # output to temporary file
        obsFile <- paste(outDir,'/', stationNameClean, '.obs', sep='')
        tempObsFile <- paste(outDir,'/', stationNameClean, '_tempfile.obs', sep='')
        tempResult <- CRHMr::writeObsFile(stationObs, tempObsFile, comment='ACIS data', quiet=quiet)
        
        # read back in!
        # find header
        con <- file(tempObsFile, "r", blocking = FALSE)
        input <- readLines(con, encoding = "latin1")
        close(con)
        
        #delete temp file
        file.remove(tempObsFile)
        # find header
        header.linenum <- grep("##", input, fixed=TRUE)
        rest <- input[-(1:header.linenum)]
        
        # append to existing obs file
        con <- file(obsFile, "at", blocking = FALSE)
        writeLines(rest, con, sep=eol.val)
        close(con)
        
        # write variable information
        commentFile <- paste(outDir,'/', stationNameClean, '_comments.csv', sep='')
        utils::write.table(stationComments, file=commentFile, 
                           col.names = FALSE, row.names=FALSE, append=TRUE, sep=',')

      }

    }
  }
}