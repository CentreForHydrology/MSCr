#' Converts single ACIS file to obs files
#' @description Reads in a single file of met data downloaded from the Alberta Climate Information Service (ACIS), \url{https://agriculture.alberta.ca/acis/}. Each station's values are written to a separate \code{.obs} file, named after the station. The comments for all of the variables are written to a separate \code{.csv} file for each station.
#' @param ACISfile Required. The file containing the ACIS data. Note that the file name should be the default produced by the ACID download system, and daily file names must contain the word 'Daily'. 
#' @param outDir Optional. The location for the \code{.obs} files. If not specified, then the directory containing the ACIS file will be used.
#' @param timezone Optional. The name of the timezone of the data as a character string. This should be the timezone of the data, i.e. Mountain Standard Time. Note that the timezone code is specific to your OS. Under Linux, you can use \option{MST} for Mountain Standard time. Under Windows or OSX, you can use \option{etc/GMT+7}, which is the default.
#' @param quiet Optional. Suppresses display of messages, except for errors. If you are calling this function in an \R script, you will usually leave \code{quiet=TRUE} (i.e. the default). If you are working interactively, you will probably want to set \code{quiet=FALSE}.
#' 
#' @return If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}.
#' 
#' @author Kevin Shook
#' @export
#'
#' @examples \dontrun{
#' result <- singleACIStoObs('ACISHourlyData-20170209-20170223-PID122537324.csv')}
singleACIStoObs <- function(ACISfile='', outDir='', timezone='etc/GMT+7', quiet=TRUE){
  result <- c(0)
   if (ACISfile==''){
    cat('Error: ACIS file missing\n')
    return(FALSE)
  }
  
  if(outDir==''){
    outDir <- dirname(ACISfile)
  }
  
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
    
    # write output file name
    obsFile <- paste(outDir,'/', stationNameClean, '.obs', sep='')
    result[i] <- CRHMr::writeObsFile(stationObs, obsFile, comment='ACIS data', quiet=quiet)
    
    # write variable information
    commentFile <- paste(outDir,'/', stationNameClean, '_comments.csv', sep='')
    utils::write.csv(stationComments, file=commentFile, row.names=FALSE)
  }
  return(result)
}