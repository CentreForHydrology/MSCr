#' Reads MSC radar precipitation.
#' @description  Reads in file containing MSC radar precipitation data and adds the missing (zero) values. The data consist of a header, followed by a single line containing the word \option{DATA}  and all of the values, which are comma-delimited.
#' @param radarFile Required. File containing precipitation data
#' @param outputFormat Optional. Either \option{grid} (the default) or \option{XYZ}. 
#'
#' @return If successful, returns radar data in specified format, which consists of a list containing the header information and either grided (matrix) or XYZ (dataframe) values. The precipitation vaues are in mm/hr. If unsuccessful, returns \code{FALSE}.
#' @export
#'
#' @examples
#' \dontrun{
#' readRadarPrecip('junk.num')}
readRadarPrecip <- function(radarFile='', outputFormat='grid'){
  # check parameters
  if (radarFile==''){
    cat('Error: file of radar precip missing\n')
    return(FALSE)
  }
  
  outputFormat <- str_to_lower(outputFormat)
  if(str_detect(outputFormat, 'x'))
    outputFormat <- 'xyz'
  else
    outputFormat <- 'grid'
  
  # read in file of Radar precip
  con <- file(radarFile, "r", blocking = FALSE)
  input <- readLines(con, encoding = "latin1")
  close(con)

  # get header info
  headerLines <- input[1:19]
  header <- str_split_fixed(headerLines, ' ', 2)
  header <- as.data.frame(header, stringsAsFactors=FALSE)
  names(header) <- c('Parameter', 'Value')

  # get data
  dataLine <- input[20]
  dataSplit <- str_split_fixed(dataLine, ' ',2)
  precipData <- unlist(str_split(dataSplit[2], ','))
  precipDataWidth <- length(precipData)

  # now reshape vector to array
  latLocs <- seq(1, precipDataWidth, 3)
  lats <- as.numeric(precipData[latLocs])

  lonLocs <- seq(2, precipDataWidth, 3)
  lons <- as.numeric(precipData[lonLocs])

  precipLocs <- seq(3, precipDataWidth, 3)
  precips <- as.numeric(precipData[precipLocs])
  
  # combine into a dataframe
  allData <- data.frame(lons, lats, precips)
  names(allData) <- c('lon', 'lat', 'precip')

  # force data into a grid
  width <- as.numeric(header[7, 2])
  height <- as.numeric(header[8, 2])
  
  maxDataLon <- max(allData$lon)
  maxDataLat <- max(allData$lat)
  minDataLon <- min(allData$lon)
  minDataLat <- min(allData$lat)
  
  latInc <- (maxDataLat - minDataLat) / (height - 1)
  lonInc <- (maxDataLon - minDataLon) / (width - 1)
  allData$x <- round((allData$lon - minDataLon) / lonInc) + 1  
  allData$y <- round((allData$lat - minDataLat) / latInc) + 1
  xyData <- subset(allData, select=c(x, y, precip))
  
  # add derived values to header
  header <- rbind(header, c('Min lat', minDataLat))
  header <- rbind(header, c('Max lat', maxDataLat))
  header <- rbind(header, c('Min lon', minDataLon))
  header <- rbind(header, c('Max lon', maxDataLon))
  header <- rbind(header, c('Lat inc', latInc))
  header <- rbind(header, c('Lon inc', lonInc ))
  
  # fill up array, adding zero values
  precipGrid <- matrix(data=0, nrow=height, ncol=width)
  for (i in 1:nrow(xyData)){
    precipRow <- xyData$y[i]
    precipCol <- xyData$x[i]
    precipGrid[precipCol, precipRow] <- xyData$precip[i]
  }

  if (outputFormat == 'grid'){
    precip <- precipGrid
    output <- list(precip, header)
    names(output) <- c('precip', 'header')
    return(output)
  }
  
  else{
    # melt grid to dataframe
    # get lats and lons
    minCol <- 1
    maxCol <- ncol(precipGrid)
    minRow <- 1
    maxRow <- nrow(precipGrid)
    cols <- seq(minCol, maxCol)
    lons <- (cols - 1) * lonInc + minDataLon
    rows <- seq(maxRow, minRow, -1)
    lats <- (rows - 1) * latInc + minDataLat
    dimnames(precipGrid) <- list(lats, lons)
    
    precip <- c(0)
    lat <- c(0)
    lon <- c(0)
    rowNum <- 1
    for (precipRow in minRow:maxRow)
      for(precipCol in minCol:maxCol){
        lat[rowNum] <- (precipRow - 1) * latInc + minDataLat
        lon[rowNum] <- (precipCol - 1) * lonInc + minDataLon
        precip[rowNum] <- precipGrid[precipCol, precipRow]
        rowNum <- rowNum + 1
      }
    precipMelted <- data.frame(lat, lon, precip)
    names(precipMelted) <- c('lat', 'lon', 'precip')

    precip <- precipMelted
    output <- list(precip, header)
    names(output) <- c('precip', 'header')
    return(output)
  }
}
