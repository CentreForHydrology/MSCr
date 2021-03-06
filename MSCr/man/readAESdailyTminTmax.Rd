% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readAESdailyTminTmax.R
\name{readAESdailyTminTmax}
\alias{readAESdailyTminTmax}
\title{Reads individual AES files of daily tmin and tmax data in a directory and creates obs files.}
\usage{
readAESdailyTminTmax(directory, filespec = "A2*", timezone = "")
}
\arguments{
\item{directory}{Optional. Directory containing AES data files. If not specified, defaults to current directory. Note that this is an R path, which uses the \code{'/'} symbol on ALL operating systems.}

\item{filespec}{Optional. File specification (including wildcards) of the tmin data. Default is \option{'A2*'}.}

\item{timezone}{Required. The name of the timezone of the data as a character string. This should be the timezone of your data, but omitting daylight savings time. Note that the timezone code is specific to your OS. To avoid problems, you should use a timezone without daylight savings time. Under Linux, you can use \option{CST} and \option{MST} for Central Standard or Mountain Standard time, respectively. Under Windows or OSX, you can use \option{etc/GMT+6} or \option{etc/GMT+7} for Central Standard and Mountain Standard time. DO NOT use \option{America/Regina} as the time zone, as it includes historical changes between standard and daylight savings time.}
}
\value{
If successful, returns \code{TRUE}. If unsuccessful, returns \code{FALSE}
}
\description{
Reads very old Atmospheric Environment Service (AES) files of daily minimum and maximum air temperatures. The air temperatures may be interpolated to hourly values by reading in the .obs file using the functions \code{readObsFile} and \code{tMinMaxToHourly} in \pkg{CRHMr}.
}
\examples{
\dontrun{
readAESdailyTminTmax('./HistoricalData', timezone='etc/GMT+6')
}
}
\seealso{
\code{\link{readAEShourlyRH}} \code{\link{readAEShourlyWind}} \code{\link{readAEShourlyT}} \code{\link{readAESdailyP}}
}
\author{
Kevin Shook
}
