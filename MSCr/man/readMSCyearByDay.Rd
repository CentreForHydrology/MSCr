% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readMSCyearByDay.R
\name{readMSCyearByDay}
\alias{readMSCyearByDay}
\title{Reads MSC precipitation arranged year by day}
\usage{
readMSCyearByDay(MSCfile = "", station = "")
}
\arguments{
\item{MSCfile}{Required. String containing file name.}

\item{station}{Optional. A string containing. the ID of a single station. The default is an empty string.}
}
\value{
If successful, returns a data frame consisting of the \code{station ID}, \code{latitude}, \code{longitude}, 
\code{year}, \code{date}, and \code{precip}. If the \code{station} is specified, then only the values for that station
will be returned. Otherwise all values are returned. If unsuccessful, returns \code{FALSE}.
}
\description{
Reads MSC precipitation arranged year by day
}
\examples{
\dontrun{vals <- readMSCyearByDay("Prnational1950.txt")}
}
\author{
Kevin Shook
}
