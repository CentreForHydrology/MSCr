#' @title Functions to read Meteorological Service of Canada (MSC) data
#' @docType package
#' @name MSCr-package
#' 
#' @description
#' This package contains functions to do the following:
#' \enumerate{
#'   \item Read data from the MSC website and write CRHM files of hourly and daily observations.
#'   \item Read data files of Adjusted and Homogenized Canadian Climate Data (AHCCD) monthly and daily data.
#'   \item Read in file of MSC radar data and write a grid or xyz file
#'   \item Read in large MSC data files containing daily or hourly values for many locations and write a CRHM obs file for each location.
#'   \item Read in old format files of MSC or AES data for variables for a single site and write a CRHM obs file.
#' }
#' 
#' @references 
#' To cite \pkg{MSCr} in publications, use the command \code{citation('MSCr')} to get the current version of the citation.\cr
#' The CRHM program is described in:\cr
#'\cite{Pomeroy, John W, D M Gray, T Brown, N Hedstrom, W L Quinton, R J Granger, and S K Carey. 2007. \dQuote{The Cold Regions Hydrological Model : A Platform for Basing Process Representation and Model Structure on Physical Evidence}. Hydrological Processes 21 (19): 2650-2567.}\cr
#'The CRHM model may be downloaded from \url{http://www.usask.ca/hydrology/CRHM.php}.\cr
#' @import stringr reshape2
#' @importFrom CRHMr writeObsFile logAction win.eol distributeP
#' @importFrom lubridate days_in_month
#' @importFrom utils read.fwf stack txtProgressBar download.file setTxtProgressBar read.csv
#' setTxtProgressBar glob2rx read.table write.csv
#' @importFrom stats aggregate na.omit

NULL