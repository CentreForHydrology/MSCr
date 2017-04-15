# MSCr
This R package contains functions for accessing data from the Meteorological Service of Canada (MSC). Most of the functions read in data files, as the MSC data is generally formatted to be very difficult for R. The function downloadMSCobs() downloads data directly from the MSC web server. Note that the behavior of the server can change at any time, and may break this function.

## Installation
You can download the complete package, as well as the manual .pdf by clicking on **releases**. However, you can download and install the most up-to-date version directly from this repository. The procedure is
1. Install the package "devtools" - you only have to do this once. Note that this will also install several dependancies
2. Load the devtools library
3. Install the package.

The commands are:

	install.packages("devtools")
	library(devtools)
	install_github("CentreForHydrology/MSCr/MSCr")

