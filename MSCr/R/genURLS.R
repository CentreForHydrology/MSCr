genURLS <-
function(id, start, end) {
  years <- seq(start, end, by = 1)
  nyears <- length(years)
  years <- rep(years, each = 12)
  months <- rep(1:12, times = nyears)

  URLS <- paste0("http://climate.weather.gc.ca/climateData/bulkdata_e.html?timeframe=1&Prov=SK&StationID=",
                 id,
                 "&hlyRange=1953-01-30%7C2014-12-31&cmdB1=Go&Year=",
                 years,
                 "&Month=",
                 months,
                 "&Day=27",
                 "&format=csv",
                 "&stationID=",
                 id)
  list(urls = URLS, ids = rep(id, nyears * 12), years = years, months = months)
}
