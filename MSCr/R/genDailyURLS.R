genDailyURLS <-
function(id, start, end) {
  years <- seq(start, end, by = 1)
  nyears <- length(years)
  years <- rep(years, each = 1)
  months <- rep(1:1, times = nyears)
 # http://climate.weather.gc.ca/climateData/dailydata_e.html?timeframe=2&Prov=AB%20%20&StationID=1839&dlyRange=1946-03-01|2015-01-16&Year=2015&Month=1&Day=16
  URLS <- paste0("http://climate.weather.gc.ca/climateData/bulkdata_e.html?timeframe=2&Prov=SK&StationID=",
                 id,
                 "&dlyRange=1953-01-30%7C2014-12-31&cmdB1=Go&Year=",
                 years,
                 "&Month=1&Day=15",
                 "&format=csv",
                 "&stationID=",
                 id)
  list(urls = URLS, ids = rep(id, nyears * 12), years = years, months = months)
}
