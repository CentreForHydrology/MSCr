genURLS <-
function(id, start, end) {
  years <- seq(start, end, by = 1)
  nyears <- length(years)
  years <- rep(years, each = 12)
  months <- rep(1:12, times = nyears)
 # http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=1706&Year=${year}&Month=${month}&Day=14&timeframe=1&submit= Download+Data"
  URLS <- paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?",
                 "format=csv&stationID=", id,
                 "&Year=", years,
                 "&Month=", months,
                 "&Day=14",
                 "&timeframe=1&submit=%20Download+Data")
  list(urls = URLS, ids = rep(id, nyears * 12), years = years, months = months)
}
