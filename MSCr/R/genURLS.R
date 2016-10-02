genURLS <- function(id, start, end, timeframe = c("hourly", "daily", "monthly")) {
  years <- seq(start, end, by = 1)
  nyears <- length(years)
  timeframe <- match.arg(timeframe)
  if (isTRUE(all.equal(timeframe, "hourly"))) {
    years <-  rep(years, each = 12)
    months <- rep(1:12, times = nyears)
    ids <- rep(id, nyears * 12)
  } else if (isTRUE(all.equal(timeframe, "daily"))) {
    months <- 1                      # this is essentially arbitrary & ignored if daily
    ids <- rep(id, nyears)
  } else {
    years <- start                   # again arbitrary, for monthly it just gives you all data
    months <- 1                      # and this is also ignored
    ids <- id
  }
  timeframe <- match(timeframe, c("hourly", "daily", "monthly"))
  URLS <- paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?stationID=", id,
                 "&Year=", years,
                 "&Month=", months,
                 "&Day=14",
                 "&format=csv",
                 "&timeframe=", timeframe,
                 "&submit=%20Download+Data"## need this stoopid thing as of 11-May-2016
  )
  list(urls = URLS, ids = ids, years = years, months = rep(months, length.out = length(URLS)))
}