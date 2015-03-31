rh2vp <-
function(airtemp, rh){
  # converts air temp (C) and RH (percent) to vapour pressure (kPa)
  if (is.na(airtemp) | is.na(rh))
    return(NA_real_)
  else{
    if (airtemp <= 0)
      estar <- 0.611 * exp((21.88 * airtemp) / ( airtemp + 265.5))
    else
      estar <- 0.611 * exp((17.27 * airtemp) / ( airtemp + 237.3))
    vp <- max(estar * rh / 100, 0)
    return(vp)
  }
}
