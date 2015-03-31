vp2rh <-
function(airtemp, vapour.pressure){
  # converts air temp (C) and vapour pressure (kPa) to rh (percent)
  if (is.na(airtemp) | is.na(vapour.pressure))
    return(NA_real_)
  else{
  if (airtemp <= 0)
    estar <- 0.611 * exp((21.88 * airtemp) / ( airtemp + 265.5))
  else
    estar <- 0.611 * exp((17.27 * airtemp) / ( airtemp + 237.3))
  
  rh <- min(max((100 * (vapour.pressure/estar)),0),100)
  return(rh)
  }
}
