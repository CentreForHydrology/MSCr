CRHM_summary <-
function(CRHMdataset){
  # summarises CRHM dataset
  names <- c(0)
  values <- c(0)
  if (nrow(CRHMdataset) == 0)
  {
    cat('Missing dataset\n')
    return(FALSE)
  }
  else{
    numrows <- nrow(CRHMdataset)
    clean <- na.omit(CRHMdataset)
    clean.numrows <- nrow(clean)
    variables <- names(CRHMdataset)[-1]
    variable.count <- length(variables)
    names[1] <- 'Time step (hours):'
    values[1] <- timestep.hours(CRHMdataset[1,1], CRHMdataset[2,1])
    names[2] <- 'Total rows:'
    values[2] <- nrow(CRHMdataset)
    names[3] <- 'Complete rows:'
    values[3] <- nrow(clean)
    names[4] <- 'From:'
    values[4] <- format(CRHMdataset[1,1], format='%Y-%m-%d')
    names[5] <- 'To:'
    values[5] <- format(CRHMdataset[numrows,1], format='%Y-%m-%d')
    names[6] <- 'First complete date:'
    values[6] <- format(clean[1,1], format='%Y-%m-%d')
    names[7] <- 'Last complete date:'
    values[7] <- format(clean[clean.numrows,1], format='%Y-%m-%d')
    names[8] <- 'Number of variables:'
    values[8] <- variable.count
    names[9] <- 'Variable names:'
    values[9] <- str_c(variables, collapse=' ')
    complete.summary <- data.frame(names, values)
    names(complete.summary) <- c('summary','value')
    return(complete.summary)
  }
}
