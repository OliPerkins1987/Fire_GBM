
######################################################################

### The following code provides a suite of functions to weight a set of data
### using raking

### Author: Oli Perkins, Feb-March 2021


######################################################################



########################################################################################

### 1) Make sampling thresholds

########################################################################################

### What are the quartiles of the target distribution to define the weights?

Make.sampling_thresholds <- function(vars = list('HDI' = NULL, 
                              'Pop_dense' = NULL, 'ETo' = NULL)) {
  

  ### !!! function arguments
  ### vars should be a named list containing the target variables as a 1-column data frame or numeric vector
  
  
  vars.sum      <- lapply(vars, summary)
  
  Var.threshold <- data.frame(row.names = c('Min', 'LQ', 'Median', 'Mean', 'UQ', 'Max'))
  
  for(i in 1:length(vars.sum)) {
    
    Var.threshold[names(vars.sum)[i]] <- as.numeric(vars.sum[[i]])[1:6]
    
  }
  
  return(Var.threshold[c(2, 3, 4), ]) ## return only quartiles
  
}

###########################################################

### 2) Make DAFI thresholds

###########################################################

### Defines weights for the input data based on relationship to quartiles of target distributions

Make.DAFIweights <- function(dat, key = NULL, thresholds, trim.thresholds = NULL) {
  
  ### !!! function arguments
  ### dat - a dataframe of observations combined with their sampled values of the target variables
  ### key - which columns are the target variables in?
  ### thresholds - output of Make.sampling_thresholds
  ### trim.thresholds - either NULL or a length two numeric vector giving lower and upper bounds for the weights
 
  key             <- ifelse(is.null(key), 1:ncol(dat), key)
  
  dat             <- dat[key, ]
  
  ### Assign quartile
  
  for(i in 1:ncol(dat)) {
    
    col               <- dat[, i]
    
    dat[, i] <- ifelse(round(col, 3) <= Var.threshold[1, i], 1, 
                       ifelse(round(col, 3) <= Var.threshold[2, i], 2, 
                              ifelse(round(col, 3) <= Var.threshold[3, i], 3, 4)))
    
  }
  
  ### Assign weight based on over / under sampling
  
  dat <- apply(dat, 2, function(x) {
    
    (length(x) / 4) / table(x)
    
  })
  
  if(any(unlist(apply(dat, 2, length)) != 4)) {
    
    stop('One or more quartiles did not overlap the reference data.')
    
  }
  
  if(!is.null(trim.thresholds)) {
    
    dat <- apply(dat, 2, function(x) {
      
      ifelse(x < trim.thresholds[1], trim.thresholds[1], 
             ifelse(x > trim.thresholds[2], trim.thresholds[2], 
                    x))
      
    })
    
  }
  
  row.names(dat) <- c('LQ', 'LMQ', 'UMQ', 'UQ')
  return(data.frame(dat))
  
}


###########################################################################

### 4) Prepare weighted sample

###########################################################################


### Creates a random sample of the original data based on the weights created by Make.DAFI_weights

Prepare.weighted_sample    <- function(dat, thresholds, DAFI.weights, trim.thresholds = c(0.3, 3),
                                       var.names = c('HDI', 'Pop_dense', 'ET')) {
  
  ### !!! function arguments
  ### dat -  the original data frame from Make.DAFI_weights - NB column 1 should be (unique ID)
  ### thresholds - output of Make.sampling_thresholds
  ### DAFI.weights - output of Make.DAFI_weights
  ### trim.thresholds - either NULL or a numeric vector giving lower and upper bounds for trimming of weights
  ### var.names - character vector giving the names of the variables to be extracted from dat
  
  ### assign quartile of global distribution
  
  for(i in 2:ncol(dat)) {
    
    col               <- dat[, i]
    
    dat[, i]          <- ifelse(round(col, 3) <= thresholds[1, i-1], 1, 
                                ifelse(round(col, 3) <= thresholds[2, i-1], 2, 
                                       ifelse(round(col, 3) <= thresholds[3, i-1], 3, 4)))
    
  }
  
  
  ### extract weights
  
  for(j in 1:nrow(dat)) {
    
    for(k in 1:length(var.names)) {
      
      dat[[var.names[k]]][j] <- DAFI.weights[[var.names[k]]][dat[[var.names[k]]][j]]
      
    }
    
  }
  
  ### Multiply and trim
  
  dat$weight <- apply(dat[, 2:ncol(dat)], 1, function(x) {prod(x, na.rm = T)})
  
  if(!is.null(trim.thresholds)) {
    
    dat$weight <- ifelse(dat$weight < trim.thresholds[1], trim.thresholds[1], 
                         ifelse(dat$weight > trim.thresholds[2], trim.thresholds[2], 
                                dat$weight))
    
  }
  
  
  return(dat)
  
  
}





