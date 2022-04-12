

######################################################################

### The following code provides a suite of functions to drive an AFT distribution
### based on decision tree sub-models

require(tidyverse)
require(rpart)


#####################################################################

### 1) Bootstrap tree objects

#####################################################################

### This function takes an underlying data set, and creates a tree on bootstrapped resamples of it
### It can also optionally prunes the tree to prevent overfitting

boot.tree    <- function(dat, dat.weights = T, weights = NULL, form, var.cols, tree.method = 'class', 
                         k = 10000, min_node, Prune = T, tree.prune = 5) {

  ### !!! Function arguments
  
  ### dat: the underlying data set of dataframe type
  ### dat.weights - Boolean, should weights be included during the bootstrap resampling process?
  ### weights - a vector of weights to be used during bootstrap resampling
  ### form - a character vector that is coerced to a formula; the first item should be the target variable, 
  ###         the remaining items should be predictor variables to be used
  ### var.cols - a vector specifying which columns from the dataframe should be used for modelling
  ### tree.method - 'class' specifies a classification tree, currently only classification trees are supported
  ### k - the number of bootstrap replicates to use
  ### min_node - an integer to specify the smallest number of data points that can be assigned to a terminal leaf
  ### Prune    - a boolean specifying if pruning should be conducted
  ### tree.prune - an integer specifying how many nodes the tree should be pruned down to
  
  ### returns: a list of decision tree models
  
  split.list <- list()
  
  if(dat.weights == TRUE) {
    
    dat$weights <- weights
    
  }
  
  
    for(j in 1:k) {
      
      if(dat.weights == TRUE) {
        
        boot.dat <- dat[sample.int(nrow(dat), replace = T, prob = weights), ]
        
      } else {
        
        boot.dat <- dat[sample.int(nrow(dat), replace = T), ]
        
      }
      

        f <- formula(paste0(form[1], ' ~ ', paste(form[2:length(form)], collapse = ' + ')))
        
        split.list[[j]] <- tree::tree(f, control = tree.control(nrow(boot.dat), mincut = min_node),
                                      data = boot.dat[, var.cols])
        
        if(Prune == TRUE) {
        
          if(tree.method == 'class') {
          
        split.list[[j]] <- prune.misclass(split.list[[j]], best = tree.prune)

          } else if(tree.method == 'regression') {
            
        split.list[[j]] <- prune.tree(split.list[[j]], best = tree.prune)
            
          }
        
        }
        
    }

    return(split.list)
    
    
  }


##########################################################

### 2) aggregate bootstrapped trees

##########################################################

### Building on the intial function, this extracts a list describing the frequency of splits at different nodes
### across all of the bootstrapped trees.

aggregate.boot_tree <- function(splits, nsplits) {
  
  ###!!! Function arguments
  
  ### splits - the output of boot.tree
  ### nsplits- the number of nodes to take into consideration
  
  
  res <- list()
  
  for(i in 1:nsplits) {
    
    ### get correct index for var frame
    
    res[[i]]   <- table(unlist(lapply(splits, function(x) {
      
      node     <- which(row.names(x$frame) == as.character(i))
      #node    <- node[which.min(as.integer(node)[which(as.integer(node) >= i)])]
      
      x$frame$var[as.integer(node)]}
      
    )))
    
    
  }
  
  return(res)
}


#########################################
### select trees matching majority structure
#########################################

select.boot_tree <- function(splits, key) {
  
  ### This extracts trees with a given structure from the bootstrapped list
  
  ### !!! Function arguments
  ### splits - the output of boot.tree
  ### key    - a named list of the form: 
  ###           ('node number' = 'variable name', etc)
  ###          NB: only exact matches will be selected from the bootstrapped list
  
  for(i in 1:length(splits)) {

      if(all.equal(as.character(splits[[i]]$frame$var[which(row.names(splits[[i]]$frame) %in% names(key))]), 
                   as.character(unlist(key))) == TRUE) {
        
        
        if(all.equal(as.character(splits[[i]]$frame$var[which(!row.names(splits[[i]]$frame) %in% names(key))]), 
                     rep('<leaf>', times = length(as.character(splits[[i]]$frame$var[which(!row.names(splits[[i]]$frame) %in% names(key))])))) == TRUE) {
          
          
          NULL
          
        } else {
            
          
          splits[[i]] <- 'Not a match'
          
          }
        
          
      } else {
        
        
          splits[[i]] <- 'Not a match'
        
        }
      

     }

  
  splits <- splits[which(unlist(lapply(splits, function(x) {x[1] != 'Not a match'})))]

  return(splits)
  
  
  }
  



################################################################

### 3) extract bootstrapped values

################################################################

get.split_vals <- function(splits, node_n, var_name) {
  
  ### obtains threshold values used to split the tree from the tree list produced by boot.tree
  
  ### !!! Function arguments
  ### node_n and varname are the key value pairs from the named list used to filter the output of boot.tree
  ### at the preivous step
  
  res <- lapply(splits, function(x) {
    
    try({
      
      if(x$frame$var[row.names(x$frame) == as.character(node_n)] == var_name) {
        
        as.numeric(substr(as.character(x$frame$splits[which(row.names(x$frame) == as.character(node_n)), 1]), 2, 
            nchar(as.character(x$frame$splits[which(row.names(x$frame) == as.character(node_n)), 1]))))
        
      }
      
    })
    
  })
  
}

################################

### Bootstrapped probabilities

################################

get.leaf_probs <- function(split.list, leaf_n) {
  
  ### obtains threshold values used to split the tree from the tree list produced by boot.tree
  
  ### !!! Function arguments
  ### leaf_n - the number of the leaf in the decision tree frame object
  
  res <- lapply(split.list, function(x) {

        x$frame$yprob[which(names(splits)[leaf_n] == row.names(x$frame)), ]

  })
  
}

### for regression trees

get.leaf_vals <- function(split.list, leaf_n) {
  
  ### obtains threshold values used to split the tree from the tree list produced by boot.tree
  
  ### !!! Function arguments
  ### leaf_n - the number of the leaf in the decision tree frame object
  
  res <- lapply(split.list, function(x) {
    
    x$frame$yval[which(names(splits)[leaf_n] == row.names(x$frame))]
    
  })
  
}




####################################################################

### 4) Predict from bootstrapped tree frame

####################################################################

make.final_tree <- function(tree.template, how = c('Median', 'Random', 'Sequential'),
                            split.key, thresholds, probs, sequential.key = NULL, 
                            Regression = F) {

  ### Creates a final decision tree structure from the bootstrapped list
  
  ### !! Function arguments
  ### how - specifies the way in which the final tree is made:
  ###         * 'Median' uses the median value of the distribution of each threshold splits
  ###         * 'Random' takes a random draw for each threshold splits
  ###         * 'Sequential' takes an integer value (sequential.key) to allow all thresholds to be sampled systematically
  ### split.key - the named list used to split the output of boot.tree
  ### thresholds - the output of get.split_vals
  ### probs      - the output of get.leaf_probs
  ### sequential.key - see how = 'Sequential'
  
  
  ft <- tree.template

  if(how == 'Random') {
    
    rand.key           <- sample.int(length(thresholds[[1]]), size = 1)
    
  }
  
  for(i in 1:nrow(ft$frame)) {
  
  ### update node thresholds
  
  if(ft$frame$var[i] != '<leaf>') {
    
    if(how == 'Median') {
    
    ft$frame$splits[i, ] <- c(paste0("<", 
                              median(thresholds[[which(names(split.key) == row.names(ft$frame)[i])]])),
                              paste0(">", median(thresholds[[which(names(split.key) == row.names(ft$frame)[i])]])))
    
    
    
    } else if(how == 'Random') {

      ft$frame$splits[i, ] <- c(paste0("<", 
                                thresholds[[which(names(split.key) == row.names(ft$frame)[i])]][[rand.key]]),
                                paste0(">", thresholds[[which(names(split.key) == row.names(ft$frame)[i])]][[rand.key]]))
      
    } else if(how == 'Sequential') {
      
      ft$frame$splits[i, ] <- c(paste0("<", 
                                       thresholds[[which(names(split.key) == row.names(ft$frame)[i])]][[sequential.key]]),
                                paste0(">", thresholds[[which(names(split.key) == row.names(ft$frame)[i])]][[sequential.key]]))
      
      
    }
    
    ### update leaf probs
    
  } else if(ft$frame$var[i] == '<leaf>') {
    
    probs.val <- which(as.character(names(probs)) == row.names(ft$frame)[i])
    
    
    if(how == 'Median') {
    
      
    ft$frame$yprob[i, ] <- unlist(lapply(probs[[probs.val]], median))
    
    
    } else if(how == 'Random') {
          
    ft$frame$yprob[i, ] <- as.numeric(probs[[probs.val]][rand.key, ])
      
      
      
    } else if(how == 'Sequential') {
          
      if(Regression == T) {
        
      ft$frame$yval[i] <- as.numeric(probs[[probs.val]][sequential.key])
        
      } else {
      
      ft$frame$yprob[i, ] <- as.numeric(probs[[probs.val]][sequential.key, ])
      
      }
      
        }
    
      }
  
   }
  
 return(ft)

}


################################################################################

### 5) Recalculate tree thresholds based on upsampling

################################################################################

Relearn.tree_par <- function(target, variable, threshold.type = 'Lower', weights) {
  
  ### This function takes a fixed decision tree structure and relearns the optimum split threshold for one of its nodes
  ### This is done using the optimise base R function
  ### It is only currently implemented for a categorical outcome and a numeric predictor, 
  ###   though an ordered factor (ordinal variable) may be included as an integer vector
  
  
  ### !!! Function arguments
  ### target - factor giving the variable on which the tree split should be based
  ### variable - numeric giving the potential values on which to split the tree
  ### threshold.type - one of 'Upper' or 'Lower; is the target variable lower / higher 
  ###    than the numeric threshold in the tree structure
  ### weights - should a weights vector be used in calculating the split value?
  
  ### Known issue: if you supply a numeric vector as a predictor in which min = max, or which has no meaningful 
  ### relationship with the target, the optimise algorithm may fail.
  
  
  treesplit <- function(thresh, tar, var, how, wts) {
    
    if(how == 'Upper') {
      
      t1    <- factor(ifelse(var >= thresh, TRUE, FALSE))
      
    } else if (how == 'Lower') {
      
      t1    <- factor(ifelse(var < thresh, TRUE, FALSE))
      
    }
    
    ModelMetrics::gini((as.numeric(tar)-1), as.numeric(t1)-1)
    
  }
  
  pars <- as.numeric(summary(variable))
  
  return(optimise(treesplit, interval = pars[c(1, 6)], 
                  lower = pars[1], upper = pars[6],
                  tar = target, var = variable, how = threshold.type, 
                  wts = weights, maximum = TRUE))
  
}



#######################################################################

### 6) Assess model

#######################################################################

Assess.model <- function(niche_mod = Final.tree, reference_mod = Final.tree2, 
                         dat = Combo.dat, bootstrap_dat = dat.samps, 
                         bootstrap = T, target = 'AFR', 
                         overall_target = 'AFR') {
  
  
  ### Function groups together a range of procedures to evalute models learned 
  ###   on different sets / weights of the same data
  
  ### !!! Function arguments
  ### niche_mod, reference_mod decision trees of class 'tree' to be evaluated
  ### dat, the data on which to evaluate them
  ### bootstrap_dat, a list of integer vectors which describe the bootstrap samples to be taken
  ### bootstrap - boolean describing of bootstrapping should be used to calculate metrics across
  ###   multiple data fols
  ### target - character giving name of target variable
  
  
  results          <- list()
  results$All_data <- list('AUC' = list(), 'Class_Prediction' = list())
  results$Bootstrap <- list('AUC' = list(), 'Kappa' = list(), 'Accuracy' = list())
  
  
  ### do single result
  
  results$All_data$AUC$Niche     <- ModelMetrics::auc(as.numeric(dat[[target]])-1, 
                                     predict(niche_mod, dat, type = 'vector')[, 2])
  
  results$All_data$AUC$Reference <- ModelMetrics::auc(as.numeric(dat[[target]])-1, 
                                     predict(reference_mod, dat, type = 'vector')[, 2])
  
  results$All_data$Class_Prediction$Niche       <- caret::confusionMatrix(predict(niche_mod, dat, type = 'class'), 
                                                    factor(dat[[target]]))$overall[1:2]
  
  results$All_data$Class_Prediction$Reference   <- caret::confusionMatrix(predict(reference_mod, dat, type = 'class'), 
                                                    factor(dat[[target]]))$overall[1:2]

  Combined.preds <- data.frame(predict(niche_mod, dat, type = 'vector')[, 2], 
                    predict(reference_mod, dat, type = 'vector')[, 2])
  
  
  results$All_data$AUC$Combined <- ModelMetrics::auc(as.numeric(dat[[target]])-1, 
                                    apply(Combined.preds, 1, mean))
  
  overall.pred <- apply(Combined.preds, 1, mean) >= 0.5
  results$All_data$Class_Prediction$Combined <- caret::confusionMatrix(factor(overall.pred), factor(dat[[overall_target]]))$overall[1:2]
  
  
  
  ### bootstrapping

  for(i in 1:length(bootstrap_dat)) {
  
  bootstrap_dat[[i]] <- dat[bootstrap_dat[[i]], ]
    
  results$Bootstrap$AUC$Niche[[i]]       <- ModelMetrics::auc(as.numeric(bootstrap_dat[[i]][[target]])-1, 
                                            predict(niche_mod, bootstrap_dat[[i]], type = 'vector')[, 2])
  
  results$Bootstrap$AUC$Reference[[i]]   <- ModelMetrics::auc(as.numeric(bootstrap_dat[[i]][[target]])-1, 
                                            predict(reference_mod, bootstrap_dat[[i]], type = 'vector')[, 2])
  
  results$Bootstrap$Kappa$Niche[[i]]     <- caret::confusionMatrix(predict(niche_mod, bootstrap_dat[[i]], type = 'class'), 
                                            factor(bootstrap_dat[[i]][[target]]))$overall[2]
  
  results$Bootstrap$Kappa$Reference[[i]] <- caret::confusionMatrix(predict(reference_mod, bootstrap_dat[[i]], type = 'class'), 
                                            factor(bootstrap_dat[[i]][[target]]))$overall[2]
  
  results$Bootstrap$Accuracy$Niche[[i]]     <- caret::confusionMatrix(predict(niche_mod, bootstrap_dat[[i]], type = 'class'), 
                                            factor(bootstrap_dat[[i]][[target]]))$overall[1]
  
  results$Bootstrap$Accuracy$Reference[[i]] <- caret::confusionMatrix(predict(reference_mod, bootstrap_dat[[i]], type = 'class'), 
                                            factor(bootstrap_dat[[i]][[target]]))$overall[1]
  
  
  Combined.preds <- data.frame(predict(niche_mod, bootstrap_dat[[i]], type = 'vector')[, 2], 
                      predict(reference_mod, bootstrap_dat[[i]], type = 'vector')[, 2])
  
  
  results$Bootstrap$AUC$Combined[[i]]    <- ModelMetrics::auc(as.numeric(bootstrap_dat[[i]][[overall_target]])-1, 
                                            apply(Combined.preds, 1, mean))
  
  overall.pred                          <- apply(Combined.preds, 1, mean) >= 0.5
  results$Bootstrap$Kappa$Combined[[i]] <- caret::confusionMatrix(factor(overall.pred), factor(bootstrap_dat[[i]][[overall_target]]))$overall[2]
  results$Bootstrap$Accuracy$Combined[[i]] <- caret::confusionMatrix(factor(overall.pred), factor(bootstrap_dat[[i]][[overall_target]]))$overall[1]
  
  
  }
  
  results$Bootstrap$AUC$Niche      <- summary(unlist(results$Bootstrap$AUC$Niche))
  results$Bootstrap$AUC$Reference  <- summary(unlist(results$Bootstrap$AUC$Reference))
  results$Bootstrap$AUC$Combined   <- summary(unlist(results$Bootstrap$AUC$Combined))
  results$Bootstrap$Kappa$Niche    <- summary(unlist(results$Bootstrap$Kappa$Niche))
  results$Bootstrap$Kappa$Reference<- summary(unlist(results$Bootstrap$Kappa$Reference))
  results$Bootstrap$Kappa$Combined <- summary(unlist(results$Bootstrap$Kappa$Combined))
  results$Bootstrap$Accuracy$Niche    <- summary(unlist(results$Bootstrap$Accuracy$Niche))
  results$Bootstrap$Accuracy$Reference<- summary(unlist(results$Bootstrap$Accuracy$Reference))
  results$Bootstrap$Accuracy$Combined <- summary(unlist(results$Bootstrap$Accuracy$Combined))
  
  
  return(results)
  
}


#####################################################################################

### 7) As a map

#####################################################################################


Compile.map <- function(which.prob = 2) {

  ### Takes 1 tree structure, and two sets of thresholds and probabilities
  ### outputs two sets of maps based on these, as well as their mean
  ### These are named as in the illustrative pipeline script
  
  ### Function arguments
  ### which.prob refers to the column of the decision tree output to be selected (2 for TRUE / Presence)
  ### for multinomial predictors this can be changed to ID the relevant category of the target. 
  
 
boot.rast           <- list()
boot.rast_reference <- list()
Combined.rast       <- list()

for(i in 1:length(split.list)) {
  
  ### niche
  
  Final.tree      <- make.final_tree(split.list[[1]], how = 'Sequential', split.key, 
                                     thresholds,probs, i) 
  
  preds.vals      <- predict(Final.tree, preds.frame)
  boot.rast[[i]]  <- JULES.mask
  
  values(boot.rast[[i]]) <- as.numeric(data.frame(preds.vals)[, which.prob])
  
  
  ### reference
  
  Final.tree      <- make.final_tree(split.list[[1]], how = 'Sequential', split.key, 
                                     weighted.thresholds, weighted.probs, i) 
  
  preds.vals      <- predict(Final.tree, preds.frame)
  
  boot.rast_reference[[i]]         <- JULES.mask
  values(boot.rast_reference[[i]]) <- as.numeric(data.frame(preds.vals)[, which.prob])

  ### Combined
  
  Combined.rast[[i]] <- (boot.rast_reference[[i]] + boot.rast[[i]]) / 2
  
}



return(list('Niche' = boot.rast, 'Reference' = boot.rast_reference, 
       'Combined' = Combined.rast))
  
}


#################################################################################

### Functions to make sample weights

##################################################################################


###############################################################

### 1) Load JULES

###############################################################

require(ncdf4)

load.Jules <- function(path, filename) {
  
  JULES.lc<- nc_open(paste0(path, filename))
  
  lon     <- ncvar_get(JULES.lc, "longitude")
  lat     <- ncvar_get(JULES.lc, "latitude", verbose = F)
  vars    <- ncvar_get(JULES.lc, "pseudo")
  
  
  #######################
  
  ### get data
  
  #######################
  
  setwd(path)
  
  r.temp       <- ncvar_get(JULES.lc, "field1391")
  r.temp       <- aperm(r.temp, c(2, 1, 3))
  r.temp       <- lapply(1:27, function(i) r.temp[, c(97:192, 1:96), i])
  r.temp       <- lapply(r.temp, function(x) {raster(x)})
  
  
  for(r in 1:length(r.temp)) {
    
    extent(r.temp[[r]]) <- c(-180, 180, -90, 90)
    
  }
  
  r.temp       <- lapply(r.temp, function(x) {flip(x, direction = 'y')})
  r.temp       <- r.temp[1:27]
  
  names(r.temp)<- c('tropical broadleaf evergreen', 
                    'temperate broadleaf evergreen', 
                    'broadleaf deciduous', 
                    'needleleaf evergreen', 
                    'needleleaf deciduous', 
                    'C3 grass', 'C3 crop','C3 pasture',
                    'C4 grass', 'C4 crop', 'C4 pasture',
                    'evergreen shrubs', 'deciduous shrubs', 
                    'urban', 'lake', 'bare soil', 'ice', letters[1:10])
  
  return(r.temp)
  
}


########################################################################################

### 2) Make sampling thresholds

########################################################################################

Make.sampling_thresholds <- function(vars = list('HDI' = NULL, 
                                                 'Pop_dense' = NULL, 'ETo' = NULL)) {
  
  vars.sum      <- lapply(vars, summary)
  
  Var.threshold <- data.frame(row.names = c('Min', 'LQ', 'Median', 'Mean', 'UQ', 'Max'))
  
  for(i in 1:length(vars.sum)) {
    
    Var.threshold[names(vars.sum)[i]] <- as.numeric(vars.sum[[i]])[1:6]
    
  }
  
  return(Var.threshold[c(2, 3, 4), ]) ## return only quartiles
  
}

###########################################################

### 3) Make DAFI thresholds

###########################################################


Make.DAFIweights <- function(dat, key, thresholds, trim.thresholds = NULL) {
  
  ### DAT should be case study ID, followed by variable columns
  
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


Prepare.weighted_sample    <- function(dat, thresholds, DAFI.weights, trim.thresholds = c(0.3, 3),
                                       var.names = c('HDI', 'Pop_dense', 'ET')) {
  
  ### NB column 1 should be Case Study ID (unique ID)
  
  
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



###########################################################################

### 5) Load secondary data

###########################################################################

load.secondary <- function(path, file_type = '*.tif') {
  
  setwd(path)
  
  rast.files <- list.files(pattern = file_type)
  
  lapply(1:length(rast.files), function(i) {
    
    f.string <- substr(rast.files[i], 1, (nchar(rast.files[i])-4))
    
    assign(f.string, brick(rast.files[i]), envir = .GlobalEnv)
    
  })
  
}
