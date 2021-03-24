

require(rpart)


#####################################################################

### 1) Bootstrap tree objects

#####################################################################


boot.tree    <- function(dat, dat.weights = T, weights = NULL, form, var.cols, tree.method = 'class', 
                         k = 10000, nsplits = 1, min_node, tree.prune = 5) {

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
        
        split.list[[j]] <- tree(f, control = tree.control(nrow(boot.dat), mincut = min_node),
                                      data = boot.dat[, var.cols])
        
        split.list[[j]] <- prune.misclass(split.list[[j]], best = tree.prune)

    }

    return(split.list)
    
    
  }



##########################################################

### 2) aggregate bootstrapped trees

##########################################################

aggregate.boot_tree <- function(splits, nsplits) {
  
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
  
  res <- lapply(splits, function(x) {
    
    try({
      
      if(x$frame$var[row.names(x$frame) == as.character(node_n)] == var_name) {
        
        ### could we make it work for other values?
        
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
  
  res <- lapply(split.list, function(x) {

        x$frame$yprob[which(names(splits)[leaf_n] == row.names(x$frame)), ]

  })
  
}



####################################################################

### 4) Predict from bootstrapped tree frame

####################################################################

make.final_tree <- function(tree.template, how = c('Median', 'Random', 'Sequential'),
                            split.key, thresholds, probs, sequential.key = NULL) {

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
          
      
      ft$frame$yprob[i, ] <- as.numeric(probs[[probs.val]][sequential.key, ])
      
        }
    
      }
  
   }
  
 return(ft)

}


################################################################################

### 5) Recalculate tree thresholds based on upsampling

################################################################################

Relearn.tree_par <- function(target, variable, threshold.type = 'Lower', weights) {
  
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
                         bootstrap = T, target = 'Fire_development_stage') {
  
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
  results$All_data$Class_Prediction$Combined <- caret::confusionMatrix(factor(overall.pred), factor(dat[[target]]))$overall[1:2]
  
  
  
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
  
  
  results$Bootstrap$AUC$Combined[[i]]    <- ModelMetrics::auc(as.numeric(bootstrap_dat[[i]][[target]])-1, 
                                            apply(Combined.preds, 1, mean))
  
  overall.pred                          <- apply(Combined.preds, 1, mean) >= 0.5
  results$Bootstrap$Kappa$Combined[[i]] <- caret::confusionMatrix(factor(overall.pred), factor(bootstrap_dat[[i]][[target]]))$overall[2]
  results$Bootstrap$Accuracy$Combined[[i]] <- caret::confusionMatrix(factor(overall.pred), factor(bootstrap_dat[[i]][[target]]))$overall[1]
  
  
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


Compile.map <- function() {

 
boot.rast           <- list()
boot.rast_reference <- list()
Combined.rast       <- list()

for(i in 1:length(split.list)) {
  
  ### niche
  
  Final.tree      <- make.final_tree(split.list[[1]], how = 'Sequential', split.key, 
                                     thresholds,probs, i) 
  
  preds.vals      <- predict(Final.tree, preds.frame)
  boot.rast[[i]]  <- JULES.mask
  
  values(boot.rast[[i]]) <- as.numeric(data.frame(preds.vals)[, 2])
  
  
  ### reference
  
  Final.tree      <- make.final_tree(split.list[[1]], how = 'Sequential', split.key, 
                                     weighted.thresholds, weighted.probs, i) 
  
  preds.vals      <- predict(Final.tree, preds.frame)
  
  boot.rast_reference[[i]]         <- JULES.mask
  values(boot.rast_reference[[i]]) <- as.numeric(data.frame(preds.vals)[, 2])

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





