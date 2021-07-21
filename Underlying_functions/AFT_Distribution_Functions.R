

require(rpart)


#####################################################################

### 1) Bootstrap tree objects

#####################################################################

### This function produces k tree structures for a given data set
### In analysis, the tree structures were pruned using a value taken from an initial variable selection


boot.tree    <- function(dat, dat.weights = T, weights = NULL, form, var.cols, tree.method = 'class', 
                         k = 10000, min_node, tree.prune = 5) {

  ### !!! function arguments
  ### dat = data to learn tree structure
  ### dat.weights - does that data have an associated set of weights?
  ### weights - the weights to be supplied to the tree learning algorithm
  ### form - a character string of the type ('target', 'predictor_1', 'predictor_2', etc) that will be translated into a formula object
  ### var.cols - integer providing index of variable columns to use (for speed & memory)
  ### k = number of bootstrap iterations
  ### min_node - the smallest number of data points allowed at any terminal leaf
  ### tree.prune - the size to prune the tree back to during cross-validation
  
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

### This function returns the number of trees that had a given variable as the node at a given point in the tree
### It is used to find the most frequently occuring tree structure in a set of bootstrapped trees

aggregate.boot_tree <- function(splits, nsplits) {
  
  ### !!! function arguments
  ### splits - the output of boot.tree
  ### nsplits - the depth of tree splits to be considered in the resulting list
  
  
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
  
  ### !!! function arguments
  ### splits - the output of boot.tree
  ### key - a named list of the form ('1' = 'var-name', '2' = 'var-name'... etc)
  
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

### Pulls the values of the thresholds used to split a tree from a list of bootstrapped structures

get.split_vals <- function(splits, node_n, var_name) {
  
  ### !!! function arguments
  ### splits - the output of boot.tree
  ### node_n & var_name should be two strings comprised of a key-value pair from the key used in select.boot_tree
  
  
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

### extracts output probabilities from a list of boostrapped trees

get.leaf_probs <- function(split.list, leaf_n) {
  
  ### !!! function arguments
  ### split.list - the output of boot.tree
  ### leaf_n - integer giving the location of the leaf in the tree structure
  
  
  res <- lapply(split.list, function(x) {

        x$frame$yprob[which(names(splits)[leaf_n] == row.names(x$frame)), ]

  })
  
}



####################################################################

### 4) Predict from bootstrapped tree frame

####################################################################

### Assuming a constant structure, and a series of k threshold & associated output probability sets
### this function allows updating of the tree structure with a new set of thresholds / output probs


make.final_tree <- function(tree.template, how = c('Median', 'Random', 'Sequential'),
                            split.key, thresholds, probs, sequential.key = NULL) {

  
  ### !!! function arguments
  ### tree.template - a dataframe of the type my_tree$frame
  ### split.key - named list used in select.boot_tree
  ### thresholds - a list of thresholds from get.split_vals
  ### probs - a list of output probabilities from get.leaf_probs
  ### how arguments: Median takes the median thresholds / probs, random makes a random draw, 
  ### Sequential takes an integer input (sequential.key) to draw from the thresholds/probs- this can then be used as an iterator in a loop or other function
  
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

### Given a known tree structure and a new set of data, this function will relearn the threshold values and output probabilities 
### Optimisation is done using the gini impurity coefficient as a measure
    
Relearn.tree_par <- function(target, variable, weights, threshold.type = 'Lower') {
  
  ### !!! function arguments
  ### target - a factor providing the variable to predict
  ### variable - a numeric or factor used to split the tree at this node
  ### weights - a vector of weights to use to inform tree splitting

  
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

### Utility function
### Calculates a wide range of model metrics for a single tree with two sets of parameter values
    
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

### From a single tree structure & two sets of parameter values (thresholds & probs)
### This function will produce out prediction maps from the tree across all parameter sets

Compile.map <- function() {

  ### uses variables assigned in the global environment in the underlying pipeline
  
 
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




