

require(rpart)


#####################################################################

### 1) Bootstrap tree objects

#####################################################################


boot.tree    <- function(dat, dat.weights = T, form, var.cols, tree.method = 'class', 
                         k = 10000, nsplits = 1, min_node, tree.prune = 5) {

  split.list <- list()

    for(j in 1:k) {
      
      if(dat.weights == TRUE) {
        
        boot.dat <- dat[sample.int(nrow(dat), replace = T, prob = dat$weight), ]
        
      } else {
        
        boot.dat <- dat[sample.int(nrow(dat), replace = T), ]
        
      }
      
      if(dat.weights == T) {

        f <- formula(paste0(form[1], ' ~ ', paste(form[2:length(form)], collapse = ' + ')))
        
        split.list[[j]] <- tree(f, control = tree.control(nrow(boot.dat), mincut = min_node), 
                                      data = boot.dat[, var.cols], weights = boot.dat$weights)
        
        split.list[[j]] <- prune.misclass(split.list[[j]], best = tree.prune)
        
      } else {
        
        f <- formula(paste0(form[1], ' ~ ', paste(form[2:length(form)], collapse = ' + ')))
        
        split.list[[j]] <- tree(f, control = tree.control(nrow(boot.dat), mincut = min_node),
                                      data = boot.dat[, var.cols])
        
        split.list[[j]] <- prune.misclass(split.list[[j]], best = tree.prune)
        
      }
      
      
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

### 5) Reweight predictions based on upsampling

################################################################################



