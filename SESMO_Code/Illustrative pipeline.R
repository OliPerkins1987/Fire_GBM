
### Code by Oli Perkins April 2021
### Code makes decision tree models of a given target AFT / FDS
### It outputs maps of that AFT's competitiveness globally

library(tidyverse)
library(ggplot2)
library(viridisLite)
library(maps)
library(readxl)
library(tree)
library(rpart)
library(caret)
library(raster)
library(rgdal)
library(ncdf4)
library(devtools)

set.seed(1987)

###############################

### 1) Load data

###############################

load(url("https://github.com/OliPerkins1987/Fire_GBM/blob/main/Data/AFT_Distribution_Data_15052021.RData?raw=true"))
source_url("https://raw.githubusercontent.com/OliPerkins1987/Fire_GBM/main/Underlying_functions/AFT_Distribution_Functions.R")


#############################################################

### Combine data for modelling

#############################################################

### This combines spatial information from DAFI with associated Land use and secondary data sampled at those points

Landuse.dat    <- merge(DAFI[, c(1, 4)], Landuse, 
                      by.x = 'Case.Study.ID', by.y = 'Case Study ID', all.x = T)[, -c(2, 20)]

Combo.dat      <- cbind(DAFI, Landuse.dat[, 3:7]) %>% 
                    mutate('Fire_development_stage' = factor(`Fire development stage` == 'Pre-industrial'))

Combo.dat                  <- merge(Combo.dat, Synthetic.DAFI, by = 'Case.Study.ID', all.x = T)
Combo.dat$Market_influence <- Combo.dat$Market_access*Combo.dat$GDP 


### Tidy data - remove unused variables and missing values
Combo.dat      <- Combo.dat[complete.cases(Combo.dat[, c(3:13, 42, 53:55)]), -c(43:51)] ## col 41 is cropland weight
Combo.dat      <- Combo.dat[!is.na(Combo.dat$Cropland_weights) & !is.na(Combo.dat$Fire_development_stage), ]

### Add convlution variable(s) for prediction

Combo.dat$HDI_GDP <- Combo.dat$HDI*log(Combo.dat$GDP)


#############################################################

### Make Training set with 50/50 split of presence and absence

#############################################################

### 50 / 50 split of presence and absence

NULL.mod       <- Combo.dat[Combo.dat$Fire_development_stage == FALSE, ]
Pre.dat        <- Combo.dat %>% filter(Fire_development_stage ==TRUE)
Pre.dat        <- Pre.dat[sample.int(nrow(Pre.dat), size = nrow(NULL.mod), replace = T), ]

Pre.train.rows <- sample.int(nrow(Pre.dat), size = nrow(Pre.dat)/1.25)
NULL.train.rows<- sample.int(nrow(NULL.mod), size = nrow(NULL.mod)/1.25)

Pre.train <- Pre.dat[Pre.train.rows, ]
NULL.train<- NULL.mod[NULL.train.rows, ]

Train.dat <- rbind(NULL.train, Pre.train)
Test.dat  <- rbind(NULL.mod[-c(NULL.train.rows), ], 
                   Pre.dat[-c(Pre.train.rows), ])

#############################################################

### 2) Make tree submodel 

#############################################################

### 2a) Initial trees to ID candidate variables
### We grow a large tree using all candidate predictors
### Then prune this to identify a plausible set

stage.tree      <- rpart(Fire_development_stage ~ ., 
                        data = Train.dat[, c(3:9, 43:47)], 
                        weights = Train.dat$Cropland_weights, method = 'class')

plot(stage.tree)
text(stage.tree, pretty=0)
stage.tree.pred = predict(stage.tree, Train.dat, type="class", weights = Train.dat$Cropland_weights)
caret::confusionMatrix(stage.tree.pred, Train.dat$Fire_development_stage)

### select pruning threshold
plotcp(stage.tree)
stage.tree$cptable
stage.tree2      <- prune(stage.tree,cp= stage.tree$cptable[3,"CP"]) ### 3 entails 3 nodes etc

### thinner tree
plot(stage.tree2)
text(stage.tree2, pretty=0)
stage.pred2      <- predict(stage.tree2, Train.dat, type="class", weights = Train.dat$Cropland_weights)
caret::confusionMatrix(factor(stage.pred2), factor(Train.dat$Fire_development_stage))
Metrics::auc(as.numeric(Train.dat$Fire_development_stage)-1, predict(stage.tree2, Train.dat, type = 'prob')[, 2])

### Compare with test set
overall.pred <- predict(stage.tree2, Test.dat, type = 'class', weights = Test.dat$Cropland_weights)
caret::confusionMatrix(factor(overall.pred ), factor(Test.dat$Fire_development_stage))
Metrics::auc(as.numeric(Test.dat$Fire_development_stage)-1, predict(stage.tree2, Test.dat, type = 'prob')[, 2])

##############################################

### 2b) Boostrapped tree structures

### Based on the variable selection excercise above, we now search for the most resilient tree structure
### This is done through bootstrapping

##############################################

split.list       <- boot.tree(Train.dat, 
                     dat.weights = TRUE, weights = Train.dat$Cropland_weights,
                      form = c('Fire_development_stage', 'HDI', 'Market_influence'), 
                          var.cols =  3:ncol(Train.dat), tree.method = 'class', 
                              k = 1000, min_node = min(stage.tree2$frame$n), 
                               tree.prune = 3) ## prune val from stage 1

### Identify splits in bootstrapped tree structures
splits           <- aggregate.boot_tree(split.list, 15)
n.nodes          <- summary(unlist(lapply(split.list, function(x) {length(which(x$frame$var != '<leaf>'))})))

### choose trees based on frequency of splits
split.key        <- list('1' = 'Market_influence', '2' = 'HDI')
test.list        <- select.boot_tree(split.list, split.key)
split.list       <- test.list
remove(test.list)

##################################

### Extract threshold values and output probabilities from bootstrapped trees

##################################

splits           <- aggregate.boot_tree(split.list, 7)
splits           <- splits[sapply(splits, sum) == length(split.list)]
names(splits)    <- rownames(split.list[[1]]$frame)

thresholds       <- lapply(names(split.key), function(z) {
  
                    unlist(get.split_vals(split.list , as.character(z), as.character(split.key[z])))
                        
                        })

probs            <- lapply(1:length(splits), function(i) {
  
                      if(as.numeric(splits[[i]][1]) == length(split.list)) {
                        
                        res <- get.leaf_probs(split.list, i)
                        
                      } else {
                        
                        res <- 'Node'
                        
                       }
  
                      res
  
                      })

probs        <- lapply(probs, function(x) bind_rows(lapply(x, as.data.frame.list)))
names(probs) <- names(splits)


##########################################################

## 3) Predict on overall data

##########################################################

overall.pred <- predict(stage.tree2, Test.dat, type = 'class')
caret::confusionMatrix(factor(overall.pred ), factor(Test.dat$Fire_development_stage))

#Final tree can also be a random sample of values for Uncert Quant - how = 'Random'
Final.tree   <- make.final_tree(split.list[[1]], split.key, thresholds, probs, 
                                how = 'Median') 

overall.pred <- predict(Final.tree, Test.dat, type = 'class', weights = Test.dat$Cropland_weights)
caret::confusionMatrix(factor(overall.pred), factor(Test.dat$Fire_development_stage))


################################################################################

### 4) Reweight thresholds and probabilities based on upsampling to match reference set

################################################################################

### weighted thresholds

weighted.thresholds <- thresholds
dat.samps           <- lapply(1:length(split.list), function (i) sample.int(nrow(Combo.dat), replace = T, prob = Combo.dat$Cropland_weights))


for(i in 1:length(split.list)) {
  
  dat.boot                      <- Combo.dat[dat.samps[[i]], ]
  
  weighted.thresholds[[1]][[i]] <- Relearn.tree_par(dat.boot$Fire_development_stage, 
                                     dat.boot$Market_influence, weights = dat.boot$Cropland_weights, threshold.type = 'Lower')$maximum
  
  Final.tree$frame$splits[1, ]  <- c(paste0("<", as.character(weighted.thresholds[[1]][[i]])), 
                                     paste0(">", as.character(weighted.thresholds[[1]][[i]])))
  
  key <- predict(Final.tree, dat.boot, type = 'where')
  
  weighted.thresholds[[2]][[i]] <- Relearn.tree_par(dat.boot$Fire_development_stage[which(key %in% c(3, 4))], 
                                      dat.boot$HDI[which(key %in% c(3, 4))], 
                                      weights = dat.boot$Cropland_weights, threshold.type = 'Lower')$maximum
 
  print(i)
   
}


 ### weighted output probabilities

weighted.probs <- list()

for(i in 1:length(split.list)) {
  
  Final.tree          <- make.final_tree(split.list[[1]], how = 'Sequential', 
                                         split.key, weighted.thresholds, probs, i)
  
  dat.boot            <- Combo.dat[dat.samps[[i]], ]
  
  node_to_datapoint   <- predict(Final.tree, dat.boot, type = 'where')
  
  weighted.probs[[i]] <- dat.boot %>% split(node_to_datapoint) %>% 
                          lapply(function(x) {table(x$Fire_development_stage) / nrow(x)})
  
  print(i)
  
}

### Some bootstrap samples due to failure of oprimisiation algorithm,
### may turn up with the incorrect number of nodes / terminal leafs
### Remove these

Node_n         <- 3 ### correct number of nodes

weighted.probs <- lapply(weighted.probs, function(x) bind_rows(lapply(x, as.data.frame.list)))
temp           <- lapply(weighted.probs, function(x) {mutate(x, 'Leaf' = 1:nrow(x))})
temp           <- lapply(temp, function (x) {mutate(x, 'Max_leaf' = max(Leaf))})
temp           <- lapply(1:length(temp), function(i) {if(max(temp[[i]]$Max_leaf) != Node_n) {NULL} else {temp[[i]]}})
dud.key        <- lapply(temp, function(x) {is.null(x)})
temp           <- plyr::rbind.fill(temp)
temp           <- temp[, 1:3]
temp           <- lapply(split(temp, temp$Leaf), function(x) x[, 1:2])

################################################################################

### Update tree lists to remove bootstrap samples where optimisation failed

################################################################################

split.list          <- split.list[!unlist(dud.key)]
thresholds          <- lapply(thresholds, function(x) x[!unlist(dud.key)])
weighted.thresholds <- lapply(weighted.thresholds, function(x) {x[!unlist(dud.key)]}) 
probs               <- lapply(probs, function(x) if(length(x) > 1) {x[which(!unlist(dud.key)), ]} else{x})
weighted.probs      <- lapply(1:length(probs), function(i) {if(length(probs[[i]]) == 1) {probs[[i]]} else{ temp[[max(c(i- (length(probs) - length(temp)), 1))]]}})

names(weighted.probs) <- names(probs)



#################################################

### 5) Performance of updated model

#################################################

Final.tree  <- make.final_tree(split.list[[1]], split.key, 
                  thresholds, probs, how = 'Median') 

Final.tree2 <- make.final_tree(split.list[[1]], split.key, 
                  weighted.thresholds, weighted.probs, how = 'Median') 

Metrics     <- Assess.model(bootstrap = T)


#########################################################

### 6) As a map

#########################################################

preds.frame           <- data.frame(HDI[[1]][], (MA.Synthetic[[1]] * GDP[[1]])[])
colnames(preds.frame) <- c('HDI', 'Market_influence')

### Compile & plot map

combined.rast <- Compile.map()

plot(calc(brick(unlist(combined.rast$Combined)), mean) * (JULES.mask > 0.05) * (JULES.icemask < 0.95))




