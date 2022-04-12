
###########################################################################################################
### Code by Oli Perkins April 2022
### Code makes decision tree models of a given target AFR/LFS
### It outputs maps of that LFS's competitiveness globally
###########################################################################################################

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

Landuse.dat    <- merge(DAFI[, c(1, 4)], Landuse, 
                        by.x = 'Case.Study.ID', by.y = 'Case Study ID', all.x = T)[, -c(2, 20)]

Combo.dat      <- cbind(DAFI, Landuse.dat[, 3:7]) %>% 
  mutate('Fire_development_stage' = factor(`Anthropogenic fire regime` == 'Pre-industrial'))

Combo.dat                  <- merge(Combo.dat, Synthetic.DAFI, by = 'Case.Study.ID', all.x = T)
Combo.dat$Market_influence <- Combo.dat$Market_access*Combo.dat$GDP 


Combo.dat      <- Combo.dat[complete.cases(Combo.dat[, c(3:13, 42, 53:55)]), -c(43:51)] ## col 41 is cropland weight
Combo.dat      <- Combo.dat[!is.na(Combo.dat$Cropland_weights) & !is.na(Combo.dat$Fire_development_stage), ]

### Add variable that works for prediction

Combo.dat$HDI_GDP <- Combo.dat$HDI*log(Combo.dat$GDP)

#############################################################

### Make Training set based on NULL and sampled AFTs

#############################################################


### 50 / 50 split of presence and absence

NULL.mod       <- Combo.dat[Combo.dat$Fire_development_stage == FALSE, ]
Pre.dat        <- Combo.dat %>% filter(Fire_development_stage ==TRUE)
Pre.dat        <- Pre.dat[sample.int(nrow(Pre.dat), size = nrow(NULL.mod), replace = T), ]
#Pre.dat$Cropland_mixed_weights <- median(NULL.mod$Cropland_mixed_weights, na.rm = T)

Pre.train.rows <- sample.int(nrow(Pre.dat), size = nrow(Pre.dat)/1.25)
NULL.train.rows<- sample.int(nrow(NULL.mod), size = nrow(NULL.mod)/1.25)

Pre.train <- Pre.dat[Pre.train.rows, ]
NULL.train<- NULL.mod[NULL.train.rows, ]

Train.dat <- rbind(NULL.train, Pre.train)
Test.dat  <- rbind(NULL.mod[-c(NULL.train.rows), ], 
                   Pre.dat[-c(Pre.train.rows), ])

All.dat <- rbind(Train.dat, Test.dat)

#############################################################

### 2) Make tree submodel 

#############################################################

### 2a) Initial trees to ID candidate variables

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

##############################################

split.list       <- boot.tree(All.dat, 
                              dat.weights = TRUE, weights = All.dat$Cropland_weights,
                              form = c('Fire_development_stage', 'HDI', 'Market_influence'), 
                              var.cols =  3:ncol(All.dat), tree.method = 'class', 
                              k = 1000, min_node = min(stage.tree2$frame$n), 
                              tree.prune = 3) ## prune val from stage 1

splits           <- aggregate.boot_tree(split.list, 15)
n.nodes          <- summary(unlist(lapply(split.list, function(x) {length(which(x$frame$var != '<leaf>'))})))

### choose trees
split.key        <- list('1' = 'Market_influence', '2' = 'HDI')
test.list        <- select.boot_tree(split.list, split.key)
split.list       <- test.list
remove(test.list)

### get tree components
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

#################################################

### 3) Performance of updated model

#################################################


dat.samps   <- lapply(1:length(split.list), 
                      function (i) sample.int(nrow(All.dat), 
                                              replace = T, prob = All.dat$Cropland_weights))


Final.tree  <- make.final_tree(split.list[[1]], split.key, 
                               thresholds, probs, how = 'Median') 

Final.tree2 <- make.final_tree(split.list[[1]], split.key, 
                               thresholds, probs, how = 'Median') 

Metrics      <- Assess.model(bootstrap = T, dat = All.dat,
                             target = 'Fire_development_stage', 
                             overall_target = 'Fire_development_stage')





