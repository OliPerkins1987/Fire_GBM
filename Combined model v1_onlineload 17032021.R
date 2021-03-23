
library(tidyverse)
library(raster)


load(url("https://github.com/OliPerkins1987/Fire_GBM/raw/main/Working_mod_JM.RData"))
load(url("https://github.com/OliPerkins1987/Fire_GBM/raw/main/NULL_model.RData"))

Compete.AFT <- function(AFT.list, threshold = 0.1) {
  
  AFT.list   <- lapply(AFT.list, function(x) {x[x[] < threshold] <- 0
  return(x)})
  AFT.ref    <- sum(unlist(brick(AFT.list)))
  AFT.compete<- lapply(AFT.list, function(x) x/AFT.ref)
  
  AFT.compete
}

#####################################################################################

### 1) Decision tree model

#####################################################################################


####################################################################

### 2016

####################################################################


####################

### a) Cropland

####################

Cropland.compete  <- Compete.AFT(AFT.list = Ymod$Cropland)
lapply(Cropland.compete, function(x) (plot(x, zlim = c(0, 1))))
Cropland_dominant <- raster::which.max(brick(unlist(Cropland.compete)))
plot(Cropland_dominant * (CMIP6_Cropland_2016 > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### b) Pasture

#################

Pasture.compete  <- Compete.AFT(AFT.list = Ymod$Pasture)
lapply(Pasture.compete, function(x) (plot(x, zlim = c(0, 1))))
Pasture_dominant <- raster::which.max(brick(unlist(Pasture.compete)))
plot(Pasture_dominant * (CMIP6_Pasture[[26]] > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### c) Rangeland

#################

Rangeland.compete  <- Compete.AFT(AFT.list = Ymod$Rangeland)
lapply(Rangeland.compete, function(x) (plot(x, zlim = c(0, 1))))
Rangeland_dominant <- raster::which.max(brick(unlist(Rangeland.compete)))
plot(Rangeland_dominant * (CMIP6_Rangeland[[26]] > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### d) Forestry

#################

Forestry.compete  <- Compete.AFT(AFT.list = Ymod$Forestry)
lapply(Forestry.compete, function(x) (plot(x, zlim = c(0, 1))))
Forestry_dominant <- raster::which.max(brick(unlist(Forestry.compete)))
plot(Forestry_dominant * (Xmod$Forestry > 0.01), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### e) Non-extractive

#################

Nonex.compete  <- Compete.AFT(AFT.list = Ymod$Nonex)
lapply(Nonex.compete, function(x) (plot(x, zlim = c(0, 1))))
Nonex_dominant <- raster::which.max(brick(unlist(Nonex.compete)))
plot(Nonex_dominant * (Xmod$Openvegetation != 0), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))


#*********************************************************************************

##################################################################################

### 1990

##################################################################################

#*********************************************************************************



####################

### 1) Cropland

####################

Cropland.compete  <- Compete.AFT(AFT.list = Ymod1990$Cropland)
lapply(Cropland.compete, function(x) (plot(x, zlim = c(0, 1))))
Cropland_dominant <- raster::which.max(brick(unlist(Cropland.compete)))
plot(Cropland_dominant * (CMIP6_Cropland_1990 > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### 2) Pasture

#################

Pasture.compete  <- Compete.AFT(AFT.list = Ymod1990$Pasture)
lapply(Pasture.compete, function(x) (plot(x, zlim = c(0, 1))))
Pasture_dominant <- raster::which.max(brick(unlist(Pasture.compete)))
plot(Pasture_dominant * (CMIP6_Pasture[[1]] > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### 3) Rangeland

#################

Rangeland.compete  <- Compete.AFT(AFT.list = Ymod1990$Rangeland)
lapply(Rangeland.compete, function(x) (plot(x, zlim = c(0, 1))))
Rangeland_dominant <- raster::which.max(brick(unlist(Rangeland.compete)))
plot(Rangeland_dominant * (CMIP6_Rangeland[[1]] > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### 4) Forestry

#################

Forestry.compete  <- Compete.AFT(AFT.list = Ymod1990$Forestry)
lapply(Forestry.compete, function(x) (plot(x, zlim = c(0, 1))))
Forestry_dominant <- raster::which.max(brick(unlist(Forestry.compete)))
plot(Forestry_dominant * (Xmod1990$Forestry > 0.01), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### 5) Nonex

#################

Nonex.compete  <- Compete.AFT(AFT.list = Ymod1990$Nonex)
lapply(Nonex.compete, function(x) (plot(x, zlim = c(0, 1))))
Nonex_dominant <- raster::which.max(brick(unlist(Nonex.compete)))
plot(Nonex_dominant * (Xmod1990$Openvegetation != 0), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))


#################################################################################

### 2) NULL model

#################################################################################

####################################################################

### 2016

####################################################################


####################

### a) Cropland

####################


for(i in 1:length(Cropland.2016.preds)) {extent(Cropland.2016.preds[[i]]) <- c(-180, 180, -90, 90)}
Cropland.compete  <- Compete.AFT(AFT.list = Cropland.2016.preds)
lapply(Cropland.compete, function(x) (plot(x, zlim = c(0, 1))))
Cropland_dominant <- raster::which.max(brick(unlist(Cropland.compete)))
plot(Cropland_dominant * (CMIP6_Cropland_2016 > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### b) Pasture

#################

for(i in 1:length(Pasture.2016.preds)) {extent(Pasture.2016.preds[[i]]) <- c(-180, 180, -90, 90)}
Pasture.compete  <- Compete.AFT(AFT.list = Pasture.2016.preds)
lapply(Pasture.compete, function(x) (plot(x, zlim = c(0, 1))))
Pasture_dominant <- raster::which.max(brick(unlist(Pasture.compete)))
plot(Pasture_dominant * (CMIP6_Pasture[[26]] > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### c) Rangeland

#################

for(i in 1:length(Rangeland.2016.preds)) {extent(Rangeland.2016.preds[[i]]) <- c(-180, 180, -90, 90)}
Rangeland.compete  <- Compete.AFT(AFT.list = Rangeland.2016.preds)
lapply(Rangeland.compete, function(x) (plot(x, zlim = c(0, 1))))
Rangeland_dominant <- raster::which.max(brick(unlist(Rangeland.compete)))
plot(Rangeland_dominant * (CMIP6_Rangeland[[26]] > 0.05), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### d) Forestry

#################

for(i in 1:length(Forestry.2016.preds)) {extent(Forestry.2016.preds[[i]]) <- c(-180, 180, -90, 90)}
Forestry.compete  <- Compete.AFT(AFT.list = Forestry.2016.preds)
lapply(Forestry.compete, function(x) (plot(x, zlim = c(0, 1))))
Forestry_dominant <- raster::which.max(brick(unlist(Forestry.compete)))
plot(Forestry_dominant * (Xmod$Forestry > 0.01), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))

#################

### e) Non-extractive

#################

for(i in 1:length(Nonex.2016.preds)) {extent(Nonex.2016.preds[[i]]) <- c(-180, 180, -90, 90)}
Nonex.compete  <- Compete.AFT(AFT.list = Nonex.2016.preds)
lapply(Nonex.compete, function(x) (plot(x, zlim = c(0, 1))))
Nonex_dominant <- raster::which.max(brick(unlist(Nonex.compete)))
plot(Nonex_dominant * (Xmod$Openvegetation != 0), col=colorRampPalette(c("white", "green", "yellow", "blue"))(255))


