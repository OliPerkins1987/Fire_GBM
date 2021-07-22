

library(raster)
library(tidyverse)
library(devtools)

source_url('https://github.com/OliPerkins1987/Fire_GBM/blob/main/SESMO_Code/AFR_Competition_Simulation_v2.RData')


Compete.AFT <- function(AFT.list, threshold = 0.1) {
  
  AFT.list   <- lapply(AFT.list, function(x) {x[x[] < threshold] <- 0
  return(x)})
  AFT.ref    <- sum(unlist(brick(AFT.list)))
  AFT.compete<- lapply(AFT.list, function(x) x/AFT.ref)
  
  AFT.compete
  
}



##########################################################################

### Compile model outputs

##########################################################################


Years <- 1990:2015

for(Year in 1:length(Years)) {
  
  
  ##############################################
  
  ### Xaxis
  
  ##############################################
  
  Xmod <- list(Cropland = CMIP6_Cropland[[Year]], 
               Pasture  = CMIP6_Pasture[[Year]], 
               Rangeland= CMIP6_Rangeland[[Year]],
               Forestry = CMIP6_Forest[[Year]], 
               Urban    = CMIP6_Urban[[Year]],
               Nonex    = list(All = Xaxis.1[[Year]], Forest = Xaxis.2[[Year]]), 
               Unoccupied = Pre.non[[Year]])
  
  ### calc x fraction of forestry, unoccupied and non-extractive
  
  Xmod$Forestry       <- Xmod$Forestry * (1-Xmod$Nonex$Forest) * (1-Xmod$Unoccupied)
  Xmod$Openvegetation <- 1 - Xmod$Cropland - Xmod$Pasture - Xmod$Rangeland - Xmod$Forestry - Xmod$Urban
  Xmod$Nonex$Final    <- Xmod$Openvegetation * (Xmod$Nonex$All / (Xmod$Nonex$All + Xmod$Unoccupied))
  Xmod$Unoccupied     <- Xmod$Openvegetation * (Xmod$Unoccupied / (Xmod$Nonex$All + Xmod$Unoccupied))
  Xmod$Nonex          <- Xmod$Nonex$Final
  Xmod$Openvegetation <- NULL
  
  
  ####################################################################################
  
  ### Yaxis
  
  ####################################################################################
  
  for (sim in c('Mean', 'Upper, Lower')) {
    
    if(sim == 'Mean') {
      
      Ymod <- list(Cropland = list(Pre.crop[[Year]], Trans.crop[[Year]], Intense.crop[[Year]]), 
                   Pasture  = list(Trans.pasture[[Year]], Intense.pasture[[Year]]), 
                   Rangeland= list(Pre.range[[Year]], Trans.range[[Year]], Intense.range[[Year]]), 
                   Forestry = list(Trans.forest[[Year]], Intense.forest[[Year]], Post.forest[[Year]]),
                   Nonex    = list(Pre.forest[[Year]], Trans.non[[Year]], Intense.non[[Year]], Post.non[[Year]]))
      
      
      
    } else if(sim == 'Upper') {
      
      Ymod <- list(Cropland = list(Pre.crop[[Year]], Trans.crop[[Year]], Intense.crop[[Year]]), 
                   Pasture  = list(Trans.pasture[[Year]], Intense.pasture[[Year]]), 
                   Rangeland= list(Pre.range[[Year]], Trans.range[[Year]], Intense.range[[Year]]), 
                   Forestry = list(Trans.forest[[Year]], Intense.forest[[Year]], Post.forest[[Year]] + Sd.Post.forest[[Year]]),
                   Nonex    = list(Pre.forest[[Year]], Trans.non[[Year]], Intense.non[[Year]], Post.non[[Year]] + Sd.Post.non[[Year]]))
      
      
    } else {
      
      Ymod <- list(Cropland = list(Pre.crop[[Year]], Trans.crop[[Year]], Intense.crop[[Year]]), 
                   Pasture  = list(Trans.pasture[[Year]], Intense.pasture[[Year]]), 
                   Rangeland= list(Pre.range[[Year]], Trans.range[[Year]], Intense.range[[Year]]), 
                   Forestry = list(Trans.forest[[Year]], Intense.forest[[Year]], Post.forest[[Year]] - Sd.Post.forest[[Year]]),
                   Nonex    = list(Pre.forest[[Year]], Trans.non[[Year]], Intense.non[[Year]], Post.non[[Year]] - Sd.Post.non[[Year]]))
    }
    
    
    for(k in 1:length(Ymod)) {
      
      for(k.map in 1:length(Ymod[[k]])) {
        
        Ymod[[k]][[k.map]][Ymod[[k]][[k.map]] < 0] <- 0
        
        Ymod[[k]][[k.map]][Ymod[[k]][[k.map]] > 1] <- 1
        
      }
      
    }
    
    
    Cropland.compete  <- Compete.AFT(AFT.list = Ymod$Cropland)
    Pasture.compete   <- Compete.AFT(AFT.list = Ymod$Pasture)
    Rangeland.compete <- Compete.AFT(AFT.list = Ymod$Rangeland)
    Forestry.compete  <- Compete.AFT(AFT.list = Ymod$Forestry)
    Nonex.compete     <- Compete.AFT(AFT.list = Ymod$Nonex)
    
    
    
    
    #####################################################################################################
    
    ### Write files out
    
    #####################################################################################################
    
    if(sim == 'Mean') {
      
      setwd('C:/Users/Oli/Documents/PhD/Model development/AFT Distribution/Beta version/Combined model/Maps/All years/Yaxis')
      
    } else if(sim == 'Upper') {
      
      setwd('C:/Users/Oli/Documents/PhD/Model development/AFT Distribution/Beta version/Combined model/Maps/All years/Yaxis_upper/Post')
      
    } else {
      
      setwd('C:/Users/Oli/Documents/PhD/Model development/AFT Distribution/Beta version/Combined model/Maps/All years/Yaxis_lower/Post')
      
      
    }
    
    
    dir.create(paste0(getwd(), '/', (Year+1989)))
    setwd(paste0(getwd(), '/', (Year+1989)))
    
    writeRaster(brick(unlist(Cropland.compete)), 'Cropland.tif', overwrite = T)
    writeRaster(brick(unlist(Rangeland.compete)), 'Rangeland.tif', overwrite = T)
    writeRaster(brick(unlist(Pasture.compete)), 'Pasture.tif', overwrite = T)
    writeRaster(brick(unlist(Forestry.compete)), 'Forestry.tif', overwrite = T)
    writeRaster(brick(unlist(Nonex.compete)), 'Nonex.tif', overwrite = T)
    
    
    print(Year)
    
  }
  
}



