

##############################################################

### This code reproduces figures in Perkins et al., 2021 (in submission)

##############################################################

library(tree)
library(tidyverse)
library(ggplot2)
library(raster)
library(viridisLite)
library(plotly)
library(maptools)
library(rgdal)

continents   <- readOGR('C:/Users/Oli/Documents/PhD/Model development/Data/Secondary data/Socio economic/Country boundaries/World_Continents')
continents   <- continents[continents@data$CONTINENT != 'Antarctica', ]


########################################################

### Figure 3

########################################################

### Outputs of functions in data weighting

rake.plot <- read.csv('C:/Users/Oli/Documents/PhD/Journal Submissions/Global modelling special issue 2021/Figures/DAFI_weights.csv')

rake.plot <- pivot_longer(rake.plot, cols = c('HDI', 'ET'), names_to = 'Variable') %>% 
              mutate(X = factor(X, levels = c('LQ', 'LMQ', 'UMQ', 'UQ'), ordered = T), 
                     Variable = ifelse(Variable == 'ET', 'ETo', 'HDI'))


ggplot(rake.plot, aes(x = X, y = value, fill = Variable)) + 
  geom_col(position = position_dodge(), colour = 'black') + theme_classic() +
    scale_fill_viridis_d() + xlab('Quartile') + ylab('Proportion of case studies') +
      geom_hline(yintercept = 0.25, linetype = 'dashed') +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1)) 

#################################################################################################

### Figure 5a)

#################################################################################################



Overall_2015 <- brick(unlist(list(`Pre industrial` = sum(Pre.industrial_2015), Transition = sum(Trans_2015), 
                             Industrial = sum(Intense_2015), `Post industrial` = sum(Post_2015))))

plot(Overall_2015, zlim = c(0, 1), 
     col= viridis(255, direction = 1), 
     legend.args = list(text = 'Fractional coverage', side = 3,
                        font = 2, line = 2.5, cex = 0.8))

######################################################################################################

### Figure 5b)

######################################################################################################

TS.frame %>%
  mutate(`AFR` = factor(`AFR`, ordered = T,
  levels = c('Pre', 'Transition', 'Industrial', 'Post', 'Unoccupied'))) %>%
  ggplot(aes(x = Year, y = value)) + 
  geom_smooth(size = 2, aes(colour = AFR, fill = AFR), alpha = 0.5) + 
  theme_classic() +ylab('Fractional coverage') +
  scale_colour_viridis_d() + scale_fill_viridis_d() + 
  theme(text = element_text(size=18))


##########################################################################################################

### Figure 6a)

##########################################################################################################

LS.result <- rbind(Result.crop, Result.livestock, Result.forestry, Result.Nonex)

LS.result %>% filter(AFR != 'Post-industrial' & LS %in% c('Cropland', 'Livestock')) %>%
  ggplot(aes(x = Year, y = unlist.x., colour = AFR)) + geom_line(size = 1.5) +
  facet_grid(LS~.) + theme_classic() + ylab('Fraction of Land system') + 
  scale_colour_viridis_d() + theme(text = element_text(size=15))


########################################################################################

### Figure 6b)

########################################################################################


setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Global modelling special issue 2021/Model outputs')

Continents.frame  <- read.csv('Continents.csv')

Continents.frame %>% mutate(`AFR` = factor(`Fire.system`, ordered = T,
  levels = c('Unoccupied', 'Pre', 'Transition', 'Industrial', 'Post'))) %>%
  filter(Year %in% c(1990, 2003, 2015)) %>%
  mutate(Year = factor(Year, levels = c(1990, 2003, 2015), ordered = T)) %>%
  ggplot(aes(x = Continent, y = value, colour = Year, fill = Year)) + 
  geom_col(size = 1, position = position_dodge(), colour= 'black') + 
  facet_grid(`AFR`~., scales = 'free_y') + 
  scale_y_continuous(limits= c(0, 0.5), breaks = c(0, 0.2, 0.4)) +
  theme_classic() +ylab('Fractional coverage') +
  scale_colour_viridis_d() + scale_fill_viridis_d() + 
  theme(text = element_text(size=18))


########################################################

### Figure 7a) Frequency of variables by node

########################################################

### Made from counting nodes in Classification trees

dat.featureplot <- data.frame('HDI / GDP' = c(12, 5),
                              'Biophysical' = c(1, 8),
                              'Market Access' = c(3, 1), 
                              'Topography' = c(0, 4),
                              'Population' = c(0, 3) 
)

dat.featureplot$`Node number` <- rep(c('1st node', '2nd node or lower'), times = 1)
dat.featureplot <- pivot_longer(dat.featureplot, 
                                cols = colnames(dat.featureplot)[1:5], 
                                values_to = 'Count', names_to = 'Variable type') 

dat.featureplot <- dat.featureplot %>% 
  mutate(`Variable type` = factor(`Variable type`, 
                                  levels = dat.featureplot$`Variable type`[1:5], ordered = T))


dat.featureplot$`Variable type` <- factor(rep(c('HDI & GDP', 'Biophysical', 'Market Access', 'Topography', 'Population'), 
                                              times= 2), levels = c('HDI & GDP', 'Biophysical', 'Market Access', 'Topography', 'Population'), 
                                          ordered = T)

ggplot(dat.featureplot, aes(x = `Variable type`,
                            y = Count, fill = `Node number`)) + geom_col(colour = 'black') + scale_fill_viridis_d() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 15, vjust=0.45))


############################################################################################

### 7b) Relationships of LFS to secondary data

############################################################################################


### data for 2015

Secondary_analysis.frame <- data.frame(Pre = Overall_2015$Pre.industrial[], Trans = Overall_2015$Transition[],
                                       Intense = Overall_2015$Industrial[], Post  = Overall_2015$Post.industrial[],
                                       HDI = HDI[[26]][], GDP = GDP[[26]][])

Secondary_analysis.frame <- Secondary_analysis.frame %>% 
  pivot_longer(cols = c('Pre', 'Trans', 'Intense', 'Post'), names_to = 'Fire system') %>%
  mutate(`Fire system` = factor(`Fire system`, levels = c('Pre', 'Trans', 'Intense', 'Post'), 
                                ordered = T))


ggplot(Secondary_analysis.frame, aes(x = HDI*log(GDP), y = value, colour = `Fire system`)) + 
  geom_point(alpha = 0.1) + theme_classic() + facet_grid(.~`Fire system`) +
  scale_colour_viridis_d() + theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 1)) + ylab('Fractional coverage') +
  theme(text = element_text(size=15))


###############################################################################

### Figure 8

###############################################################################

plot(Pastoralism.tree)
text(Pastoralism.tree)

plot(Pyro_diverse.tree)
text(Pyro_diverse.tree)

plot(Intense_crop.tree)
text(Intense_crop.tree)


##############################################################################

### Figure 9) HANPP comparison

##############################################################################


### Calculations to produce wHANPPe for cropland

wHANPPe.2010 <- data.frame('Pre_industrial' = (CMIP6_Cropland[[21]]*Pre_Crop[[21]]*HANPP[[3]])[], 
                            'Transition' = (CMIP6_Cropland[[21]]*Trans_Crop[[21]]*HANPP[[3]])[], 
                            'Industrial' = (CMIP6_Cropland[[21]]*Intense_Crop[[21]]*HANPP[[3]])[], 
                            'Pre.score' = (Pre_Crop[[21]])[], 
                            'Trans.score' = (Trans_Crop[[21]])[], 
                            'Intense.score' = (Intense_Crop[[21]])[], 
                            'Crop.frac' = CMIP6_Cropland[[21]][]) %>%
  filter(Crop.frac >= 0.1) %>%
  pivot_longer(cols = c('Pre_industrial', 'Transition', 'Industrial'), 
               names_to = 'AFR', values_to = 'value') %>%
  mutate('Coverage' = ifelse(AFR == 'Pre_industrial', Pre.score, 
                             ifelse(AFR == 'Transition', Trans.score, Intense.score)), 
         'AFR' = factor(AFR, levels = c('Pre_industrial', 'Transition', 'Industrial'))) %>%
  filter(!is.na(value) & Coverage >= 0.5) 


wHANPPe.2010 <- dat.2010.crop %>% 
  mutate('AFR' = recode(AFR, 'Pre_industrial' = 'Pre-industrial')) %>%
  group_by(AFR) %>% summarise(mean = mean(value),
                              qs   = quantile(value, 
                                              probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))


wHANPPe.2000 <- data.frame('Pre_industrial' = (CMIP6_Cropland[[11]]*Pre_Crop[[11]]*HANPP[[2]])[], 
                            'Transition' = (CMIP6_Cropland[[11]]*Trans_Crop[[11]]*HANPP[[2]])[], 
                            'Industrial' = (CMIP6_Cropland[[11]]*Intense_Crop[[11]]*HANPP[[2]])[], 
                            'Pre.score' = (Pre_Crop[[11]])[], 
                            'Trans.score' = (Trans_Crop[[11]])[], 
                            'Intense.score' = (Intense_Crop[[11]])[], 
                            'Crop.frac' = CMIP6_Cropland[[11]][]) %>%
  filter(Crop.frac > 0.1) %>%
  pivot_longer(cols = c('Pre_industrial', 'Transition', 'Industrial'), 
               names_to = 'AFR', values_to = 'value') %>%
  mutate('Coverage' = ifelse(AFR == 'Pre_industrial', Pre.score, 
                             ifelse(AFR == 'Transition', Trans.score, Intense.score)), 
         'AFR' = factor(AFR, levels = c('Pre_industrial', 'Transition', 'Industrial'))) %>%
  filter(!is.na(value)  & Coverage > 0.5) 

wHANPPe.2000 <- dat.2000.crop %>% 
  mutate('AFR' = recode(AFR, 'Pre_industrial' = 'Pre-industrial')) %>%
  group_by(AFR) %>% summarise(mean = mean(value),
                              qs   = quantile(value, 
                                              probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))

wHANPPe.1990 <- data.frame('Pre_industrial' = (CMIP6_Cropland[[1]]*Pre_Crop[[1]]*HANPP[[1]])[], 
                            'Transition' = (CMIP6_Cropland[[1]]*Trans_Crop[[1]]*HANPP[[1]])[], 
                            'Industrial' = (CMIP6_Cropland[[1]]*Intense_Crop[[1]]*HANPP[[1]])[], 
                            'Pre.score' = (Pre_Crop[[1]])[], 
                            'Trans.score' = (Trans_Crop[[1]])[], 
                            'Intense.score' = (Intense_Crop[[1]])[], 
                            'Crop.frac' = CMIP6_Cropland[[1]][]) %>%
  filter(Crop.frac > 0.1) %>%
  pivot_longer(cols = c('Pre_industrial', 'Transition', 'Industrial'), 
               names_to = 'AFR', values_to = 'value') %>%
  mutate('Coverage' = ifelse(AFR == 'Pre_industrial', Pre.score, 
                             ifelse(AFR == 'Transition', Trans.score, Intense.score)), 
         'AFR' = factor(AFR, levels = c('Pre_industrial', 'Transition', 'Industrial'))) %>%
  filter(!is.na(value) & Coverage > 0.5) 

wHANPPe.1990 <- dat.1990.crop %>% 
  mutate('AFR' = recode(AFR, 'Pre_industrial' = 'Pre-industrial')) %>%
  group_by(AFR) %>% summarise(mean = mean(value),
                              qs   = quantile(value, 
                                              probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))


### Gather data and plot

All.wHANPPe <- rbind(wHANPPe.1990, wHANPPe.2000, wHANPPe.2010) %>% 
  mutate(Year = rep(c(1990, 2000, 2010), each = 5), 
         Quantile = rep(c("0.1", "0.25", "0.5", "0.75", "0.9"), times = 3)) %>% 
  pivot_wider(names_from = Quantile, values_from  = qs)

All.wHANPPe %>% 
  mutate(AFR = factor(AFR, levels = c('Pre-industrial', 'Transition', 'Industrial'), ordered = T)) %>%
  pivot_longer(cols = c('mean','0.25', '0.5', '0.75'), 
               values_to = 'HANPP', names_to = 'Metric') %>%
  ggplot(aes(x = Year, y = HANPP, fill = Metric)) +
  geom_line(aes(colour=Metric), size= 1) + geom_point(aes(colour = Metric), size = 2) +
  facet_grid(.~AFR) + theme_classic() + scale_colour_viridis_d() +
  ylab('Weighted HANPP Efficiency') + scale_x_continuous(breaks = c(1990, 2000, 2010)) +
  theme(text = element_text(size=16), axis.text.x = element_text(angle = 60, hjust=1))



##############################################################################

### Figure 10) HANPP maps

##############################################################################

Dominant.2010 <- raster::which.max(brick(unlist(list(Pre_Crop[[21]], Trans_Crop[[21]], Intense_Crop[[21]]))))
Dominant.2010[is.na(Dominant.2010)] <- 0

Dominant.2010 <- Dominant.2010*(JULES.mask>0.05)*(CMIP6_Cropland[[21]]>0.1)

plot(Dominant.2010, col = plasma(3), zlim = c(1, 3), legend = T, 
     ext = extent(c(-180, 180, -57.5, 80)), box=FALSE, axes = F)
plot(continents, add=TRUE, lwd = 0.01)
plot(HANPP_original_res[[3]], col = plasma(255), 
     ext = extent(c(-180, 180, -57.5, 80)), box=FALSE, axes = F)
plot(continents, add=TRUE)


