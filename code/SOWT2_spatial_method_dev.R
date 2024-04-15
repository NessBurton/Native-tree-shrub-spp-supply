
# date: 13-02-24
# author: VB
# purpose: Explore spatial method for reproducing woodland communities

### working dirs ---------------------------------------------------------------

wd <- "C:/Users/vbu/OneDrive - the Woodland Trust/Projects/CO&E - SoWT2/Project-SoWT2" # WT laptop path
dirData <- paste0(wd,"/data-raw/")
dirScratch <- paste0(wd,"/data-scratch/")
dirOut <- paste0(wd,"data-out")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(sf)
library(stars)

### read in regions of provenance ----------------------------------------------

RoP_poly <- st_read(paste0(dirData,"Forest_Reproductive_Materials_Regions_Of_Provenance_GB.shp"))

summary(RoP_poly)

ggplot()+
  geom_sf(RoP_poly, mapping = aes(fill = as.factor(SEED_ZONES)))+
  labs(fill = "Native seed zones")+
  theme_minimal()

### read in species shapefiles -------------------------------------------------

lstSpecies <- c("Hornbeam", "Holly", "Hazel", "Guelder_rose")

myPal <- c("4" = "#005A32",
           "3" = "#41AB5D", 
           "2" = "#1B9E77",
           "1" = "darkkhaki",
           "0" = "white")

for (i in lstSpecies){
  
  #i <- lstSpecies[1]
  
  species <- st_read(paste0(dirData,i,".shp"))
  
  species$Code <- factor(species$Code, levels = c(NA,0,1,1.5,2,2.5,3,4))
  
  print(ggplot()+
          geom_sf(species, mapping = aes(fill = Code))+
          scale_fill_manual(values = myPal)+
          labs(fill = "Appropriateness")+
          ggtitle(i)+
          theme_minimal())
  
}

