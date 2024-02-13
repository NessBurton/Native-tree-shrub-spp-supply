
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
