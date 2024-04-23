
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
library(terra)
library(stringr)
library(magrittr)

### read in species shapefiles -------------------------------------------------

# manually first, need to check and clean

hornbeam <- st_read(paste0(dirData,"Hornbeam.shp"))
hornbeam
unique(hornbeam$Code)

# convert NAs to 0
hornbeam$Code[which(is.na(hornbeam$Code))] <- 0
hornbeam$Code <- factor(hornbeam$Code, levels = c(0,1,1.5,2,2.5,3,4))

ggplot()+
  geom_sf(hornbeam, mapping = aes(fill = Code), colour = NA)+
  scale_fill_brewer(type = "seq", palette = 2)+
  labs(fill = "Appropriateness")+
  ggtitle(i)+
  theme_minimal()

# loop once data cleaned

lstSpecies <- c("Hornbeam", "Holly", "Hazel", "Guelder_rose")

for (i in lstSpecies){
  
  #i <- lstSpecies[1]
  
  species <- st_read(paste0(dirData,i,".shp"))
  
  species$Code <- factor(species$Code, levels = c(NA,0,1,1.5,2,2.5,3,4))
  
  print(ggplot()+
          geom_sf(species, mapping = aes(fill = Code), colour = NA)+
          scale_fill_brewer(type = "seq", palette = 2)+
          labs(fill = "Appropriateness")+
          ggtitle(i)+
          theme_minimal())
  
}


### read in England creation target data ---------------------------------------

# woodland opportunity data from FoE https://friendsoftheearth.uk/nature/woodland-opportunity-local-authority-full-dataset 
dfOpportunity <- read.csv(paste0(dirData,"Woodland_opportunity_by_local_authority_full_data.csv"))
head(dfOpportunity)

# local authorities shapefile from https://geoportal.statistics.gov.uk/datasets/196d1a072aaa4882a50be333679d4f63/explore
sfLA <- st_read(paste0(dirData,"Local_Authority_Districts_May_2022_UK_BFE_V3_2022_3331011932393166417/LAD_MAY_2022_UK_BFE_V3.shp"))
head(sfLA)

  unique(sfLA$LAD22CD)
# filter to England

sfLA <- sfLA %>% filter(grepl("E",LAD22CD))

# check whether LA names match
dfOpportunity$local.authority %in% sfLA$LAD22NM
unique(dfOpportunity$local.authority)
unique(sfLA$LAD22NM)
# need to strip "district" and "B" from most the the dfOpportunity entries
words <- c("[()]", "London Boro")
pat <-str_c(words, collapse = "|")
words2 <- c("District B", " B")
pat2 <- str_c(words2, collapse = "|")
dfOpportunity$local.authority %>%  str_remove_all(pat) %>% str_remove_all(pat2) %>% trimws()

dfOpportunity$local.authority <- dfOpportunity$local.authority %>% str_remove_all(pat) %>% str_remove_all(pat2) %>% trimws()

dfOpportunity$local.authority %in% sfLA$LAD22NM #  some still don't match

# join

# plot LAs
ggplot()+
  geom_sf(sfLA, mapping = aes(fill = LAD22NM))+
  #scale_fill_brewer(type = "seq", palette = 2)+
  #labs(fill = "Appropriateness")+
  theme_minimal()

### read in regions of provenance ----------------------------------------------

RoP_poly <- st_read(paste0(dirData,"Forest_Reproductive_Materials_Regions_Of_Provenance_GB.shp"))

summary(RoP_poly)

ggplot()+
  geom_sf(RoP_poly, mapping = aes(fill = as.factor(SEED_ZONES)))+
  labs(fill = "Native seed zones")+
  theme_minimal()

