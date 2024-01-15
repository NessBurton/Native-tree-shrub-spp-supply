
# date: 15-01-24
# author: VB
# purpose: Explore options for presenting creation data.

### working dirs --------------------------------------------------------------------------------------

wd <- "~/Documents/Woodland-Trust/Data-Analysis/Project-SOWT2"
dirData <- paste0(wd,"/data-raw/")
dirScratch <- paste0(wd,"/data-scratch/")
dirOut <- paste0(wd,"data-out")

### libraries -----------------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

### read in data --------------------------------------------------------------------------------------

# downloaded from https://www.forestresearch.gov.uk/tools-and-resources/statistics/data-downloads/
# on 15-01-2024

data_area <- read.csv(paste0(dirData,""))

### have a look ---------------------------------------------------------------------------------------


