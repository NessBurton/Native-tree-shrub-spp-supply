
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
library(stringr)

### read in data --------------------------------------------------------------------------------------

# downloaded from https://www.forestresearch.gov.uk/tools-and-resources/statistics/data-downloads/
# on 15-01-2024

data_area <- read.csv(paste0(dirData,"area-timeseries-UK.csv"))

### have a look ---------------------------------------------------------------------------------------

head(data_area)
summary(data_area)
colnames(data_area)
colnames(data_area) <- c("year",
                         "private.sector.cf",
                         "public.sector.cf",
                         "private.sector.bf",
                         "public.sector.bf",
                         "uk.total")

# issue that a couple of variables are reading in as characters

### clean ---------------------------------------------------------------------------------------------

# strip out ","
test <- str_replace_all(data_area$uk.total,",","")
as.numeric(test)

# all good, apply to data
data_area$uk.total <- as.numeric(str_replace_all(data_area$uk.total,",",""))
data_area$private.sector.bf <- as.numeric(str_replace_all(data_area$private.sector.bf,",",""))


### plot it -------------------------------------------------------------------------------------------

data_area %>% 
  ggplot()+
  geom_point(aes(x = year, y = uk.total))

