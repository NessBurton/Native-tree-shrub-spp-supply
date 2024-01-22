
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
library(tidyr)

### read in data --------------------------------------------------------------------------------------

# downloaded from https://www.forestresearch.gov.uk/tools-and-resources/statistics/data-downloads/
# on 15-01-2024
# converted selected spreadsheets to csv format

# data on woodland area
data_area <- read.csv(paste0(dirData,"area-timeseries-UK.csv"))

# planting & restocking data
data_creation <- read.csv(paste0(dirData,"new_planting_type_ownership.csv"))
data_restock <- read.csv(paste0(dirData,"restocking_type_ownership.csv"))

# loss/change data
# https://www.globalforestwatch.org/dashboards/country/GBR/?location=WyJjb3VudHJ5IiwiR0JSIl0%3D
data_loss <- read.csv(paste0(dirData, "GFW_UK_tree_cover_loss_subnational.csv"))


### area first - have a look --------------------------------------------------------------------------

head(data_area)
summary(data_area)
colnames(data_area)
# change column names
colnames(data_area) <- c("year",
                         "private.sector.cf",
                         "public.sector.cf",
                         "private.sector.bf",
                         "public.sector.bf",
                         "uk.total")

# issue that a couple of variables are reading in as characters
# clean - strip out ","
test <- str_replace_all(data_area$uk.total,",","")
# convert to numeric
as.numeric(test)
rm(test)

# all good, apply to data
data_area$uk.total <- as.numeric(str_replace_all(data_area$uk.total,",",""))
data_area$private.sector.bf <- as.numeric(str_replace_all(data_area$private.sector.bf,",",""))
summary(data_area)

### wrangle -------------------------------------------------------------------------------------------

# convert to long format
data_area_long <- gather(data_area, ownership, thousand.ha, private.sector.cf:uk.total, factor_key = T)

# separate conifer/broadleaf into new var
data_area_plot <- data_area_long %>% 
  filter(ownership != "uk.total") %>% 
  separate(ownership, into = c("sector","delete","woodland.type")) %>% 
  mutate(woodland.type.full = ifelse(woodland.type == "cf", "conifer","broadleaf"),
         delete = NULL)

### plot it -------------------------------------------------------------------------------------------

# labels for facets
#ownership.labs <- c("Private sector - conifer", "Public sector - conifer", "Private sector - broadleaf", "Public sector - broadleaf")
#names(ownership.labs) <- c("private.sector.cf", "public.sector.cf", "private.sector.bf", "public.sector.bf")

# plot
data_area_plot %>% 
  #filter(ownership != 'uk.total') %>% # just the country data
  ggplot()+
  #geom_area(aes(x = year, y = thousand.ha, fill = ownership))
  #geom_area(aes(x = year, y = thousand.ha), fill = "chartreuse4")+
  geom_area(aes(x = year, y = thousand.ha, fill = woodland.type.full), na.rm = TRUE)+
  facet_wrap(~sector, ncol = 2)+#,
             #labeller = labeller(ownership = ownership.labs))+
  ggtitle("Woodland area by sector, 1998 to 2023")+
  labs(x = "Year", y = "Area (thousand ha)")+
  ylim(c(0,1000))+
  #xlim(c(1998,2023))+
  theme_grey()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "Avenir"),
        axis.title.x = element_text(vjust = 0.5),
        axis.title.y = element_text(vjust = 0.5),
        legend.title = element_blank())

### now creation data ------------------------------------------------------------------------------

head(data_creation)
summary(data_creation)
colnames(data_creation) <- c("year",
                             "private.sector.cf",
                             "private.sector.bf",
                             "public.sector.cf",
                             "public.sector.bf",
                             "total",
                             "note")
summary(data_creation)

# clean (character to numeric)
data_creation$private.sector.cf <- as.numeric(str_replace_all(data_creation$private.sector.cf,",",""))
data_creation$public.sector.cf <- as.numeric(str_replace_all(data_creation$public.sector.cf,",",""))
data_creation$public.sector.bf <- as.numeric(str_replace_all(data_creation$public.sector.bf,",",""))
summary(data_creation)

### wrangle ---------------------------------------------------------------------------------------

# convert to long format
data_creation_long <- gather(data_creation, ownership, thousand.ha, private.sector.cf:total, factor_key = T)
data_creation_long


### plot ------------------------------------------------------------------------------------------
# labels for facets
ownership.labs <- c("Private sector - conifer", "Public sector - conifer", "Private sector - broadleaf", "Public sector - broadleaf")
names(ownership.labs) <- c("private.sector.cf", "public.sector.cf", "private.sector.bf", "public.sector.bf")

# plot
data_creation_long %>% 
  filter(ownership != 'total') %>% # just the country data
  ggplot()+
  #geom_area(aes(x = year, y = thousand.ha, fill = ownership))
  geom_area(aes(x = year, y = thousand.ha), fill = "chartreuse4")+
  facet_wrap(~ownership, ncol = 2,
             labeller = labeller(ownership = ownership.labs))+
  ggtitle("Woodland creation by sector, 1998 to 2023")+
  labs(x = "Year", y = "Area (thousand ha)")+
  #ylim(c(0,2000))+
  #xlim(c(1998,2023))+
  theme_grey()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "TomsHand"),
        axis.title.x = element_text(vjust = 0.5),
        axis.title.y = element_text(vjust = 0.5),
        legend.title = element_blank())

### now re-stock data ----------------------------------------------------------------------------

head(data_restock)
summary(data_restock)
colnames(data_restock)
colnames(data_restock) <- c("year",
                             "private.sector.cf",
                             "private.sector.bf",
                             "public.sector.cf",
                             "public.sector.bf",
                             "total",
                             "note")
summary(data_restock)

# clean (character to numeric)
data_restock$private.sector.cf <- as.numeric(str_replace_all(data_restock$private.sector.cf,",",""))
data_restock$private.sector.bf <- as.numeric(str_replace_all(data_restock$private.sector.bf,",",""))

summary(data_restock)

### wrangle ---------------------------------------------------------------------------------------

# convert to long format
data_restock_long <- gather(data_restock, ownership, thousand.ha, private.sector.cf:total, factor_key = T)
data_restock_long


### plot ------------------------------------------------------------------------------------------

# plot
data_restock_long %>% 
  filter(ownership != 'total') %>% # just the country data
  ggplot()+
  #geom_area(aes(x = year, y = thousand.ha, fill = ownership))
  geom_area(aes(x = year, y = thousand.ha), fill = "red4")+
  facet_wrap(~ownership, ncol = 2,
             labeller = labeller(ownership = ownership.labs))+
  ggtitle("Woodland restock by sector, 1998 to 2023")+
  labs(x = "Year", y = "Area (thousand ha)")+
  #ylim(c(0,2000))+
  #xlim(c(1998,2023))+
  theme_grey()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "Lato Hairline"),
        axis.title.x = element_text(vjust = 0.5),
        axis.title.y = element_text(vjust = 0.5),
        legend.title = element_blank())
