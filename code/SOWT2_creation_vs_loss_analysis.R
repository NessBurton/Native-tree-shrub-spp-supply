
# date: 15-01-24
# author: VB
# purpose: Explore options for presenting creation data.

### working dirs ---------------------------------------------------------------

#wd <- "~/Documents/Woodland-Trust/Data-Analysis/Project-SOWT2" # MacBook path
wd <- "C:/Users/vbu/OneDrive - the Woodland Trust/Projects/CO&E - SoWT2/Project-SoWT2" # WT laptop path
dirData <- paste0(wd,"/data-raw/")
dirScratch <- paste0(wd,"/data-scratch/")
dirOut <- paste0(wd,"data-out")

### libraries ------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)

### read in data ---------------------------------------------------------------

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


### area first - have a look ---------------------------------------------------

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

### wrangle --------------------------------------------------------------------

# convert to long format
data_area_long <- gather(data_area, ownership, thousand.ha, private.sector.cf:uk.total, factor_key = T)

# separate conifer/broadleaf into new var
data_area_plot <- data_area_long %>% 
  filter(ownership != "uk.total") %>% 
  separate(ownership, into = c("sector","delete","woodland.type")) %>% 
  mutate(woodland.type.full = ifelse(woodland.type == "cf", "conifer","broadleaf"),
         delete = NULL)

### plot it --------------------------------------------------------------------

# labels for facets
ownership.labs <- c("Private sector - conifer", "Public sector - conifer", "Private sector - broadleaf", "Public sector - broadleaf")
names(ownership.labs) <- c("private.sector.cf", "public.sector.cf", "private.sector.bf", "public.sector.bf")

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
  #ylim(c(0,1000))+
  #xlim(c(1998,2023))+
  theme_grey()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "Avenir"),
        axis.title.x = element_text(vjust = 0.5),
        axis.title.y = element_text(vjust = 0.5),
        legend.title = element_blank())

### now creation data ----------------------------------------------------------

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

### wrangle --------------------------------------------------------------------

# convert to long format
data_creation_long <- gather(data_creation, ownership, thousand.ha, private.sector.cf:total, factor_key = T)
data_creation_long


### plot -----------------------------------------------------------------------
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

### now re-stock data ----------------------------------------------------------

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

### wrangle --------------------------------------------------------------------

# convert to long format
data_restock_long <- gather(data_restock, ownership, thousand.ha, private.sector.cf:total, factor_key = T)
data_restock_long


### plot -----------------------------------------------------------------------

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
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "Arial Narrow"),
        axis.title.x = element_text(vjust = 0.5),
        axis.title.y = element_text(vjust = 0.5),
        legend.title = element_blank())



### plan -----------------------------------------------------------------------

# what do I want to show?
# essentially want to show woodland area & creation (woodland stock)
# to woodland loss (with restock taken account of)
# by country
# in thousand ha units

# read in data that i wrangled by hand from forestry stats xls

df_forestry_stats <- read.csv(paste0(dirScratch,"forestry_stats_2023_creation-restock_wrangled.csv"),
                                header = TRUE)
head(df_forestry_stats)
summary(df_forestry_stats)

### plot -----------------------------------------------------------------------

df_FS_long <- df_forestry_stats %>% 
  gather(., forest.stat, t.ha, area.t.ha:restock.t.ha) #%>% 
  #mutate(year = as.factor(year))

summary(df_FS_long)

df_FS_long %>% 
  filter(forest.stat == "area.t.ha" & country == "England" & sector == "Public") %>% 
  ggplot()+
  geom_area(aes(year,t.ha, fill = woodland.type))
  
df_FS_long %>% 
  filter(forest.stat == "creation.t.ha") %>% 
  ggplot()+
  geom_area(aes(year,t.ha, fill = woodland.type), na.rm = T)+
  facet_grid(country~sector)

df_FS_long %>% 
  filter(forest.stat == "area.t.ha") %>% 
  ggplot()+
  geom_area(aes(year,t.ha, fill = woodland.type), na.rm = T)+
  facet_grid(country~sector)

df_FS_long %>% 
  filter(forest.stat == "restock.t.ha") %>% 
  ggplot()+
  geom_area(aes(year,t.ha, fill = woodland.type), na.rm = T)+
  facet_grid(country~sector)


### now add in loss to compare -------------------------------------------------

# loss/change data
# https://www.globalforestwatch.org/dashboards/country/GBR/?location=WyJjb3VudHJ5IiwiR0JSIl0%3D
df_loss <- read.csv(paste0(dirData, "GFW_UK_tree_cover_loss_subnational.csv"))

head(df_loss)
summary(df_loss)
colnames(df_loss)

### wrangle --------------------------------------------------------------------

# convert to long format
df_loss_long <- gather(df_loss, year, tc.loss.ha, tc_loss_ha_2001:tc_loss_ha_2022) #, factor_key = T)
df_loss_long
summary(df_loss_long)

df_loss_long <- tidyr::separate(data = df_loss_long, year, into = c("delete1","delete2","delete3", "year"))
summary(df_loss_long)

# use mutate, change year to numeric and remove un-needed vars
df_loss_long <- df_loss_long %>% 
  mutate(Country = subnational1,
         subnational1 = NULL,
         year = as.numeric(df_loss_long$year),
         delete1 = NULL,
         delete2 = NULL,
         delete3 = NULL)

### plot -----------------------------------------------------------------------

# loss is recorded for different thresholds of canopy cover, so facet by these
df_loss_long %>% 
  ggplot()+
  geom_area(aes(year,tc.loss.ha, fill = Country))+
  ggtitle("Woodland loss over time, 2001 - 2022")+
  facet_wrap(~threshold)

# or, show variation by threshold
df_loss_long %>% 
  ggplot()+
  geom_boxplot(aes(as.factor(year),tc.loss.ha))+
  ggtitle("Woodland loss over time, 2001 - 2022")+
  facet_wrap(~Country)+
  theme_grey()

df_loss_long %>% 
  ggplot()+
  geom_line(aes(year,tc.loss.ha, colour = as.factor(threshold)))+
  ggtitle("Woodland loss over time, 2001 - 2022")+
  facet_wrap(~Country)+
  theme_grey()

# filter to just 30% canopy cover threshold
#df_loss_30 <- df_loss_long %>% 
  #filter(., threshold == 30) %>% 
  #mutate(country = NULL,
         #country = Country,
         #Country = NULL)

# doesn't split by woodland.type, or sector, so need to remove that from df_FS_long before joining
df_FS_wide <- pivot_wider(df_FS_long, names_from = c(woodland.type, sector), values_from = t.ha) %>% 
  mutate(tot.t.ha = Conifer_Private + Broadleaf_Private + Conifer_Public + Broadleaf_Public)

# join & convert ha to t.ha
# it's to do with the join type here - sort tomorrow
df_all <- left_join(df_FS_wide, df_loss_long, by = c("year","country")) %>% 
  mutate(#threshold = NULL,
         area_ha = NULL,
         extent_2000_ha = NULL,
         extent_2010_ha = NULL,
         gain_2000.2020_ha = NULL,
         tc.loss.ha = tc.loss.ha/1000)

# compare area against creation, against restock, against loss

df_all <- df_all %>% 
  mutate(Conifer_Private = NULL,
         Broadleaf_Private = NULL,
         Conifer_Public = NULL,
         Broadleaf_Public = NULL) %>% 
  pivot_wider(., names_from = forest.stat, values_from = tot.t.ha) %>% 
  pivot_longer(., cols = c('tc.loss.ha','restock.t.ha','area.t.ha','creation.t.ha'), names_to = "forest.stat", values_to = "t.ha") %>% 
  # tidy up names
  mutate(stat.new = ifelse(forest.stat == 'tc.loss.ha', "loss",
                              ifelse(forest.stat == 'area.t.ha', "existing",
                                     ifelse(forest.stat == 'creation.t.ha', "created",
                                            ifelse(forest.stat == 'restock.t.ha', "restocked", NA)))))

# look at existing area and creation data together
df_all %>% 
  filter(., stat.new == 'existing' | stat.new == 'created') %>% 
  ggplot()+
  geom_area(aes(year,t.ha, fill = country))+
  facet_grid(stat.new~country, scales = 'free')

# filter 
test <- filter(df_all, stat.new == "loss")

# then compare restock stats against total loss
df_all %>% 
  filter(., stat.new == 'loss' | stat.new == 'restocked') %>% 
  ggplot()+
  geom_line(aes(year,t.ha, colour = stat.new, linetype = stat.new))+
  scale_linetype_manual(values = c("twodash","solid"))+
  scale_color_manual(values=c('#999999','#E69F00'))+
  #scale_color_brewer(palette = "Dark2")+
  xlab("Year")+ylab("Area (thousand ha)")+
  facet_wrap(~country)+
  theme_bw()

# what I actually want to do is compare restock & creation against overall loss - 
# and a range of potential loss due to the different data set which is calculated at different thresholds of uncertainty

ggplot()+
  geom_col(data = df_all %>% filter(., stat.new == 'restocked' | stat.new == 'created'),aes(year,t.ha, fill = stat.new), position = 'stack')+
  geom_line(data = df_all %>% filter(., stat.new == 'loss'), aes(year,t.ha), colour = "black", size = 1)+
  #scale_fill_manual(values=c('#999999','#E69F00'))+
  scale_fill_brewer(palette = "Dark2")+
  xlab("Year")+ylab("Area (thousand ha)")+
  ggtitle("Area of woodland restocked & created, vs. total loss")+
  facet_wrap(~country)+
  theme_bw()+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10,0,10,0), family = "Calibri"),
        axis.title.x = element_text(vjust = 0.5),
        axis.title.y = element_text(vjust = 0.5),
        legend.title = element_blank())

# now I want to show the range of potential loss, not just the 30% canopy cover threshold.

# also add in lines to illustrate annual creation target per country? or do a separate plot for that
