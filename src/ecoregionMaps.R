library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scico)

setwd("~/BSU/MRRMAid/watershedResilience/")

## read the shp
huc12eco = st_read("./data/ecoregionSage.shp")

## Names for tables
namesIntact= c('index', 'na_l3', 'low95_intact', 'mean_intact', 'median_intact', 'up95_intact')
namesVar= c('index', 'na_l3', 'low95_cv', 'mean_cv', 'median_cv', 'up95_cv')
namesCorr= c('index', 'na_l3', 'low95_corr', 'mean_corr', 'median_corr', 'up95_corr')

## Intactness, variability, and correlation tables
intact = read.csv("./output/intact.csv")
colnames(intact) = namesIntact

var = read.csv("./output/CV.csv")
colnames(var) = namesVar

corr = read.csv("./output/correlation.csv")
colnames(corr) = namesCorr


## Western High Plains renamed to High Plains

intact = intact %>%
  mutate(na_l3name = if_else(na_l3 == "Western High Plains", "High Plains", na_l3))

var = var %>%
  mutate(na_l3name = if_else(na_l3 == "Western High Plains", "High Plains", na_l3))

corr = corr %>%
  mutate(na_l3name = if_else(na_l3 == "Western High Plains", "High Plains", na_l3))

############ Agriculture and pasture by ecoregion ##################

ecoAg = read.csv("./data/sageEcoAg.csv")

## Multiple units per ecoregion - need to weight each 'mean' var by the area relative to the total

ecoregions = unique(ecoAg$na_l3name)
length(ecoregions)

## aggregate the unit means
## sum area/ecoregion

ecoSum = ecoAg %>%
  select(!.geo)%>% ##remove the unwieldy .geo column
  group_by(l3_key)%>%
  mutate(sum_area = sum(shape_area))

## Calculate ecoregion level proportion of agriculture
ecoPropAg = ecoSum %>%
  mutate(ecoProp = shape_area/sum_area) %>%
  mutate(agProp = ecoProp*mean) %>%
  group_by(l3_key) %>%
  mutate(ag_sum = sum(agProp))

## Join with trends and NDRI
ecoJoin = left_join(ecoPropAg, intact, by = 'na_l3name' )
ecoJoin = left_join(ecoJoin, var, by = 'na_l3name' )
ecoJoin = left_join(ecoJoin, corr, by = 'na_l3name' )
ecoJoin = drop_na(ecoJoin) %>%
  select(all_of(c('l3_key', 'na_l3name', 'shape_area', 'sum_area','low95_intact', 'mean_intact', 'median_intact', 'up95_intact', 
                  'low95_cv', 'mean_cv', 'median_cv', 'up95_cv',
                  'low95_corr', 'mean_corr', 'median_corr', 'up95_corr','ecoProp', 'agProp', 'ag_sum')))

################## Land Tenure #####################

fed = read.csv("./data/ecoFed.csv")
usfs = read.csv("./data/ecoUSFS.csv")
blm = read.csv("./data/ecoBLM.csv")

## select relevant columns and rename 'mean'(proportion of polygon of given tenure)
fed =  fed%>%
  select(all_of(c('l3_key', 'shape_area', 'mean')))
colnames(fed) = c('l3_key', 'shape_area', 'meanFED')

usfs =  usfs%>%
  select(all_of(c('l3_key', 'shape_area', 'mean')))
colnames(usfs) = c('l3_key', 'shape_area', 'meanUSFS')

blm =  blm%>%
  select(all_of(c('l3_key', 'shape_area', 'mean')))
colnames(blm) = c('l3_key', 'shape_area', 'meanBLM')

ecoFed = left_join(ecoJoin, fed, by = "shape_area")
ecoUSFS = left_join(ecoFed, usfs, by = "shape_area")
ecoALL = left_join(ecoUSFS, blm, by = "shape_area")

## Calculate ecoregion level proportion of fed 
ecoProp = ecoALL %>%
  mutate(fedProp = ecoProp*meanFED) %>%
  mutate(usfsProp = ecoProp*meanUSFS) %>%
  mutate(blmProp = ecoProp*meanBLM) %>%
  group_by(l3_key.x) %>%
  mutate(fed_sum = sum(fedProp)) %>%
  mutate(usfs_sum = sum(usfsProp)) %>%
  mutate(blm_sum = sum(blmProp))

ecoTable = ecoProp %>%
  select(all_of(c("na_l3name", "ag_sum", "fed_sum", "usfs_sum", "blm_sum")))

ecoTable = unique(ecoTable) ## No Western High Plains?
  
## export a table to included w HUC12 counts
write.table(ecoTable,file = "./output/ecoTable.csv")

ecoSHP = left_join(huc12eco, ecoProp, by = 'shape_area')
ecoSHP = na.omit(ecoSHP)

#plot(ecoSHP["fed_sum"])
#plot(ecoSHP["usfs_sum"])
#plot(ecoSHP["blm_sum"])
#plot(ecoSHP["ag_sum"])
#plot(ecoSHP["mean_ndri"])
#plot(ecoSHP["mean_tr"])

####### Figures ###############

sage = st_read("./data/sagebrushBiome.shp")

ggFed = ggplot(data = ecoSHP) +
  geom_sf(aes(fill = fed_sum)) +
  scale_fill_viridis_c(option = "A") +
  geom_sf(data = sage, color = "white", fill = NA, lwd = 2) +
  ggtitle("Proportion of federal land")+
  labs(x = "Longitude", y = "Latitude")
ggFed

ggUSFS= ggplot(data = ecoSHP) +
  geom_sf(aes(fill = usfs_sum)) +
  scale_fill_viridis_c(option = "A") +
  geom_sf(data = sage, color = "white", fill = NA, lwd = 2) +
  ggtitle("Proportion of USFS land")+
  labs(x = "Longitude", y = "Latitude")

ggBLM= ggplot(data = ecoSHP) +
  geom_sf(aes(fill = blm_sum)) +
  scale_fill_viridis_c(option = "A") +
  geom_sf(data = sage, color = "white", fill = NA, lwd = 2) +
  ggtitle("Proportion of BLM land")+
  labs(x = "Longitude", y = "Latitude")

ggAg= ggplot(data = ecoSHP) +
  geom_sf(aes(fill = ag_sum)) +
  scale_fill_viridis_c(option = "A") +
  geom_sf(data = sage, color = "white", fill = NA, lwd = 2) +
  ggtitle("Proportion of crops/pasture")+
  labs(x = "Longitude", y = "Latitude")

ggIntact= ggplot(data = ecoSHP) +
  geom_sf(aes(fill = median_intact)) +
  scico::scale_fill_scico(palette = "vik", direction = -1) +
  geom_sf(data = sage, color = "black", fill = NA, lwd = 2) +
  #ggtitle("Trend of September mesic vegetation")+
  ggtitle("A")+
  labs(x = "Longitude", y = "Latitude")

ggVar= ggplot(data = ecoSHP) +
  geom_sf(aes(fill = median_cv)) +
  scico::scale_fill_scico(palette = "vik") +
  geom_sf(data = sage, color = "black", fill = NA, lwd = 2) +
  #ggtitle("Trend of September mesic vegetation")+
  ggtitle("B")+
  labs(x = "Longitude", y = "Latitude")

ggCorr= ggplot(data = ecoSHP) +
  geom_sf(aes(fill = median_corr)) +
  scico::scale_fill_scico(palette = "vik") +
  geom_sf(data = sage, color = "black", fill = NA, lwd = 2) +
  #ggtitle("Association of SPEI and mesic vegetation")+
  ggtitle("C")+
  labs(x = "Longitude", y = "Latitude")

states = st_read("./data/tl_2024_us_state.shp")
westList = c("WA", "OR", "CA", "NV", "NM", "UT", "ID", "MT", "CO", "WY", "NE",
             "ND", "SD", "AZ")
west = states %>%
  filter(STUSPS %in% westList)

ggLoc = ggplot(west) +
  geom_sf()+
  geom_sf(data = sage, color = "black", fill = NA, lwd = 2)+
  ggtitle("D")+
  labs(x = "Longitude", y = "Latitude")

#ggarrange(ggFed, ggUSFS, ggBLM, ggAg, ggTrend, ggNDRI, nrow = 2, ncol = 3)

#ggarrange(ggIntact, ggVar, ggCorr, ggLoc, nrow = 2, ncol = 2)
ggarrange(ggIntact, ggVar, ggCorr, nrow = 2, ncol = 2)