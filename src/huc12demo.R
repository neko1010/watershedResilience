library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scico)
library(ggspatial)

setwd("~/BSU/MRRMAid/watershedResilience/")

## read the shp
huc12metrics = st_read("./output/huc12metricSPEI.shp")

sage = st_read("./data/sagebrushBiome.shp")
states = st_read("./data/tl_2024_us_state.shp")
westList = c("WA", "OR", "CA", "NV", "NM", "UT", "ID", "MT", "CO", "WY",  "AZ")
#westList = c("WA", "OR", "CA", "NV", "NM", "UT", "ID", "MT", "CO", "WY", "NE", "ND", "SD", "AZ")
west = states %>%
  filter(STUSPS %in% westList)

## HUC8 for fig
study8 = '1706020'

huc12metrics8 = huc12metrics %>%
  mutate(huc8 = substr(as.character(huc12), 1, 8)) %>%
  filter(huc8 == '17060206')



ggIntact = ggplot(data = huc12metrics8) +
  geom_sf(aes(fill = meanTS)) +
  scico::scale_fill_scico(palette = "vik", direction = -1) +
  ggtitle("A")+
  labs(x = "Longitude", y = "Latitude")
ggIntact

ggVar = ggplot(data = huc12metrics8) +
  geom_sf(aes(fill = CV)) +
  scico::scale_fill_scico(palette = "vik") +
  ggtitle("B")+
  labs(x = "Longitude", y = "Latitude")
ggVar

ggCorr = ggplot(data = huc12metrics8) +
  geom_sf(aes(fill = hucCor)) +
  scico::scale_fill_scico(palette = "vik") +
  ggtitle("C")+
  labs(x = "Longitude", y = "Latitude")
ggCorr

centroid = st_centroid(huc12metrics8)

ggLoc = ggplot(west) +
  geom_sf()+
  geom_sf(data = sage, aes(color = "black"), fill = NA, lwd = 2, show.legend = "line")+
  geom_sf(data = centroid, aes(color = 'red'), show.legend = "line")+
  scale_color_manual(name = "", values = c( "black", "red"),  labels = c("Sagebrush biome", "Example HUC8"))+
  ggtitle("D")+
  labs(x = "Longitude", y = "Latitude")+
  #annotation_scale() +
  annotation_north_arrow(height=unit(0.55, "cm"), width=unit(0.55, "cm"),
                         pad_y = unit(0.7, "cm"),
                         which_north="true",
                         style = north_arrow_orienteering(text_size = 8))
ggLoc
#ggarrange(ggFed, ggUSFS, ggBLM, ggAg, ggTrend, ggNDRI, nrow = 2, ncol = 3)

#ggarrange(ggIntact, ggVar, ggCorr, ggLoc, nrow = 2, ncol = 2)
ggarrange(ggIntact, ggVar, ggCorr, ggLoc,nrow = 2, ncol = 2)
