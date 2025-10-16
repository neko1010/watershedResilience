library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(trend)

### Process
setwd("~/BSU/MRRMAid/watershedResilience")
## read data 
huc12 = read.csv("./data/huc12narrowNFWeco500.csv")
huc12eco = read.csv("./data/huc12ecoregions30.csv")

## Pivot wider
huc12.wider = huc12 %>% pivot_wider(id_cols = c('huc12'), ## 18,861 obs
                                    names_from = 'date', values_from = 'mesicProp')

## Add ecoregions
huc12ecoCode = data.frame(huc12eco$huc12, huc12eco$min)
colnames(huc12ecoCode) = c("huc12", "min")


## get single year
huc12_201606 = huc12 %>%
  filter(date == '201606')

huc12.wider = left_join(huc12.wider, huc12_201606, by = "huc12")

huc12.wider = left_join(huc12.wider, huc12ecoCode, by = "huc12") ## remove later?
colnames(huc12.wider)

## SD
mrrmaid = huc12.wider %>% 
  select(contains(c("huc12", "min","20")))
colnames(mrrmaid)


## If June mean is lower than July mean, rm June from the SD calculation

junes = mrrmaid %>%
  select(ends_with((c("06")))) 

juneMean = rowMeans(junes)

julys = mrrmaid %>%
  select(ends_with((c("07")))) 

julyMean = rowMeans(julys)

mrrmaidNoJune = mrrmaid %>%
  select(-ends_with(c("06", "09"))) ## remove September here too - these are energy limited system

## add intactness columns - august if energy limited, september otherwise

mrrmaid = cbind(mrrmaid, juneMean, julyMean) %>%
  mutate(SD = if_else(julyMean>juneMean,
           apply(mrrmaid[,3:length(colnames(mrrmaidNoJune))], 1, sd),
           apply(mrrmaid[,3:length(colnames(mrrmaid))], 1, sd))) %>%
  mutate(meanTS = if_else(julyMean>juneMean,
                           apply(mrrmaid[,3:length(colnames(mrrmaidNoJune))], 1, mean),
                           apply(mrrmaid[,3:length(colnames(mrrmaid))], 1, mean))) %>%
  mutate(CV = SD/meanTS)


ecoLookup = c('Northwestern Glaciated Plains', 'Middle Rockies', 'Idaho Batholith',
              'Northwestern Great Plains', 'Snake River Plain', 'Eastern Cascades Slopes and Foothills', 
              'Columbia Plateau', 'Cascades','Sierra Nevada', 'Southern Rockies', 'Central Basin and Range', 
              'Mojave Basin and Range', 'Arizona/New Mexico Mountains', 'Arizona/New Mexico Plateau',
              'Wasatch and Uinta Mountains', 'Northern Basin and Range', 'Blue Mountains',
              'Colorado Plateaus', 'Wyoming Basin','High Plains')

ecoCode = c(42, 17, 16, 43, 12, 9, 10, 4, 5, 21, 13, 14, 23, 22, 19, 80, 11, 20, 18, 25)


dfEco = cbind.data.frame(ecoCode, ecoLookup)
colnames(dfEco) = c('min', 'ecoregion')

mrrmaidOut = left_join(mrrmaid, dfEco, by = "min")
## rm N cascades (77) and N Rockies (15)
mrrmaidOut = mrrmaidOut %>%
  filter(min != 15) %>%
  filter(min != 77)

#mrrmaidOut = na.omit(mrrmaidOut)




### multipanel dists
#dist =  ggplot(sepsOut, aes(x =sepTrend))+
#  geom_histogram(aes(x = sepTrend))+
#  facet_wrap(vars(ecoregion))
#dist

###### sensitivity to climate ####

spei = read.csv("./data/huc12spei1y1k.csv")
## Pivot wider
spei.wider = spei %>% pivot_wider(id_cols = c('huc12'), ## 18,861 obs
                                    names_from = 'date', values_from = 'spei1y')

speiNoJune = spei.wider %>%
  select(-ends_with(c("06", "09"))) ## remove June/September - these are energy limited system

## mesic
mesic = mrrmaidOut %>% 
  filter(juneMean>julyMean)

## rm the nas
mesic = na.omit(mesic)

## just the values
mesicVals = mesic[, 3:38]

## fill the gaps
#mesicVals = replace_na_with_mean(mesicVals)

mesicEL = mrrmaidOut %>% 
  filter(juneMean<julyMean)%>%
  select(-ends_with(c("06", "09"))) ## remove June/September - these are energy limited system

## just the values
mesicValsEL = mesicEL[3:20]


## rm NA
mesicEL = na.omit(mesicEL)

## fill the gaps
#mesicValsEL = replace_na_with_mean(mesicValsEL)
## OR remove NA
mesicValsEL = na.omit(mesicValsEL)

## spei values
spei.wider = spei.wider%>%
  filter(huc12 %in% mesic$huc12)

speiEL = speiNoJune %>% 
  filter(huc12 %in% mesicEL$huc12)

## only the values
speiVals = spei.wider[,2:length(colnames(spei.wider))]

speiValsEL = speiEL[,2:length(colnames(speiEL))]


#A <- as.matrix(mesicVals)
#B <- as.matrix(speiVals)
A <- as.matrix(mesicVals)
B <- as.matrix(speiVals)
#hucCor = sapply(seq.int(dim(A)[1]), function(i) cor(A[i,], B[i,]))
hucCor = sapply(seq.int(dim(A)[1]), function(i) cor(A[i,], B[i,], method = "spearman", use = "na.or.complete" ))

## energy limited
Ael <- as.matrix(mesicValsEL)
Bel <- as.matrix(speiValsEL)
hucCorEL = sapply(seq.int(dim(Ael)[1]), function(i) cor(Ael[i,], Bel[i,], method = "spearman", use = "na.or.complete" ))

## append corr vals to df with other metrics
mesicOut = cbind(mesic %>% 
                select(contains(c("huc12","ecoregion", "SD", "CV", "meanTS"))),
              hucCor)

mesicELOut = cbind(mesicEL %>% 
                select(contains(c("huc12","ecoregion", "SD", "CV", "meanTS"))),
              hucCorEL)

## rename for the rbind
mesicELOut = mesicELOut %>%
  rename(hucCor = hucCorEL)

metrics = rbind(mesicOut, mesicELOut)
## add a column for CV that scales with the others (1 max)
metrics = metrics %>%
    mutate(zCV6 = CV/6)## use z so it appears last in sequence
 

########### PLOTS ################

ecoCount = ggplot(metrics, aes(x = ecoregion))+
  geom_bar(aes(y = after_stat(count)))+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  stat_count(binwidth = 1, 
             geom = 'text', 
             color = 'black', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 0.5))+
  ylab("huc12 Count")+
  xlab("Ecoregion")

#sdPlot

ecoCount

histogram_data <- layer_data(ecoCount)
#get_data(ecoCount)


## write table for counts
write.table(table(metrics$ecoregion), file = "./output/counts.csv")
ggCorr = ggplot(data = metrics, aes(x=ecoregion, y=hucCor)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  xlab("Ecoregion")+
  ylab("Correlation")
#scale_y_continuous(sec.axis = sec_axis(~ . * .10, name = "Trend"))
ggCorr

## Pivot longer for multiple boxes

metricsLonger = metrics %>% pivot_longer(-c(huc12, ecoregion, SD, CV))


ggMetrics = ggplot(data = metricsLonger, aes(x=ecoregion, y=value, fill = name)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  xlab("Ecoregion")+
  ylab("Sensitivity/Intactness")+
  scale_fill_discrete("Legend", labels = c("Sensitivity", "Intactness", "Variability")) +
  labs(fill = "Indicator") +
  scale_y_continuous(sec.axis = sec_axis(~ . *6, name = "Variability"))
ggMetrics

## association bt CV and Corr
#ggVarCor = ggplot(data = metrics, aes(x=sqrt(CV), y=hucCor))+#, color = ecoregion)) +
ggVarCor = ggplot(data = metrics, aes(x=CV, y=hucCor))+#, color = ecoregion)) +
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(vars(ecoregion))+
  xlab("Variability")+
  ylab("Sensitivity")
ggVarCor

## association bt CV and Corr
ggVarIntact = ggplot(data = metrics, aes(x=CV, y=meanTS))+#, color = ecoregion)) +
  geom_point()+
  geom_smooth(method = "loess")+
  facet_wrap(vars(ecoregion))+
  #theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  xlab("Variability")+
  ylab("Intactness")
#scale_y_continuous(sec.axis = sec_axis(~ . * .10, name = "Trend"))
ggVarIntact

## association bt intact and Corr
ggCorIntact = ggplot(data = metrics, aes(x=hucCor, y=meanTS))+#, color = ecoregion)) +
  geom_point()+
  #geom_smooth()+
  geom_smooth(method = lm)+
  facet_wrap(vars(ecoregion))+
  #theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  xlab("Sensitivity")+
  ylab("Intactness")
ggCorIntact

## association bt intact and Corr
ggIntactHIST = ggplot(data = metrics, aes(x=meanTS))+#, color = ecoregion)) +
  geom_histogram()+
  facet_wrap(vars(ecoregion))+
  xlab("Intactness")
ggIntactHIST

## correlation
whiskers_centers_corr <- metrics %>%
  group_by(ecoregion) %>%
  summarise(lower_whisker = max(min(hucCor, na.rm = T), quantile(hucCor, 0.25, na.rm = T) - 1.58 * IQR(hucCor, na.rm = T), na.rm = T), 
            mean = mean(hucCor, na.rm = T),
            median = median(hucCor, na.rm = T),
            upper_whisker = min(max(hucCor, na.rm = T), quantile(hucCor, 0.75, na.rm = T) + 1.58 * IQR(hucCor, na.rm = T), na.rm = T))
whiskers_centers_corr

## round to 5 digits
corr_round = round(whiskers_centers_corr[,2:length(colnames(whiskers_centers_corr))],6)
corr_out = cbind(whiskers_centers_corr[,1], corr_round)
colnames(corr_out)
colnames(corr_out) = c('Ecoregion', 'Lower 95%', "Mean", "Median", "Upper 95%")
write.table(corr_out,file = "./output/correlation.csv")

## Intactness
whiskers_centers_intact <- metrics %>%
  group_by(ecoregion) %>%
  summarise(lower_whisker = max(min(meanTS, na.rm = T), quantile(meanTS, 0.25, na.rm = T) - 1.58 * IQR(meanTS, na.rm = T), na.rm = T), 
            mean = mean(meanTS, na.rm = T),
            median = median(meanTS, na.rm = T),
            upper_whisker = min(max(meanTS, na.rm = T), quantile(meanTS, 0.75, na.rm = T) + 1.58 * IQR(meanTS, na.rm = T), na.rm = T))
whiskers_centers_intact

## round to 5 digits
intact_round = round(whiskers_centers_intact[,2:length(colnames(whiskers_centers_intact))],6)
intact_out = cbind(whiskers_centers_intact[,1], intact_round)
colnames(intact_out)
colnames(intact_out) = c('Ecoregion', 'Lower 95%', "Mean", "Median", "Upper 95%")
write.table(intact_out,file = "./output/intact.csv")

## Variability
whiskers_centers_CV <- metrics %>%
  group_by(ecoregion) %>%
  summarise(lower_whisker = max(min(CV, na.rm = T), quantile(CV, 0.25, na.rm = T) - 1.58 * IQR(CV, na.rm = T), na.rm = T), 
            mean = mean(CV, na.rm = T),
            median = median(CV, na.rm = T),
            upper_whisker = min(max(CV, na.rm = T), quantile(CV, 0.75, na.rm = T) + 1.58 * IQR(CV, na.rm = T), na.rm = T))
whiskers_centers_CV

## round to 5 digits
CV_round = round(whiskers_centers_CV[,2:length(colnames(whiskers_centers_CV))],6)
CV_out = cbind(whiskers_centers_CV[,1], CV_round)
colnames(CV_out)
colnames(CV_out) = c('Ecoregion', 'Lower 95%', "Mean", "Median", "Upper 95%")
write.table(CV_out,file = "./output/CV.csv")

### add the .geo property from empty hucs
tempHUC = st_read("./data/huc12.shp") %>%
  select(c('huc12', 'geometry'))

## convert to numeric
hucNum = as.numeric(tempHUC$huc12)
emptyHUC = data.frame(hucNum, tempHUC$geometry)
colnames(emptyHUC) = colnames(tempHUC)

hucJoin = left_join(metrics, emptyHUC, by = 'huc12')
st_write(hucJoin, "./output/huc12metricSPEI.shp")
#st_write(hucJoin, "./output/huc12metricsFill.shp")

