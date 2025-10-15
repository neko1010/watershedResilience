
# area adjusted accuracy assessment 
# calculated as in Olofsson et al. 2014

library(xtable)
library(sf)

setwd("~/BSU/MRRMAid/sageBiome/src")

## list files
val.files = list.files("../data/validation/")

names.ext = sapply(val.files, strsplit, "\\.")
names = sapply(names.ext, function(x){return(x[1])})
names = unique(names)

## get yearmonths
yearmonths.split = sapply(names, strsplit, "l")
yearmonths = sapply(yearmonths.split, function(x){return(x[2])})
yearmonths = na.omit(yearmonths)

## function to perform acc assessment and save tables
area_adjust_acc = function(yearmonth){
  
  ##validation shapefile
  val = st_read(paste0("../data/validation/val", yearmonth, ".shp"))
  ##area dict
  area = read.csv(paste0("../data/areas/areas", yearmonth, ".csv"))

  ## reassign snow and shadow (4,5) to other (1)
 snow.shadow = subset(val, classifica > 3)
 if (length(snow.shadow$classifica) > 0) {
   snow.shadow$classifica <- 1}
 
 all.valid = subset(val, classifica < 4)
 
 ## merge the dfs back
 validation = rbind(all.valid, snow.shadow)
 
 ## create regular accuracy matrix (map (rows), reference (cols))
 confmat <- table(as.factor(validation$classifica),as.factor(validation$class))
 confmat
 
 ## name cols and rows
 rownames(confmat) <- c('Upland', 'Mesic', 'Water')
 colnames(confmat) <- c('Upland', 'Mesic', 'Water')
 
 ## convert area dicts from m² to km²
 other = as.numeric(area[[1]])/1000000
 mesic = as.numeric(area[[2]])/1000000
 water = as.numeric(area[[3]])/1000000
 
 sum = sum(other, mesic, water)
 
 nclass <- length(unique(validation$class))
 classes <- c(1,2,3)
 
 ### START HERE ###
 maparea = cbind(other, mesic, water)
 
 # set confidence interval
 conf <- 1.96
 
 # total  map area
 A <- sum(maparea)
 # proportion of area mapped as class i
 W_i <- as.vector(maparea / A)
 # number of map points per class
 n_i <- rowSums(confmat) 
 
 # population error matrix (Eq.4)
 p <- W_i * confmat / n_i
 p[is.na(p)] <- 0
 p[is.infinite(p)] <- 0
 
 # area estimation
 p_area <- colSums(p) * A
 
 # area estimation confidence interval (Eq.10)
 p_area_CI <- conf * A * sqrt(colSums((W_i * p - p ^ 2) / (n_i - 1))) 
 
 # overall accuracy (Eq.1)
 OA <- sum(diag(p))
 # producers accuracy (Eq.2)
 PA <- diag(p) / colSums(p)
 
 ## swap NaNs for zeroes
 PA[is.na(PA)] <- 0
 
 # users accuracy (Eq.3)
 UA <- diag(p) / rowSums(p)
 
 ## swap NaNs for zeroes
 UA[is.na(UA)] <- 0
 
 # overall accuracy confidence interval (Eq.5)
 OA_CI <- conf * sqrt(sum(W_i ^ 2 * UA * (1 - UA) / (n_i - 1)))
 # user accuracy confidence interval (Eq.6)
 UA_CI <- conf * sqrt(UA * (1 - UA) / (n_i - 1)) 
 # producer accuracy confidence interval (Eq.7)
 N_j <- sapply(1:nclass, function(x) sum(maparea / n_i * confmat[ , x]) )
 tmp <- sapply(1:nclass, function(x) sum(maparea[-x] ^ 2 * confmat[-x, x] / n_i[-x] * ( 1 - confmat[-x, x] / n_i[-x]) / (n_i[-x] - 1)) )
 
 PA_CI <- conf * sqrt(1 / N_j ^ 2 * (maparea ^ 2 * ( 1 - PA ) ^ 2 * UA * (1 - UA) / (n_i - 1) + PA ^ 2 * tmp))
 ## swap NaNs for zeroes
 PA_CI[is.na(PA_CI)] <- 0
 
 # gather results
 result <- matrix(c(p_area, p_area_CI, PA * 100, PA_CI * 100, UA * 100, UA_CI * 100, c(OA * 100, rep(NA, nclass-1)), c(OA_CI * 100, rep(NA, nclass-1))), nrow = nclass)
 result <- round(result, digits = 2) 
 #rownames(result) <- levels(as.factor(train$classes))
 rownames(result) <- c( 'Upland','Mesic','Water') 
 colnames(result) <- c("km²", "km²±", "PA", "PA±", "UA", "UA±", "OA", "OA±")
 class(result) <- "table"
 result
 
 xresult = xtable(result)
 
 print(xresult, file = paste0("../output/acc/acc", yearmonth, ".html"), overwrite = T)
 print(xresult, file = paste0("../output/acc/acc", yearmonth, ".txt"), overwrite = T)
 write.table(result, paste0("../output/acc/acc", yearmonth, ".csv"), col.names = TRUE,
             row.names = TRUE, sep = ",")
 
 ## conf mat
 xconfmat = xtable(confmat)
 
 print(xconfmat, file = paste0("../output/confmat/confmat", yearmonth, ".html"), overwrite = T)
 print(xconfmat, file = paste0("../output/confmat/confmat", yearmonth, ".txt"), overwrite = T)
 write.table(confmat, paste0("../output/confmat/confmat", yearmonth, ".csv"), col.names = TRUE,
             row.names = TRUE, sep = ",")
}

## apply the function
sapply(yearmonths, area_adjust_acc)
