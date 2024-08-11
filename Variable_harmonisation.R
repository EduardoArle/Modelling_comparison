#load packages
library(terra)

#list WDs
wd_bioclim <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"
wd_landuse <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Land_cover_layers"

#load one of the bioclim vars
setwd(wd_bioclim)
bio1 <- rast('bio1.bil')

#load landuse var
setwd(wd_landuse)
lu <- rast('global_LULC_2015.tif')

#change raster projection
lu_RGS84 <- project(lu, bio1, method = 'near')

#check if we can stack
stack_test <- c(bio1, lu_RGS84)

#save raster
setwd(wd_landuse)
writeRaster(lu_RGS84, 'global_LULC_2015_RGS84.tif')

