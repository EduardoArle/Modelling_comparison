#load packages
library(terra)

#list WDs
wd_bioclim <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"
wd_landuse <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Land_cover_layers"
wd_landuse_4.5 <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Land_cover_layers/SSP2_RCP45"
wd_landuse_8.5 <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Land_cover_layers/SSP5_RCP85"

#load one of the bioclim vars
setwd(wd_bioclim)
bio1 <- rast('bio1.bil')

#load landuse var
setwd(wd_landuse)
lu <- rast('global_LULC_2015.tif')

setwd(wd_landuse_4.5)
lu_4.5_2050 <- rast('global_SSP2_RCP45_2050.tif')
lu_4.5_2090 <- rast('global_SSP2_RCP45_2090.tif')

setwd(wd_landuse_8.5)
lu_8.5_2050 <- rast('global_SSP5_RCP85_2050.tif')
lu_8.5_2090 <- rast('global_SSP5_RCP85_2090.tif')

#change raster projection
lu_RGS84 <- project(lu, bio1, method = 'near')

lu_4.5_2050_RGS84 <- project(lu_4.5_2050, bio1, method = 'near')
lu_4.5_2090_RGS84 <- project(lu_4.5_2090, bio1, method = 'near')

lu_8.5_2050_RGS84 <- project(lu_8.5_2050, bio1, method = 'near')
lu_8.5_2090_RGS84 <- project(lu_8.5_2090, bio1, method = 'near')

#check if we can stack
stack_test <- c(bio1, lu_RGS84, lu_4.5_2050_RGS84, lu_4.5_2090_RGS84,
                lu_8.5_2050_RGS84, lu_8.5_2090_RGS84)

#save raster
setwd(wd_landuse)

writeRaster(lu_RGS84, 'global_LULC_2015_RGS84.tif')

writeRaster(lu_4.5_2050_RGS84, 'global_SSP2_RCP45_2050_RGS84.tif')
writeRaster(lu_4.5_2090_RGS84, 'global_SSP2_RCP45_2090_RGS84.tif')

writeRaster(lu_8.5_2050_RGS84, 'global_SSP5_RCP85_2050_RGS84.tif')
writeRaster(lu_8.5_2090_RGS84, 'global_SSP5_RCP85_2090_RGS84.tif')

