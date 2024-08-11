#load packages
library(sf); library(terra)

#list WDs
wd_bioclim <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"
wd_landuse <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Land_cover_layers"
wd_shp <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Shapefiles"
wd_vars_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/Present'
wd_vars_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/Present'

#load variables
setwd(wd_bioclim)

vars <- lapply(list.files(pattern = '.bil'), rast)
names(vars) <- gsub('.bil', '', list.files(pattern = '.bil'))

setwd(wd_landuse)
lu <- rast('global_LULC_2015_RGS84.tif')

#make a list 
all_vars <- c(vars,lu)

#load study area shps
study_area_cat <- st_read('Prionailurus bengalensis_study_area', dsn = wd_shp)
study_area_plant <- st_read('Zamia prasina_study_area', dsn = wd_shp)

#mask, crop and save variables by study area
setwd(wd_vars_cat_pr)
crop_vars_cat <- list()
for(i in 1:length(all_vars))
{
  mask_vars_cat <- mask(all_vars[[i]], study_area_cat)
  crop_vars_cat[[i]] <- crop(mask_vars_cat, study_area_cat)
  writeRaster(crop_vars_cat[[i]],
              paste0(names(crop_vars_cat[[i]]),'_Prionailurus_bengalensis.tif'))
  print(i)
}

setwd(wd_vars_plant_pr)
crop_vars_plant <- list()
for(i in 1:length(all_vars))
{
  mask_vars_plant <- mask(all_vars[[i]], study_area_plant)
  crop_vars_plant[[i]] <- crop(mask_vars_plant, study_area_plant)
  writeRaster(crop_vars_plant[[i]],
              paste0(names(crop_vars_plant[[i]]),'_Zamia_prasina.tif'))
  print(i)
}




