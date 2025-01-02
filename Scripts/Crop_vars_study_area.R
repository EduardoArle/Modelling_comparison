#load packages
library(sf); library(terra)

#list WDs
wd_bioclim <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"
wd_landuse <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Land_cover_layers"
wd_raw_vars <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Raw'
wd_shp <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Shapefiles"

wd_vars_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/Present'
wd_vars_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/Present'

wd_vars_cat_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2041-2060_HadGEM3_RCP4.5'
wd_vars_plant_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2041-2060_HadGEM3_RCP4.5'

wd_vars_cat_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2041-2060_HadGEM3_RCP8.5'
wd_vars_plant_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2041-2060_HadGEM3_RCP8.5'

wd_vars_cat_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2081-2100_HadGEM3_RCP4.5'
wd_vars_plant_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2081-2100_HadGEM3_RCP4.5'

wd_vars_cat_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2081-2100_HadGEM3_RCP8.5'
wd_vars_plant_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2081-2100_HadGEM3_RCP8.5'


#load study area shps
study_area_cat <- st_read('Prionailurus bengalensis_study_area', dsn = wd_shp)
study_area_plant <- st_read('Zamia prasina_study_area', dsn = wd_shp)



###################
##### PRESENT #####
###################



#load variables
setwd(wd_bioclim)

vars <- lapply(list.files(pattern = '.bil'), rast)
names(vars) <- gsub('.bil', '', list.files(pattern = '.bil'))

setwd(wd_landuse)
lu <- rast('global_LULC_2015_RGS84.tif')

#make a list 
all_vars <- c(vars,lu)

#mask, crop and save variables by study area
setwd(wd_vars_cat_pr)
crop_vars_cat <- list() #to check
for(i in 1:length(all_vars))
{
  mask_vars_cat <- mask(all_vars[[i]], study_area_cat)
  crop_vars_cat[[i]] <- crop(mask_vars_cat, study_area_cat)
  writeRaster(crop_vars_cat[[i]],
              paste0(names(crop_vars_cat[[i]]),'_Prionailurus_bengalensis.tif'))
  print(i)
}

setwd(wd_vars_plant_pr)
crop_vars_plant <- list() #to check
for(i in 1:length(all_vars))
{
  mask_vars_plant <- mask(all_vars[[i]], study_area_plant)
  crop_vars_plant[[i]] <- crop(mask_vars_plant, study_area_plant)
  writeRaster(crop_vars_plant[[i]],
              paste0(names(crop_vars_plant[[i]]),'_Zamia_prasina.tif'))
  print(i)
}



#################################
##### 2041-2060 HadGEM3 4.5 #####
#################################



#load variables 2041-2060, HadGEM3, RCP 4.5
setwd(wd_raw_vars)

vars_had_4.5_2050 <- rast('wc2.1_2.5m_bioc_HadGEM3-GC31-LL_ssp245_2041-2060.tif')


names(vars_had_4.5_2050) <- c('bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6',
                              'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12',
                              'bio13', 'bio14', 'bio15', 'bio16', 'bio17',
                              'bio18', 'bio19')

setwd(wd_landuse)
lu_4.5_2050 <- rast('global_SSP2_RCP45_2050_RGS84.tif')

#fix extent issue (lu ended up not having Antarctica)
vars_had_4.5_2050 <- crop(vars_had_4.5_2050, lu_4.5_2050)

#make a list 
all_vars <- c(vars_had_4.5_2050, lu_4.5_2050) 

################# ATTENTION ###############

###### TEMPERATURE VARIABLES IN THE FUTURE PROJECTIONS ARE NOT MULTIPLIED PER 10 
###### AS IN THE CURRENT VARIABLES I USED IN THE MODELS, NEED TO MULTIPLY FUTURE
###### MULTIPLY VALUES OF VARS c(bio1, bio2, bio4, bio5, bio6, bio7, bio8, bio9,
###### bio10, bio11)  PER 10

################# ATTENTION ###############


#mask, crop and save variables by study area
setwd(wd_vars_cat_had_4.5_2050)
crop_vars_cat <- list() #to check
for(i in 1:nlyr(all_vars))
{
  mask_vars_cat <- mask(all_vars[[i]], study_area_cat)
  crop_vars_cat[[i]] <- crop(mask_vars_cat, study_area_cat)
  
  #multiply temp layers per 10
  if(i < 12 & i != 3){
    crop_vars_cat[[i]] <- crop_vars_cat[[i]] * 10
  }
  
  writeRaster(crop_vars_cat[[i]],
              paste0(names(crop_vars_cat[[i]]),'_Prionailurus_bengalensis.tif'))
  print(i)
}

setwd(wd_vars_plant_had_4.5_2050)
crop_vars_plant <- list() #to check
for(i in 1:nlyr(all_vars))
{
  mask_vars_plant <- mask(all_vars[[i]], study_area_plant)
  crop_vars_plant[[i]] <- crop(mask_vars_plant, study_area_plant)
  
  #multiply temp layers per 10
  if(i < 12 & i != 3){
    crop_vars_plant[[i]] <- crop_vars_plant[[i]] * 10
  }
  
  writeRaster(crop_vars_plant[[i]],
              paste0(names(crop_vars_plant[[i]]),'_Zamia_prasina.tif'))
  print(i)
}



#################################
##### 2041-2060 HadGEM3 8.5 #####
#################################



#load variables 2041-2060, HadGEM3, RCP 8.5
setwd(wd_raw_vars)

vars_had_8.5_2050 <- rast('wc2.1_2.5m_bioc_HadGEM3-GC31-LL_ssp585_2041-2060.tif')


names(vars_had_8.5_2050) <- c('bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6',
                              'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12',
                              'bio13', 'bio14', 'bio15', 'bio16', 'bio17',
                              'bio18', 'bio19')

setwd(wd_landuse)
lu_8.5_2050 <- rast('global_SSP5_RCP85_2050_RGS84.tif')

#fix extent issue (lu ended up not having Antarctica)
vars_had_8.5_2050 <- crop(vars_had_8.5_2050, lu_8.5_2050)

#make a list 
all_vars <- c(vars_had_8.5_2050, lu_8.5_2050) 

################# ATTENTION ###############

###### TEMPERATURE VARIABLES IN THE FUTURE PROJECTIONS ARE NOT MULTIPLIED PER 10 
###### AS IN THE CURRENT VARIABLES I USED IN THE MODELS, NEED TO MULTIPLY FUTURE
###### MULTIPLY VALUES OF VARS c(bio1, bio2, bio4, bio5, bio6, bio7, bio8, bio9,
###### bio10, bio11)  PER 10

################# ATTENTION ###############


#mask, crop and save variables by study area
setwd(wd_vars_cat_had_8.5_2050)
crop_vars_cat <- list() #to check
for(i in 1:nlyr(all_vars))
{
  mask_vars_cat <- mask(all_vars[[i]], study_area_cat)
  crop_vars_cat[[i]] <- crop(mask_vars_cat, study_area_cat)
  
  #multiply temp layers per 10
  if(i < 12 & i != 3){
    crop_vars_cat[[i]] <- crop_vars_cat[[i]] * 10
  }
  
  writeRaster(crop_vars_cat[[i]],
              paste0(names(crop_vars_cat[[i]]),'_Prionailurus_bengalensis.tif'))
  print(i)
}

setwd(wd_vars_plant_had_8.5_2050)
crop_vars_plant <- list() #to check
for(i in 1:nlyr(all_vars))
{
  mask_vars_plant <- mask(all_vars[[i]], study_area_plant)
  crop_vars_plant[[i]] <- crop(mask_vars_plant, study_area_plant)
  
  #multiply temp layers per 10
  if(i < 12 & i != 3){
    crop_vars_plant[[i]] <- crop_vars_plant[[i]] * 10
  }
  
  writeRaster(crop_vars_plant[[i]],
              paste0(names(crop_vars_plant[[i]]),'_Zamia_prasina.tif'))
  print(i)
}



#################################
##### 2081-2100 HadGEM3 4.5 #####
#################################



#load variables 2081-2100, HadGEM3, RCP 4.5
setwd(wd_raw_vars)

vars_had_4.5_2090 <- rast('wc2.1_2.5m_bioc_HadGEM3-GC31-LL_ssp245_2081-2100.tif')
  

names(vars_had_4.5_2090) <- c('bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6',
                              'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12',
                              'bio13', 'bio14', 'bio15', 'bio16', 'bio17',
                              'bio18', 'bio19')

setwd(wd_landuse)
lu_4.5_2090 <- rast('global_SSP2_RCP45_2090_RGS84.tif')

#fix extent issue (lu ended up not having Antarctica)
vars_had_4.5_2090 <- crop(vars_had_4.5_2090, lu_4.5_2090)

#make a list 
all_vars <- c(vars_had_4.5_2090, lu_4.5_2090) 

################# ATTENTION ###############

###### TEMPERATURE VARIABLES IN THE FUTURE PROJECTIONS ARE NOT MULTIPLIED PER 10 
###### AS IN THE CURRENT VARIABLES I USED IN THE MODELS, NEED TO MULTIPLY FUTURE
###### MULTIPLY VALUES OF VARS c(bio1, bio2, bio4, bio5, bio6, bio7, bio8, bio9,
###### bio10, bio11)  PER 10

################# ATTENTION ###############


#mask, crop and save variables by study area
setwd(wd_vars_cat_had_4.5_2090)
crop_vars_cat <- list() #to check
for(i in 1:nlyr(all_vars))
{
  mask_vars_cat <- mask(all_vars[[i]], study_area_cat)
  crop_vars_cat[[i]] <- crop(mask_vars_cat, study_area_cat)
  
  #multiply temp layers per 10
  if(i < 12 & i != 3){
    crop_vars_cat[[i]] <- crop_vars_cat[[i]] * 10
  }
  
  writeRaster(crop_vars_cat[[i]],
              paste0(names(crop_vars_cat[[i]]),'_Prionailurus_bengalensis.tif'))
  print(i)
}

setwd(wd_vars_plant_had_4.5_2090)
crop_vars_plant <- list() #to check
for(i in 1:nlyr(all_vars))
{
  mask_vars_plant <- mask(all_vars[[i]], study_area_plant)
  crop_vars_plant[[i]] <- crop(mask_vars_plant, study_area_plant)
  
  #multiply temp layers per 10
  if(i < 12 & i != 3){
    crop_vars_plant[[i]] <- crop_vars_plant[[i]] * 10
  }
  
  writeRaster(crop_vars_plant[[i]],
              paste0(names(crop_vars_plant[[i]]),'_Zamia_prasina.tif'))
  print(i)
}



#################################
##### 2081-2100 HadGEM3 8.5 #####
#################################



#load variables 2081-2100, HadGEM3, RCP 8.5
setwd(wd_raw_vars)

vars_had_8.5_2090 <- rast('wc2.1_2.5m_bioc_HadGEM3-GC31-LL_ssp585_2081-2100.tif')


names(vars_had_8.5_2090) <- c('bio1', 'bio2', 'bio3', 'bio4', 'bio5', 'bio6',
                              'bio7', 'bio8', 'bio9', 'bio10', 'bio11', 'bio12',
                              'bio13', 'bio14', 'bio15', 'bio16', 'bio17',
                              'bio18', 'bio19')

setwd(wd_landuse)
lu_8.5_2090 <- rast('global_SSP5_RCP85_2090_RGS84.tif')

#fix extent issue (lu ended up not having Antarctica)
vars_had_8.5_2090 <- crop(vars_had_8.5_2090, lu_8.5_2090)

#make a list 
all_vars <- c(vars_had_8.5_2090, lu_8.5_2090) 

################# ATTENTION ###############

###### TEMPERATURE VARIABLES IN THE FUTURE PROJECTIONS ARE NOT MULTIPLIED PER 10 
###### AS IN THE CURRENT VARIABLES I USED IN THE MODELS, NEED TO MULTIPLY FUTURE
###### MULTIPLY VALUES OF VARS c(bio1, bio2, bio4, bio5, bio6, bio7, bio8, bio9,
###### bio10, bio11)  PER 10

################# ATTENTION ###############


#mask, crop and save variables by study area
setwd(wd_vars_cat_had_8.5_2090)
crop_vars_cat <- list() #to check
for(i in 1:nlyr(all_vars))
{
  mask_vars_cat <- mask(all_vars[[i]], study_area_cat)
  crop_vars_cat[[i]] <- crop(mask_vars_cat, study_area_cat)
  
  #multiply temp layers per 10
  if(i < 12 & i != 3){
    crop_vars_cat[[i]] <- crop_vars_cat[[i]] * 10
  }
  
  writeRaster(crop_vars_cat[[i]],
              paste0(names(crop_vars_cat[[i]]),'_Prionailurus_bengalensis.tif'))
  print(i)
}

setwd(wd_vars_plant_had_8.5_2090)
crop_vars_plant <- list() #to check
for(i in 1:nlyr(all_vars))
{
  mask_vars_plant <- mask(all_vars[[i]], study_area_plant)
  crop_vars_plant[[i]] <- crop(mask_vars_plant, study_area_plant)
  
  #multiply temp layers per 10
  if(i < 12 & i != 3){
    crop_vars_plant[[i]] <- crop_vars_plant[[i]] * 10
  }
  
  writeRaster(crop_vars_plant[[i]],
              paste0(names(crop_vars_plant[[i]]),'_Zamia_prasina.tif'))
  print(i)
}