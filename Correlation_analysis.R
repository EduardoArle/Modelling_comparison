#load packages
library(sf); library(terra); library(usdm); library(rnaturalearth)

#list WDs
wd_clean_points <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Point_data"
wd_bioclim <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"

#obtain world map
world <- ne_countries(returnclass = "sf", scale = 'large')

#load variables
setwd(wd_bioclim)

vars <- lapply(list.files(pattern = '.bil'), rast)
names(vars) <- gsub('.bil', '', list.files(pattern = '.bil'))

#make a raster stack (not the smartest solution...)
vars_stack <-c(vars[[1]], vars[[2]], vars[[3]], vars[[4]], vars[[5]], vars[[6]],
               vars[[7]], vars[[8]], vars[[9]], vars[[10]], vars[[11]],
               vars[[12]], vars[[13]], vars[[14]], vars[[15]], vars[[16]],
               vars[[17]], vars[[18]], vars[[19]])

#load points
setwd(wd_clean_points)

cat_pres <- read.csv('Prionailurus bengalensis_clean_thin.csv')
plant_pres <- read.csv('Zamia prasina_clean_thin.csv')

cat_pa <- read.csv('Prionailurus bengalensis_pseudoabsence.csv')
plant_pa <- read.csv('Zamia prasina_pseudoabsence.csv')

## Join presences and pseudo-absences ##

#harmonise tables
cat_pres2 <- data.frame(occurrence = 1,
                        decimalLongitude = cat_pres$decimalLongitude,
                        decimalLatitude = cat_pres$decimalLatitude)

cat_pa2 <- data.frame(occurrence = 0,
                      decimalLongitude = cat_pa$decimalLongitude,
                      decimalLatitude = cat_pa$decimalLatitude)

plant_pres2 <- data.frame(occurrence = 1,
                          decimalLongitude = plant_pres$decimalLongitude,
                          decimalLatitude = plant_pres$decimalLatitude)

plant_pa2 <- data.frame(occurrence = 0,
                        decimalLongitude = plant_pa$decimalLongitude,
                        decimalLatitude = plant_pa$decimalLatitude)

#join tables
cat_occ <- rbind(cat_pres2, cat_pa2)
plant_occ <- rbind(plant_pres2, plant_pa2)

#make sf objects 
cat_occ_sf <- st_as_sf(cat_occ, 
                       coords = c('decimalLongitude', 'decimalLatitude'),
                       crs = crs(world))

plant_occ_sf <- st_as_sf(plant_occ, 
                         coords = c('decimalLongitude', 'decimalLatitude'), 
                         crs = crs(world))

#extract variable values in all point locations
cat_vars <- extract(vars_stack, cat_occ_sf)
plant_vars <- extract(vars_stack, plant_occ_sf)

#calculate the VIF to select less correlated variables
cat_VIF <- vifcor(cat_vars[,-1], th = 0.7)         #[,-1] ID col
plant_VIF <- vifcor(plant_vars[,-1], th = 0.7)     #[,-1] ID col


#print screen results and save in a folder



