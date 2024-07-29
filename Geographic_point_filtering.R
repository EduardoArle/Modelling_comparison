#load packages
library(sf); library(terra)

#list working directories
wd_range_mammals <-  "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Range_maps"
wd_clean_points <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Point_data"
wd_pseudo_abs <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Data_pseudo_absence"

#load species range maps
cat_range <- st_read(dsn = wd_range_mammals , layer = 'Prionailurus bengalensis')

#load clean points species
setwd(wd_clean_points)
cat_points <- read.csv('Prionailurus bengalensis_clean.csv')
plant_points <- read.csv('Zamia prasina_clean.csv')

#make sf objects 
cat_sf <- st_as_sf(cat_points, 
                   coords = c('decimalLongitude', 'decimalLatitude'),
                   crs = crs(cat_range))

plant_sf <- st_as_sf(plant_points, 
                     coords = c('decimalLongitude', 'decimalLatitude'), 
                     crs = crs(cat_range))

#select only cat points within the species range
cat_pts_range <- vapply(st_intersects(cat_sf,cat_range), 
                           function(x) if (length(x)==0) NA_integer_ else x[1],
                           FUN.VALUE = 1)

cat_pts_sel <- cat_sf[!is.na(cat_pts_range),]



#visualise
plot(st_geometry(cat_range), border = NA, col = '#805080')
plot(st_geometry(cat_pts_sel), add = T, pch = 19, cex = 0.4, col = '#50FFFF')


###### Pseudo Absence must be created taking biases in consideration ######

# for that, I will use data on mammal species (excluding the focal species)
# within the study area to draw points as pseudo-absence 
# according to article Phillips et al. 2009


#load data for pseudo-absence generation
setwd(wd_pseudo_abs)
pa_cat <- read.csv('Mammals.csv', sep = '\t')


head(pa_cat)
