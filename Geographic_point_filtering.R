#load packages
library(sf); library(terra); 

#list working directories
wd_range_mammals <-  "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Range_maps"
wd_clean_points <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Point_data"

#load species range maps
cat_range <- st_read(dsn = wd_range_mammals , layer = 'Prionailurus bengalensis')

#load clean points species
cat_points <- read.csv('Prionailurus bengalensis_clean.csv')
plant_points <- read.csv('Zamia prasina_clean.csv')

#make sf objects 
cat_sf <- st_as_sf(cat_occ_coordClean, 
                   coords = c('decimalLongitude', 'decimalLatitude'),
                   crs = crs(cat_range))

plant_sf <- st_as_sf(plant_occ_coordClean, 
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

t <- st_intersects(cat_sf,cat_range)

cat_sf_range <- st_intersection(cat_range, cat_sf)

