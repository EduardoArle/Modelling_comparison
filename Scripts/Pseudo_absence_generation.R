#load packages
library(sf); library(terra); library(data.table)

#list working directories
wd_shp <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Shapefiles"
wd_clean_points <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Point_data"
wd_pseudo_abs <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Data_pseudo_absence"
wd_vars <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"

#load species or study areas
study_area_cat <- st_read(dsn = wd_shp,
                          layer = 'Prionailurus bengalensis_study_area')
study_area_plant <- st_read(dsn = wd_shp,
                       layer = 'Zamia prasina_study_area')

#load points species
setwd(wd_clean_points)
cat_points <- read.csv('Prionailurus bengalensis_clean_thin.csv')
plant_points <- read.csv('Zamia prasina_clean_thin.csv')

#load points for pseudo-absences (Mammalia and Cycadopsida)
setwd(wd_pseudo_abs)
mammal_points <- read.csv('Mammals.csv', sep = '\t')
cyca_points <- read.csv('Cycadopsida.csv', sep = '\t')

#select only points with coordinates
mammal_points <- mammal_points[complete.cases(mammal_points$decimalLatitude),]
mammal_points <- mammal_points[complete.cases(mammal_points$decimalLongitude),]

cyca_points <- cyca_points[complete.cases(cyca_points$decimalLatitude),]
cyca_points <- cyca_points[complete.cases(cyca_points$decimalLongitude),]

#make sf objects 
cat_sf <- st_as_sf(cat_points, 
                   coords = c('decimalLongitude', 'decimalLatitude'),
                   crs = crs(study_area_cat))

plant_sf <- st_as_sf(plant_points, 
                     coords = c('decimalLongitude', 'decimalLatitude'), 
                     crs = crs(study_area_cat))

mammal_sf <- st_as_sf(mammal_points, 
                       coords = c('decimalLongitude', 'decimalLatitude'), 
                       crs = crs(study_area_cat))

cyca_sf <- st_as_sf(cyca_points, 
                      coords = c('decimalLongitude', 'decimalLatitude'), 
                      crs = crs(study_area_cat))

###### Pseudo Absence must be created taking biases in consideration ######

# for that, I will use data on mammal and cyca species (excluding the focal species)
# within the study area to draw points as pseudo-absence 
# according to article Phillips et al. 2009

#select only pseudo-absence points within each study area
mammal_pts_area <- vapply(st_intersects(mammal_sf, study_area_cat), 
                        function(x) if (length(x)==0) NA_integer_ else x[1],
                        FUN.VALUE = 1)

mammal_pa <- mammal_sf[!is.na(mammal_pts_area),]

cyca_pts_area <- vapply(st_intersects(cyca_sf,study_area_plant), 
                           function(x) if (length(x)==0) NA_integer_ else x[1],
                           FUN.VALUE = 1)

cyca_pa <- cyca_sf[!is.na(cyca_pts_area),]


#visualise
plot(st_geometry(study_area_cat), border = NA, col = '#d5d5d5')
plot(st_geometry(cat_sf), add = T, pch = 19, cex = 0.4, col = '#208920')

plot(st_geometry(study_area_cat), border = NA, col = '#d5d5d5')
plot(st_geometry(mammal_pa), add = T, pch = 19, cex = 0.4, col = '#d58920')

plot(st_geometry(study_area_plant), border = NA, col = '#d5d5d5')
plot(st_geometry(plant_sf), add = T, pch = 19, cex = 0.4, col = '#208920')

plot(st_geometry(study_area_plant), border = NA, col = '#d5d5d5')
plot(st_geometry(cyca_pa), add = T, pch = 19, cex = 0.4, col = '#d58920')

#load variable layer to thin points 
setwd(wd_vars)
bio1 <- rast('bio1.bil')

#make ID raster
ID_raster <- bio1
ID_raster[] <- c(1:ncell(ID_raster))

#get ID_cell for all points
ID_cell_mammal <- as.data.table(extract(ID_raster, mammal_pa))
mammal_pa$ID <- ID_cell_mammal$ID
mammal_pa$ID_cell <- ID_cell_mammal$bio1

ID_cell_cyca <- as.data.table(extract(ID_raster, cyca_pa))
cyca_pa$ID <- ID_cell_cyca$ID
cyca_pa$ID_cell <- ID_cell_cyca$bio1

#keep only one record per cell
ID_cell_mammal2 <- unique(ID_cell_mammal, by = 'bio1')
mammal_pa2 <- mammal_pa[mammal_pa$ID %in% ID_cell_mammal2$ID,]

ID_cell_cyca2 <- unique(ID_cell_cyca, by = 'bio1')
cyca_pa2 <- cyca_pa[cyca_pa$ID %in% ID_cell_cyca2$ID,]

#make a 50 km buffer around presences to avoid selection pseudo-absence
cat_sf_buf <- st_buffer(cat_sf, dist = 50000)
plant_sf_buf <- st_buffer(plant_sf, dist = 50000)

#make a spatial polygon object with only one feature
no_pa_cat <- st_union(cat_sf_buf)
no_pa_plant <- st_union(plant_sf_buf)

# this fixes possible 'duplicate vertex' errors
no_pa_cat <- st_make_valid(no_pa_cat) 
no_pa_plant <- st_make_valid(no_pa_plant)

#make a holes in the study areas by the small buffer around points
pa_area_cat <- st_difference(study_area_cat, no_pa_cat)
pa_area_cat <- st_make_valid(pa_area_cat)

pa_area_plant <- st_difference(study_area_plant, no_pa_plant)
pa_area_plant <- st_make_valid(pa_area_plant)

#define number of pseudo abs to be created (same as presences)
n_pa_cat <- nrow(cat_sf)
n_pa_plant <- nrow(plant_sf)

#select candidates for pa (other points same taxon) within in the pa_area
pa_cat1 <- vapply(st_intersects(mammal_pa2, pa_area_cat), 
                           function(x) if (length(x)==0) NA_integer_ else x[1],
                           FUN.VALUE = 1)

pa_cat2 <- mammal_pa2[!is.na(pa_cat1),]

pa_plant1 <- vapply(st_intersects(cyca_pa2, pa_area_plant), 
               function(x) if (length(x)==0) NA_integer_ else x[1],
               FUN.VALUE = 1)

pa_plant2 <- cyca_pa2[!is.na(pa_plant1),]

#randomly select n pa points amongst the occ of other mammals
pa_cat3 <- pa_cat2[sample(c(1:nrow(pa_cat2)), n_pa_cat),]

pa_plant3 <- pa_plant2[sample(c(1:nrow(pa_plant2)), n_pa_plant),] #error

#there are 147 presences, and 138 options for pa, so..
pa_plant3 <- pa_plant2

#visualise
plot(st_geometry(pa_area_cat), col = '#30A530')
plot(pa_cat3, add = T, col = '#000000', bg = 'orange', pch = 21, cex = 0.4)

plot(st_geometry(pa_area_plant), col = '#30A530')
plot(pa_plant3, add = T, col = '#000000', bg = 'orange', pch = 21, cex = 0.4)

#prepare tables to save
coords_cat <- as.data.frame(st_coordinates(pa_cat3)) #get coordinates
coords_plant <- as.data.frame(st_coordinates(pa_plant3)) #get coordinates

names(coords_cat) <- c('decimalLongitude', 'decimalLatitude')
names(coords_plant) <- c('decimalLongitude', 'decimalLatitude')

pa_cat4 <- cbind(as.data.frame(pa_cat3), coords_cat)
pa_plant4 <- cbind(as.data.frame(pa_plant3), coords_plant)

pa_cat4 <- pa_cat4[,-which(names(pa_cat4) == 'geometry')]
pa_plant4 <- pa_plant4[,-which(names(pa_plant4) == 'geometry')]

#save pseudo-absences
setwd(wd_clean_points)

write.csv(pa_cat4,
          'Prionailurus bengalensis_pseudoabsence.csv',
          row.names = F)

write.csv(pa_plant4,
          'Zamia prasina_pseudoabsence.csv',
          row.names = F)


#################################################################

#cat occurrence
plot(st_geometry(cat_range_buf_100), border = NA, col = '#d5d5d5')
plot(cat_pts_sel, add = T, col = '#30A530', pch = 19, cex = 0.5)

#mammal occurrence
plot(st_geometry(cat_range_buf_100), border = NA, col = '#d5d5d5')


mammal_pts_sel_2

plot(st_geometry(pa_area_cat), col = 'orange', add = T)
plot(cat_pts_sel, add = T, col = '#30A530', pch = 19, cex = 0.5)

plot(st_geometry(cat_range_buf_100), border = NA, col = '#d5d5d5')
plot(cat_pts_sel, add = T, col = '#30A530', pch = 19, cex = 0.5)
plot(pa_2, add = T, col = 'orange', pch = 19, cex = 0.5)

plot(st_geometry(cat_range_buf_100), border = NA, col = '#d5d5d5')
plot(cat_pts_sel, add = T, col = '#30A530', pch = 19, cex = 0.5)
plot(pa_3, add = T, col = 'orange', pch = 19, cex = 0.5)



head(pa_cat)
