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
cat_points <- read.csv('Prionailurus bengalensis_clean_range_thin.csv')
plant_points <- read.csv('Zamia prasina_clean_chull_thin.csv')

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
                   crs = crs(cat_range))

plant_sf <- st_as_sf(plant_points, 
                     coords = c('decimalLongitude', 'decimalLatitude'), 
                     crs = crs(cat_range))

mammal_sf <- st_as_sf(mammal_points, 
                       coords = c('decimalLongitude', 'decimalLatitude'), 
                       crs = crs(cat_range))

cyca_sf <- st_as_sf(cyca_points, 
                      coords = c('decimalLongitude', 'decimalLatitude'), 
                      crs = crs(cat_range))

###### Pseudo Absence must be created taking biases in consideration ######

# for that, I will use data on mammal and cyca species (excluding the focal species)
# within the study area to draw points as pseudo-absence 
# according to article Phillips et al. 2009

#select only pseudo-absence points within the species range (with a buffer)
mammal_pts_range <- vapply(st_intersects(mammal_sf,cat_range_buf_100), 
                        function(x) if (length(x)==0) NA_integer_ else x[1],
                        FUN.VALUE = 1)

mammal_pts_sel <- mammal_sf[!is.na(mammal_pts_range),]

cyca_pts_chull <- vapply(st_intersects(cyca_sf,plant_chull_buf_100), 
                           function(x) if (length(x)==0) NA_integer_ else x[1],
                           FUN.VALUE = 1)

cyca_pts_sel <- cyca_sf[!is.na(cyca_pts_chull),]


#visualise
plot(st_geometry(cat_range_buf_100), border = NA, col = '#d5d5d5')
plot(st_geometry(cat_pts_sel), add = T, pch = 19, cex = 0.4, col = '#208920')

plot(st_geometry(cat_range_buf_100), border = NA, col = '#d5d5d5')
plot(st_geometry(mammal_pts_sel), add = T, pch = 19, cex = 0.4, col = '#d58920')

plot(st_geometry(plant_chull_buf_100), border = NA, col = '#d5d5d5')
plot(st_geometry(plant_pts_sel), add = T, pch = 19, cex = 0.4, col = '#208920')

plot(st_geometry(plant_chull_buf_100), border = NA, col = '#d5d5d5')
plot(st_geometry(cyca_pts_sel), add = T, pch = 19, cex = 0.4, col = '#d58920')

#load variable layer to thin points 
setwd(wd_vars)
bio1 <- rast('bio1.bil')

#make ID raster
ID_raster <- bio1
ID_raster[] <- c(1:ncell(ID_raster))

#get ID_cell for all points
ID_cell <- as.data.table(extract(ID_raster, mammal_pts_sel))
mammal_pts_sel$ID <- ID_cell$ID
mammal_pts_sel$ID_cell <- ID_cell$bio1

ID_cell_cyca <- as.data.table(extract(ID_raster, cyca_pts_sel))
cyca_pts_sel$ID <- ID_cell_cyca$ID
cyca_pts_sel$ID_cell <- ID_cell_cyca$bio1

#keep only one record per cell
ID_cell_2 <- unique(ID_cell, by = 'bio1')
mammal_pts_sel_2 <- mammal_pts_sel[mammal_pts_sel$ID %in% ID_cell_2$ID,]

ID_cell_cyca_2 <- unique(ID_cell_cyca, by = 'bio1')
cyca_pts_sel_2 <- cyca_pts_sel[cyca_pts_sel$ID %in% ID_cell_cyca_2$ID,]

#make a buffer around presences to avoid selection pseudo-absence
cat_pts_buf <- st_buffer(cat_pts_sel, dist = 50000)
plant_pts_buf <- st_buffer(plant_pts_sel, dist = 50000)

#make a spatial polygon object with only one feature
no_pa_cat <- st_union(cat_pts_buf)
no_pa_plant <- st_union(plant_pts_buf)

#for some reason that escapes my understanding, after st_union to create
#'no_pa_cat' , the spatial object still has two features, being the first 
#'#one empty
no_pa_cat <- no_pa_cat[2]

# this fixes possible 'duplicate vertex' errors
no_pa_cat <- st_make_valid(no_pa_cat) 
no_pa_plant <- st_make_valid(no_pa_plant) 

#make a holes in the species range by the small buffer around points
pa_area_cat <- st_difference(cat_range_buf_100, no_pa_cat)
pa_area_cat <- st_union(pa_area_cat)
pa_area_cat <- st_make_valid(pa_area_cat)

pa_area_plant <- st_difference(plant_chull_buf_100, no_pa_plant)
pa_area_plant <- st_union(pa_area_plant)
pa_area_plant <- st_make_valid(pa_area_plant)

#define number of pseudo abs to be created (same as presences)
n_pa <- nrow(cat_pts_sel)
n_pa_plant <- nrow(plant_pts_sel)

#select candidates for pa (other mammal points) within in the pa_area
pa_1 <- vapply(st_intersects(mammal_pts_sel_2, pa_area_cat), 
                           function(x) if (length(x)==0) NA_integer_ else x[1],
                           FUN.VALUE = 1)

pa_2 <- mammal_pts_sel_2[!is.na(pa_1),]

pa_1_plant <- vapply(st_intersects(cyca_pts_sel_2, pa_area_plant), 
               function(x) if (length(x)==0) NA_integer_ else x[1],
               FUN.VALUE = 1)

pa_2_plant <- cyca_pts_sel_2[!is.na(pa_1_plant),]

#randomly select n pa points amongst the occ of other mammals
pa_3 <- pa_2[sample(c(1:nrow(pa_2)), n_pa),]

pa_3_plant <- pa_2_plant[sample(c(1:nrow(pa_2_plant)), n_pa_plant),]


#save
setwd(wd_clean_points)
cat_pts_sel_3 <- as.data.frame(cat_pts_sel_2)
cat_pts_sel_3 <- cat_pts_sel_3[,-which(names(cat_pts_sel_3) == 'geometry')]
write.csv(cat_pts_sel_3,
          'Prionailurus bengalensis_clean_range_thin.csv',
          row.names = F)




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
