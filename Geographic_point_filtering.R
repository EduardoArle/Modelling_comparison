#load packages
library(sf); library(terra); library(data.table); library(rnaturalearth)

#list working directories
wd_range_mammals <-  "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Range_maps"
wd_clean_points <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Point_data"
wd_vars <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"
wd_shp <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Shapefiles"

#obtain world map
world <- ne_countries(returnclass = "sf", scale = 'large')

#load cat's range map to decide whether to use it or a chull
cat_range <- st_read(dsn = wd_range_mammals , layer = 'Prionailurus bengalensis')

#load points after CoordinateCleaner
setwd(wd_clean_points)
cat_points <- read.csv('Prionailurus bengalensis_clean.csv')
plant_points <- read.csv('Zamia prasina_clean.csv')

#make sf objects 
cat_sf <- st_as_sf(cat_points, 
                   coords = c('decimalLongitude', 'decimalLatitude'),
                   crs = crs(world))

plant_sf <- st_as_sf(plant_points, 
                     coords = c('decimalLongitude', 'decimalLatitude'), 
                     crs = crs(world))

#add coordinates columns back to the table
cat_sf <- cbind(cat_sf, cat_points$decimalLongitude, cat_points$decimalLatitude)
plant_sf <- cbind(plant_sf, plant_points$decimalLongitude, plant_points$decimalLatitude)

#fix names of coord columns
names(cat_sf) <- gsub('cat_points.', '', names(cat_sf))
names(plant_sf) <- gsub('plant_points.', '', names(plant_sf))

#### the range map is incomplete, so we will use chull for both species

#make a convex hull for both species based on clean points
chull_cat <- st_convex_hull(st_union(cat_sf))
chull_plant <- st_convex_hull(st_union(plant_sf))

#save the chull shapefiles
st_write(chull_cat, dsn = wd_shp, layer = 'Prionailurus bengalensis_chull',
         driver = 'ESRI Shapefile')
st_write(chull_plant, dsn = wd_shp, layer = 'Zamia prasina_chull',
         driver = 'ESRI Shapefile')

#save plots showing points after CoordinateCleaner on world map
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_sf, add= T, pch = 21, bg = 'orange', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#save width 1000

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_sf, add= T, pch = 21, bg = 'orange', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#save width 1000

#save plot showing range and chulls on world map
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(st_geometry(cat_range), add= T, border = NA, col = 'darkgreen')
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#save width 1000

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(st_geometry(chull_cat), add= T, border = NA, col = 'darkgreen')
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#save width 1000

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(st_geometry(chull_plant), add= T,  border = NA, col = 'darkgreen')
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#save width 1000

#make a buffer around the convex hulls
cat_chull_buf_100 <- st_buffer(chull_cat, dist = 100000)
plant_chull_buf_100 <- st_buffer(chull_plant, dist = 100000)

#crop the buffered area by land
world <- st_make_valid(world)
study_area_cat <- st_intersection(cat_chull_buf_100, world)
study_area_cat <- st_union(study_area_cat)

study_area_plant <- st_intersection(plant_chull_buf_100, world)
study_area_plant <- st_union(study_area_plant)

#save the study area shapefiles
st_write(study_area_cat, dsn = wd_shp, 
         layer = 'Prionailurus bengalensis_study_area',
         driver = 'ESRI Shapefile')
st_write(study_area_plant, dsn = wd_shp,
         layer = 'Zamia prasina_study_area',
         driver = 'ESRI Shapefile')

#save plots showing points after CoordinateCleaner on world map

#first plot the buffered area to set the geographical extent
plot(st_geometry(cat_chull_buf_100), border = NA)
#add world map
plot(st_geometry(world), add = T, col = '#c5c5c5', border = NA)
#add chull with transparency
plot(st_geometry(chull_cat), col = '#06402b40', border = NA, add = T)
#add points that were used for the chull
plot(st_geometry(cat_sf), add= T, pch = 19, cex = 0.3)
#save width 1000

#first plot the buffered area to set the geographical extent
plot(st_geometry(cat_chull_buf_100), border = NA)
#add world map
plot(st_geometry(world), add = T, col = '#c5c5c5', border = NA)
#add buffered chull with transparency
plot(st_geometry(cat_chull_buf_100), col = '#06402b40', border = NA, add = T)
#save width 1000

#first plot the buffered area to set the geographical extent
plot(st_geometry(study_area_cat), border = NA)
#add world map
plot(st_geometry(world), add = T, col = '#c5c5c5', border = NA)
#add buffered chull with transparency
plot(st_geometry(study_area_cat), col = '#06402b40', border = NA, add = T)
#save width 1000


#save plots showing points after CoordinateCleaner on world map

#first plot the buffered area to set the geographical extent
plot(st_geometry(plant_chull_buf_100), border = NA)
#add world map
plot(st_geometry(world), add = T, col = '#c5c5c5', border = NA)
#add chull with transparency
plot(st_geometry(chull_plant), col = '#06402b40', border = NA, add = T)
#add points that were used for the chull
plot(st_geometry(plant_sf), add= T, pch = 19, cex = 0.3)
#save width 1000

#first plot the buffered area to set the geographical extent
plot(st_geometry(plant_chull_buf_100), border = NA)
#add world map
plot(st_geometry(world), add = T, col = '#c5c5c5', border = NA)
#add buffered chull with transparency
plot(st_geometry(plant_chull_buf_100), col = '#06402b40', border = NA, add = T)
#save width 1000

#first plot the buffered area to set the geographical extent
plot(st_geometry(study_area_plant), border = NA)
#add world map
plot(st_geometry(world), add = T, col = '#c5c5c5', border = NA)
#add buffered chull with transparency
plot(st_geometry(study_area_plant), col = '#06402b40', border = NA, add = T)
#save width 1000


#load variable layer to thin points 
setwd(wd_vars)
bio1 <- rast('bio1.bil')

#make ID raster
ID_raster <- bio1
ID_raster[] <- c(1:ncell(ID_raster))

#get ID_cell for all points
ID_cell <- as.data.table(extract(ID_raster, cat_sf))
cat_sf$ID <- ID_cell$ID
cat_sf$ID_cell <- ID_cell$bio1

ID_cell_plant <- as.data.table(extract(ID_raster, plant_sf))
plant_sf$ID <- ID_cell_plant$ID
plant_sf$ID_cell <- ID_cell_plant$bio1

#keep only one record per cell
ID_cell_2 <- unique(ID_cell, by = 'bio1')
cat_sf2 <- cat_sf[cat_sf$ID %in% ID_cell_2$ID,]

ID_cell_plant_2 <- unique(ID_cell_plant, by = 'bio1')
plant_sf2 <- plant_sf[plant_sf$ID %in% ID_cell_plant_2$ID,]

#prepare tables with thinned points
cat_sf3 <- as.data.frame(cat_sf2)
plant_sf3 <- as.data.frame(plant_sf2)

cat_sf3 <- cat_sf3[,-which(names(cat_sf3) == 'geometry')]
plant_sf3 <- plant_sf3[,-which(names(plant_sf3) == 'geometry')]

#save tables with thinned points
setwd(wd_clean_points)

write.csv(cat_sf3,
          'Prionailurus bengalensis_clean_thin.csv',
          row.names = F)
write.csv(plant_sf3,
          'Zamia prasina_clean_thin.csv',
          row.names = F)

#save plots

#first plot the study area to set the geographical extent
plot(st_geometry(study_area_plant), border = NA)
#add world map
plot(st_geometry(world), add = T, col = '#c5c5c5', border = NA)
#add chull with transparency
plot(st_geometry(study_area_plant), col = '#06402b40', border = NA, add = T)
#add points that were used for the chull
plot(st_geometry(plant_sf), add= T, pch = 19, cex = 0.3)

#add points that were used for the chull
plot(st_geometry(plant_pts_sel_2), add= T, pch = 19, cex = 0.3, col = 'red')
#save width 1000


plant_pts_sel_2

plot(st_geometry(cat_range_buf_100), border = NA, col = '#805080')
plot(st_geometry(cat_pts_sel), add = T, pch = 19, cex = 0.4, col = '#FF8000')
plot(st_geometry(cat_pts_sel_2), add = T, pch = 19, cex = 0.4, col = '#00FF00')

plot(st_geometry(plant_chull_buf_100), border = NA, col = '#805080')
plot(st_geometry(plant_pts_sel), add = T, pch = 19, cex = 0.4, col = '#FF8000')
plot(st_geometry(plant_pts_sel_2), add = T, pch = 19, cex = 0.4, col = '#00FF00')





plot(st_geometry(cat_range_buf), border = NA, col = '#805080')
plot(st_geometry(cat_range_buf_40), border = NA, col = '#005080', add = T)
plot(st_geometry(cat_range), border = NA, col = '#80FF80', add = T)


plot(st_geometry(world), col = '#D5D5D5', border = NA)
plot(st_geometry(plant_sf), add = T, pch = 19, cex = 0.4, col = '#208F20')



###############


#visualise
plot(st_geometry(cat_range_buf_100), border = NA, col = '#805080')
plot(st_geometry(cat_pts_sel), add = T, pch = 19, cex = 0.4, col = '#50FFFF')

plot(st_geometry(plant_chull_buf_100), border = NA, col = '#805080')
plot(st_geometry(plant_pts_sel), add = T, pch = 19, cex = 0.4, col = '#50FFFF')

#save
setwd(wd_clean_points)
write.csv(cat_pts_sel, 'Prionailurus bengalensis_clean_range.csv',
          row.names = F)
write.csv(cat_pts_sel, 'Zamia prasina_clean_chull.csv',
          row.names = F)
