#load packages
library(sf); library(terra); library(data.table)

#list working directories
wd_range_mammals <-  "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Range_maps"
wd_clean_points <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Point_data"
wd_vars <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"

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

#add coordinates columns back to the table
cat_sf <- cbind(cat_sf, cat_points$decimalLongitude, cat_points$decimalLatitude)

#fix names of coord columns
names(cat_sf) <- gsub('cat_points.', '', names(cat_sf))

#load cat's range maps
cat_range <- st_read(dsn = wd_range_mammals , layer = 'Prionailurus bengalensis')

#make a convex hull for the plant based on clean points

#make a buffer around the cat's range
cat_range_buf_100 <- st_buffer(cat_range, dist = 100000)

#select only cat points within the species range (with a buffer)
cat_pts_range <- vapply(st_intersects(cat_sf,cat_range_buf_100), 
                           function(x) if (length(x)==0) NA_integer_ else x[1],
                           FUN.VALUE = 1)

#select only plant points within the species convex hull (no range map available)
cat_pts_sel <- cat_sf[!is.na(cat_pts_range),]

#visualise
plot(st_geometry(cat_range_buf_100), border = NA, col = '#805080')
plot(st_geometry(cat_pts_sel), add = T, pch = 19, cex = 0.4, col = '#50FFFF')

#save
setwd(wd_clean_points)
write.csv(cat_pts_sel, 'Prionailurus bengalensis_clean_range.csv',
          row.names = F)

#load variable layer to thin points 
setwd(wd_vars)
bio1 <- rast('bio1.bil')

#make ID raster
ID_raster <- bio1
ID_raster[] <- c(1:ncell(ID_raster))

#get ID_cell for all points
ID_cell <- as.data.table(extract(ID_raster, cat_pts_sel))
cat_pts_sel$ID <- ID_cell$ID
cat_pts_sel$ID_cell <- ID_cell$bio1

#keep only one record per cell
ID_cell_2 <- unique(ID_cell, by = 'bio1')
cat_pts_sel_2 <- cat_pts_sel[cat_pts_sel$ID %in% ID_cell_2$ID,]

#visualise
plot(st_geometry(cat_range_buf_100), border = NA, col = '#805080')
plot(st_geometry(cat_pts_sel), add = T, pch = 19, cex = 0.4, col = '#FF8000')
plot(st_geometry(cat_pts_sel_2), add = T, pch = 19, cex = 0.4, col = '#00FF00')

#save
setwd(wd_clean_points)
cat_pts_sel_3 <- as.data.frame(cat_pts_sel_2)
cat_pts_sel_3 <- cat_pts_sel_3[,-which(names(cat_pts_sel_3) == 'geometry')]
write.csv(cat_pts_sel_3,
          'Prionailurus bengalensis_clean_range_thin.csv',
          row.names = F)



plot(st_geometry(cat_range_buf), border = NA, col = '#805080')
plot(st_geometry(cat_range_buf_40), border = NA, col = '#005080', add = T)
plot(st_geometry(cat_range), border = NA, col = '#80FF80', add = T)


