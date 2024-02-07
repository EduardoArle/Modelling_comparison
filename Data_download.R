#load packages
library(rgbif); library(CoordinateCleaner); library(rnaturalearth)
library(sf); library(raster)

#list working directories
wd_range_mammals <-  "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Range_maps"

#download occurrence data
cat_occ <- occ_search(scientificName = 'Prionailurus bengalensis',
                      limit = 2e+05, 
                      hasCoordinate = TRUE)
cat_occ <- as.data.frame(cat_occ[3])

plant_occ <- occ_search(scientificName = 'Zamia prasina',
                        limit = 2e+05, 
                        hasCoordinate = TRUE)

plant_occ <- as.data.frame(plant_occ[3])

#rename columns to remove the 'data.'
names(cat_occ) <- gsub('data.', '', names(cat_occ))
names(plant_occ) <- gsub('data.', '', names(plant_occ))

#check if the GBIF data is presence only or if there are absences (both are presence-only)
unique(cat_occ$occurrenceStatus)
unique(plant_occ$occurrenceStatus)

#use coordinate cleaner to flag likely errors in the data
cat_occ_coordClean <- clean_coordinates(cat_occ,
                                   tests = c("capitals", "centroids", "equal",
                                             "gbif", "institutions", "outliers",
                                             "seas", "zeros", "duplicates"))

plant_occ_coordClean <- clean_coordinates(plant_occ,
                                     tests = c("capitals", "centroids", "equal",
                                               "gbif", "institutions", "outliers",
                                               "seas", "zeros", "duplicates"))


#make spatial objects of the points
cat_occ_sf <- st_as_sf(cat_occ_coordClean,
                         coords = c('decimalLongitude', 'decimalLatitude'),
                         crs = crs(world))

plant_occ_sf <- st_as_sf(plant_occ_coordClean,
                         coords = c('decimalLongitude', 'decimalLatitude'),
                         crs = crs(world))

#obtain world map
world <- ne_countries(returnclass = "sf", scale = 'large')

#plot raw data 
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf, add= T, pch = 21, bg = 'orange', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#1369 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf, add= T, pch = 21, bg = 'orange', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#207 records 

#plot records flagged by coordinateCleaner
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.summary == FALSE,],
     add= T, pch = 21, bg = 'red', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#502 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.summary == FALSE,],
     add= T, pch = 21, bg = 'red', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#99 records

#plot records flagged for having validity
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.val == FALSE,],
     add= T, pch = 21, bg = 'grey30', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#0 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.val == FALSE,],
     add= T, pch = 21, bg = 'grey30', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#0 records

#plot records flagged for having equal coordinates
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.equ == FALSE,],
     add= T, pch = 21, bg = 'grey20', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#0 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.equ == FALSE,],
     add= T, pch = 21, bg = 'grey20', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#0 records

#plot records flagged for having coordinates with 0s as values
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.zer == FALSE,],
     add= T, pch = 21, bg = 'grey10', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#0 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.zer == FALSE,],
     add= T, pch = 21, bg = 'grey10', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#0 records

#plot records flagged for being close to capitals (10000 metres)
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.cap == FALSE,],
     add= T, pch = 21, bg = 'blue', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#10 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.cap == FALSE,],
     add= T, pch = 21, bg = 'blue', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#6 records

#plot records flagged for being close to country centroids (1000 metres)
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.cen == FALSE,],
     add= T, pch = 21, bg = 'magenta', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#1 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.cen == FALSE,],
     add= T, pch = 21, bg = 'magenta', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#0 records

#plot records that fall into the ocean.
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.sea == FALSE,],
     add= T, pch = 21, bg = 'lightgreen', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#62 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.sea == FALSE,],
     add= T, pch = 21, bg = 'lightgreen', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#1 record

#plot records that look like outliers.
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.otl == FALSE,],
     add= T, pch = 21, bg = 'cyan', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#4 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.otl == FALSE,],
     add= T, pch = 21, bg = 'cyan', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#8 record

#plot records within 1 degree radius around the GBIF headquarters
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.gbf == FALSE,],
     add= T, pch = 21, bg = 'grey90', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#0 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.gbf == FALSE,],
     add= T, pch = 21, bg = 'grey90', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#0 record

#plot records flagged for being close to known biodiversity institutions (100 metres)
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.inst == FALSE,],
     add= T, pch = 21, bg = 'yellow', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#0 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.inst == FALSE,],
     add= T, pch = 21, bg = 'yellow', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#1 record

#plot records flagged for being duplicated records
plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(cat_occ_sf[cat_occ_sf$.dpl == FALSE,],
     add= T, pch = 21, bg = 'purple', col = 'black', cex = 0.6)
text(0, 101, 'Prionailurus bengalensis', font = 3, cex = 3)
#492 records

plot(st_geometry(world), bg = 'azure2', col = 'khaki2', border = NA)
plot(plant_occ_sf[plant_occ_sf$.dpl == FALSE,],
     add= T, pch = 21, bg = 'purple', col = 'black', cex = 0.6)
text(0, 101, 'Zamia prasina', font = 3, cex = 3)
#89 record

#visualise categories that flagged points to decide to remove or keep
tail(names(cat_occ_clean),11)
tail(names(plant_occ_clean),11)
