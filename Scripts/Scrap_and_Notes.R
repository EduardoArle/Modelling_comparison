####################################
############. SCRAP .###############
####################################

#making a second object to deal with sf package issues...
world_2 <- st_make_valid(world)

#list the countries where the species occur
countries_cat <- unique(vapply(st_intersects(cat_occ_sf,world_2),
                               function(x) if (length(x)==0) NA_integer_ else x[1],
                               FUN.VALUE = 1))

countries_plant <- unique(vapply(st_intersects(plant_occ_sf,world_2),
                               function(x) if (length(x)==0) NA_integer_ else x[1],
                               FUN.VALUE = 1))

#get rid of NA in the country list (points on ocean)
if(length(which(is.na(countries_cat))) == 1){
  countries_cat <- countries_cat[-which(is.na(countries_cat))]
}

if(length(which(is.na(countries_plant))) == 1){
  countries_plant <- countries_plant[-which(is.na(countries_plant))]
}

#crop the world map based on the countries where the species is
coords_CP_cat <- extent(cat_occ_sf)

CP_cat <- st_polygon(list(cbind(
  c(coords_CP_cat[1],coords_CP_cat[2],coords_CP_cat[2],coords_CP_cat[1],coords_CP_cat[1]),
  c(coords_CP_cat[3],coords_CP_cat[3],coords_CP_cat[4],coords_CP_cat[4],coords_CP_cat[3]))))

CP_cat <- st_sfc(CP_cat, crs = crs(world))
CP_cat <- st_as_sf(CP_cat)

map_cat <- suppressWarnings(st_intersection(world_2, CP_cat))




#### func plotOcc from bRacatus (not working properly with the sf adjustments)

##### I NEED TO FIX THIS BLOODY PACKAGE #####

function(occ, regional = TRUE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  world <- ne_countries(returnclass = "sf", scale = 'large')
  world <- st_make_valid(world)
  
  occ_sf <- st_as_sf(occ, coords = c('decimalLongitude', 'decimalLatitude'),
                     crs = crs(world))
  
  if(regional){
    
    countries <- unique(vapply(st_intersects(occ_sf,world), 
                               function(x) if (length(x)==0) NA_integer_ else x[1],
                               FUN.VALUE = 1))
    
    if(length(which(is.na(countries))) == 1){
      countries <- countries[-which(is.na(countries))]
    }
    
    countries <- world[countries,]
    
    coords_CP <- extent(countries)
    
    CP <- st_polygon(list(cbind(
      c(coords_CP[1],coords_CP[2],coords_CP[2],coords_CP[1],coords_CP[1]),
      c(coords_CP[3],coords_CP[3],coords_CP[4],coords_CP[4],coords_CP[3]))))
    
    CP <- st_sfc(CP, crs = crs(world))
    CP <- st_as_sf(CP)
    
    map <- suppressWarnings(st_intersection(world, CP))
    
  } else {
    
    map <- world
    
  }
  
  par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  plot(st_geometry(map), col = "khaki", bg = "azure2",
       main = unique(occ_sf$species), font.main = 3)
  
  plot(st_geometry(occ_sf), add = T, pch = 21, cex = 1, bg = 'red') 
}