#load packages
library(sf); library(raster); library(sdm); library(sp)

#list WDs
wd_occ <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Point_data"
wd_bioclim <- "/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5"
wd_landuse <- "/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Land_cover_layers"
wd_sdmData <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/ENMs/sdmData'
wd_modelObjects <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/ENMs/modelObjects'

#load variables
setwd(wd_bioclim)

vars <- lapply(list.files(pattern = '.bil'), raster)
names(vars) <- gsub('.bil', '', list.files(pattern = '.bil'))

setwd(wd_landuse)
lu <- raster('global_LULC_2015_RGS84.tif')

#make raster stacks with the variables for each species
vars_cat <-stack(vars[[12]], vars[[14]], vars[[18]], vars[[5]], vars[[6]],
                 vars[[10]], lu)

vars_plant <-stack(vars[[13]], vars[[15]], vars[[17]], vars[[7]], vars[[10]],
                   lu)

#load occurrences
setwd(wd_occ)

cat_pres <- read.csv("Prionailurus bengalensis_clean_thin.csv")
cat_pa <- read.csv("Prionailurus bengalensis_pseudoabsence.csv")

plant_pres <- read.csv("Zamia prasina_clean_thin.csv")
plant_pa <- read.csv("Zamia prasina_pseudoabsence.csv")

#harmonise the occurrence table with the requirements of the package
cat_pres2 <- data.frame(occurrence = 1,
                        lon = cat_pres$decimalLongitude,
                        lat = cat_pres$decimalLatitude)

cat_pa2 <- data.frame(occurrence = 0,
                      lon = cat_pa$decimalLongitude,
                      lat = cat_pa$decimalLatitude)

plant_pres2 <- data.frame(occurrence = 1,
                          lon = plant_pres$decimalLongitude,
                          lat = plant_pres$decimalLatitude)

plant_pa2 <- data.frame(occurrence = 0,
                        lon = plant_pa$decimalLongitude,
                        lat = plant_pa$decimalLatitude)

#join tables
cat_occ <- rbind(cat_pres2, cat_pa2)
plant_occ <- rbind(plant_pres2, plant_pa2)

#create a spatial points data frame
cat_occ_sp <- cat_occ
plant_occ_sp <- plant_occ

coordinates(cat_occ_sp) <- ~ lon + lat
coordinates(plant_occ_sp) <- ~ lon + lat

#inform the geographic system
proj4string(cat_occ_sp) <- crs(vars_cat)
proj4string(cat_occ_sp) <- crs(vars_plant)

#extract values from all points
vals_cat <- as.data.frame(extract(vars_cat, cat_occ_sp))
vals_plant <- as.data.frame(extract(vars_plant, plant_occ_sp))

#prepare data frame
vals_cat2 <- cbind(cat_occ$occurrence, vals_cat)
vals_plant2 <- cbind(plant_occ$occurrence, vals_plant)

#fix col names
names(vals_cat2)[1] <- 'occurrence'
names(vals_plant2)[1] <- 'occurrence'

#delete rows with NA values in land use variable
vals_cat3 <- vals_cat2[complete.cases(vals_cat2$global_LULC_2015),]
vals_plant3 <- vals_plant2[complete.cases(vals_plant2$global_LULC_2015),]

#categorical variables as factor
vals_cat3$global_LULC_2015 <- as.factor(vals_cat3$global_LULC_2015)
vals_plant3$global_LULC_2015 <- as.factor(vals_plant3$global_LULC_2015)

#prepare data object
data_cat <- sdmData(formmula = occurrence ~ . + coords(lon+lat),
                    train = vals_cat3)

data_plant <- sdmData(formmula = occurrence ~ . + coords(lon+lat),
                      train = vals_plant3)

#save sdmData objects
setwd(wd_sdmData)
write.sdm(data_cat, 'sdmData_Prionailurus_bengalensis')
write.sdm(data_plant, 'sdmData_Zamia_prasina')

#run models
sdm_cat <- sdm(occurrence ~ ., data = data_cat,
               methods = c('brt', 'cart', 'fda', 'glm', 'mars', 'maxlike',
                           'mda', 'gam', 'rf', 'svm'), 
               replication = 'cv', cv.folds = 5, n = 5)

sdm_plant <- sdm(occurrence ~ ., data = data_plant,
               methods = c('brt', 'cart', 'fda', 'glm', 'mars', 'maxlike',
                           'mda', 'gam', 'rf', 'svm'), 
               replication = 'cv', cv.folds = 5, n = 5)

#save model objects
setwd(wd_modelObjects)
write.sdm(sdm_cat, 'models_Prionailurus_bengalensis')
write.sdm(sdm_plant, 'models_Zamia_prasina')


