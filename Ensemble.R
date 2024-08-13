#load packages
library(terra)

###############
##### CAT #####
###############

#list WDs
wd_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/Present'
wd_cat_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_HadGEM3_RCP4.5'

wd_cat_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_HadGEM3_RCP4.5'

#load two projections for each scenario for a visual check
setwd(wd_cat_pr)
cat_pr_brt <- rast('Pred_P_bengalensis_1.grd')
cat_pr_rf <- rast('Pred_P_bengalensis_212.grd')

setwd(wd_cat_had_4.5_2050)
cat_had_4.5_2050_brt <- rast('Pred_P_bengalensis_1.grd')
cat_had_4.5_2050_rf <- rast('Pred_P_bengalensis_212.grd')

setwd(wd_cat_had_4.5_2090)
cat_had_4.5_2090_brt <- rast('Pred_P_bengalensis_1.grd')
cat_had_4.5_2090_rf <- rast('Pred_P_bengalensis_212.grd')

par(mfrow = c(2, 3))
plot(cat_pr_brt, main = 'BRT Present')
plot(cat_had_4.5_2050_brt, main = 'BRT Had 4.5 2050')
plot(cat_had_4.5_2090_brt, main = 'BRT Had 4.5 2090')
plot(cat_pr_rf, main = 'RF Present')
plot(cat_had_4.5_2050_rf, main = 'RF Had 4.5 2050')
plot(cat_had_4.5_2090_rf, main = 'RF Had 4.5 2090')

#################
##### PLANT #####
#################

#list WDs
wd_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/Present'
wd_plant_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_HadGEM3_RCP4.5'

wd_plant_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_HadGEM3_RCP4.5'

#load two projections for each scenario for a visual check
setwd(wd_plant_pr)
plant_pr_brt <- rast('Pred_Z_prasina_1.grd')
plant_pr_rf <- rast('Pred_Z_prasina_212.grd')

setwd(wd_plant_had_4.5_2050)
plant_had_4.5_2050_brt <- rast('Pred_Z_prasina_1.grd')
plant_had_4.5_2050_rf <- rast('Pred_Z_prasina_212.grd')

setwd(wd_plant_had_4.5_2090)
plant_had_4.5_2090_brt <- rast('Pred_Z_prasina_1.grd')
plant_had_4.5_2090_rf <- rast('Pred_Z_prasina_212.grd')

par(mfrow = c(2, 3))
plot(plant_pr_brt, main = 'BRT Present')
plot(plant_had_4.5_2050_brt, main = 'BRT Had 4.5 2050')
plot(plant_had_4.5_2090_brt, main = 'BRT Had 4.5 2090')
plot(plant_pr_rf, main = 'RF Present')
plot(plant_had_4.5_2050_rf, main = 'RF Had 4.5 2050')
plot(plant_had_4.5_2090_rf, main = 'RF Had 4.5 2090')




