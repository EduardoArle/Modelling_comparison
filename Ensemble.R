#load packages
library(raster)

###############
##### CAT #####
###############

#list WDs
wd_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/Present'

wd_cat_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_HadGEM3_RCP4.5'
wd_cat_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_HadGEM3_RCP8.5'

wd_cat_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_HadGEM3_RCP4.5'
wd_cat_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_HadGEM3_RCP8.5'

wd_cat_mir_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_MIROC6_RCP4.5'
wd_cat_mir_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_MIROC6_RCP8.5'

wd_cat_mir_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_MIROC6_RCP4.5'
wd_cat_mir_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_MIROC6_RCP8.5'

wd_evaluation <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Model_evaluation'
wd_ensemble <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Ensembles'

#load evaluation metrics
setwd(wd_evaluation)
eval_cat <- read.csv('Eval_P_bengalensis.csv')

##### PRESENT #####

#list models with TSS higher than 0.5
cat_sel_TSS <- which(eval_cat$TSS >= 0.5)

#load models
setwd(wd_cat_pr)

#load selected projections
cat_pr <- list()
for(i in 1:length(cat_sel_TSS))
{
  cat_pr[[i]] <- raster(paste0('Pred_P_bengalensis_', cat_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
cat_pr_bin <- list()
for(i in 1:length(cat_pr))
{
  cat_pr_bin[[i]] <- cat_pr[[i]]
  th <- eval_cat$threshold[cat_sel_TSS[i]]
  cat_pr_bin[[i]][] <- ifelse(cat_pr_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
cat_pr_bin <- stack(cat_pr_bin)

#sum all layers and calculate percentage of agreement
cat_pr_ens <- sum(cat_pr_bin) / length(cat_sel_TSS) * 100

#save raster layer
setwd(wd_ensemble)
writeRaster(cat_pr_ens, 'Prionailurus_bengalensis_present.tif')

#################
##### PLANT #####
#################

#list WDs
wd_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/Present'

wd_plant_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_HadGEM3_RCP4.5'
wd_plant_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_HadGEM3_RCP8.5'

wd_plant_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_HadGEM3_RCP4.5'
wd_plant_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_HadGEM3_RCP8.5'

wd_plant_mir_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_MIROC6_RCP4.5'
wd_plant_mir_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_MIROC6_RCP8.5'

wd_plant_mir_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_MIROC6_RCP4.5'
wd_plant_mir_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_MIROC6_RCP8.5'

