#load packages
library(raster); library(sdm)

#list WDs
wd_modelObjects <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/ENMs/modelObjects'
wd_vars_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/Present'
wd_vars_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/Present'
wd_proj_cat_pr <- 

#load model objects
setwd(wd_modelObjects)
sdm_cat <- read.sdm('models_Prionailurus_bengalensis.sdm')
sdm_plant <- read.sdm('models_Zamia_prasina.sdm')

#load variables present
setwd(wd_vars_cat_pr)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_pr)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#project models to present
setwd(wd_res)

# pred_eastern <- predict(sdm_models, newdata = vars2, 
#                         filename = 'Predictions_Eastern_mask_depth.grd')

pred_eastern <- list()
for(i in 1:150)
{
  pred_eastern[[i]] <- predict(sdm_models, w = i, newdata = vars2, 
                               filename = paste0('Predictions_Eastern_mask_depth_', i, '.grd'))
}

pred_eastern <- stack(pred_eastern)
