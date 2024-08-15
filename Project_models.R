#load packages
library(raster); library(sdm)

#list WDs to be used in all projections
wd_modelObjects <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/ENMs/modelObjects'
wd_evaluation <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Model_evaluation'

#load model objects
setwd(wd_modelObjects)
sdm_cat <- read.sdm('models_Prionailurus_bengalensis.sdm')
sdm_plant <- read.sdm('models_Zamia_prasina.sdm')

#get model evaluation
eval_cat <- getEvaluation(sdm_cat, w = 1:250,
                          wtest='test.dep', 
                          stat=c('AUC','TSS','th'), opt = 2)

eval_plant <- getEvaluation(sdm_plant, w = 1:250,
                            wtest='test.dep', 
                            stat=c('AUC','TSS','th'), opt = 2)

#include column informing algorithm
eval_cat$Algorithm <- NA
eval_cat$Algorithm[1:25] <- 'brt'
eval_cat$Algorithm[26:50] <- 'cart'
eval_cat$Algorithm[51:75] <- 'fda'
eval_cat$Algorithm[76:100] <- 'glm'
eval_cat$Algorithm[101:125] <- 'mars'
eval_cat$Algorithm[126:150] <- 'maxlike'
eval_cat$Algorithm[151:175] <- 'mda'
eval_cat$Algorithm[176:200] <- 'gam'
eval_cat$Algorithm[201:225] <- 'rf'
eval_cat$Algorithm[226:250] <- 'svm'

eval_plant$Algorithm <- NA
eval_plant$Algorithm[1:25] <- 'brt'
eval_plant$Algorithm[26:50] <- 'cart'
eval_plant$Algorithm[51:75] <- 'fda'
eval_plant$Algorithm[76:100] <- 'glm'
eval_plant$Algorithm[101:125] <- 'mars'
eval_plant$Algorithm[126:150] <- 'maxlike'
eval_plant$Algorithm[151:175] <- 'mda'
eval_plant$Algorithm[176:200] <- 'gam'
eval_plant$Algorithm[201:225] <- 'rf'
eval_plant$Algorithm[226:250] <- 'svm'

#save evaluation metrics
setwd(wd_evaluation)
write.csv(eval_cat, 'Eval_P_bengalensis.csv')
write.csv(eval_plant, 'Eval_Z_prasina.csv')

#list models with TSS higher than 0.5
cat_sel_TSS <- which(eval_cat$TSS >= 0.5)
plant_sel_TSS <- which(eval_plant$TSS >= 0.5)
  


###################
##### PRESENT #####
###################



#list WDs to be used in projections for the present
wd_vars_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/Present'
wd_vars_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/Present'
wd_proj_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/Present'
wd_proj_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/Present'

#load variables
setwd(wd_vars_cat_pr)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_pr)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#project models
setwd(wd_proj_cat_pr)

pred_cat_pr <- list()
for(i in 1:250)
{
  pred_cat_pr[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                              filename = paste0('Pred_P_bengalensis_', i, '.grd'))
}

setwd(wd_proj_plant_pr)

pred_plant_pr <- list()
for(i in 1:250)
{
  pred_plant_pr[[i]] <- predict(sdm_plant, w = i, newdata = vars_sel_plant, 
                              filename = paste0('Pred_Z_prasina_', i, '.grd'))
}



#################################
##### 2041-2060 HadGEM3 4.5 #####
#################################



#list WDs to be used in projections for 2081-2100 HadGEM3 4.5
wd_vars_cat_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2041-2060_HadGEM3_RCP4.5'
wd_vars_plant_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2041-2060_HadGEM3_RCP4.5'
wd_proj_cat_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_HadGEM3_RCP4.5'
wd_proj_plant_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_HadGEM3_RCP4.5'

#load variables 2081-2100 HadGEM3 4.5
setwd(wd_vars_cat_had_4.5_2050)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_had_4.5_2050)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#change name of LU var to match the name used to train models
names(vars_sel_cat)[7] <- 'global_LULC_2015'
names(vars_sel_plant)[6] <- 'global_LULC_2015'

#project models
setwd(wd_proj_cat_had_4.5_2050)

pred_cat_had_4.5_2050 <- list()
for(i in 1:250)
{
  if(i %in% cat_sel_TSS){
    pred_cat_had_4.5_2050[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                                          filename = paste0('Pred_P_bengalensis_', i, '.grd'))
  }
}


setwd(wd_proj_plant_had_4.5_2050)

pred_plant_had_4.5_2050 <- list()
for(i in 1:250)
{
  if(i %in% plant_sel_TSS){
    pred_plant_had_4.5_2050[[i]] <- predict(sdm_plant, w = i,
                                            newdata = vars_sel_plant, 
                                            filename = paste0('Pred_Z_prasina_', i, '.grd'))
  }
}



#################################
##### 2041-2060 HadGEM3 8.5 #####
#################################



#list WDs to be used in projections for 2081-2100 HadGEM3 8.5
wd_vars_cat_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2041-2060_HadGEM3_RCP8.5'
wd_vars_plant_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2041-2060_HadGEM3_RCP8.5'
wd_proj_cat_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_HadGEM3_RCP8.5'
wd_proj_plant_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_HadGEM3_RCP8.5'

#load variables 2081-2100 HadGEM3 4.5
setwd(wd_vars_cat_had_8.5_2050)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_had_8.5_2050)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#change name of LU var to match the name used to train models
names(vars_sel_cat)[7] <- 'global_LULC_2015'
names(vars_sel_plant)[6] <- 'global_LULC_2015'

#project models
setwd(wd_proj_cat_had_8.5_2050)

pred_cat_had_8.5_2050 <- list()
for(i in 235:250)
{
  if(i %in% cat_sel_TSS){
    pred_cat_had_8.5_2050[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                                          filename = paste0('Pred_P_bengalensis_', i, '.grd'))
  }
}


setwd(wd_proj_plant_had_8.5_2050)

pred_plant_had_8.5_2050 <- list()
for(i in 1:250)
{
  if(i %in% plant_sel_TSS){
    pred_plant_had_8.5_2050[[i]] <- predict(sdm_plant, w = i,
                                            newdata = vars_sel_plant, 
                                            filename = paste0('Pred_Z_prasina_', i, '.grd'))
  }
}



#################################
##### 2081-2100 HadGEM3 4.5 #####
#################################



#list WDs to be used in projections for 2081-2100 HadGEM3 4.5
wd_vars_cat_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2081-2100_HadGEM3_RCP4.5'
wd_vars_plant_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2081-2100_HadGEM3_RCP4.5'
wd_proj_cat_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_HadGEM3_RCP4.5'
wd_proj_plant_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_HadGEM3_RCP4.5'

#load variables 2081-2100 HadGEM3 4.5
setwd(wd_vars_cat_had_4.5_2090)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_had_4.5_2090)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#change name of LU var to match the name used to train models
names(vars_sel_cat)[7] <- 'global_LULC_2015'
names(vars_sel_plant)[6] <- 'global_LULC_2015'

#project models
setwd(wd_proj_cat_had_4.5_2090)

pred_cat_had_4.5_2090 <- list()
for(i in 1:250)
{
  if(i %in% cat_sel_TSS){
    pred_cat_had_4.5_2090[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                          filename = paste0('Pred_P_bengalensis_', i, '.grd'))
  }
}


setwd(wd_proj_plant_had_4.5_2090)

pred_plant_had_4.5_2090 <- list()
for(i in 1:250)
{
  if(i %in% plant_sel_TSS){
    pred_plant_had_4.5_2090[[i]] <- predict(sdm_plant, w = i,
                                            newdata = vars_sel_plant, 
                              filename = paste0('Pred_Z_prasina_', i, '.grd'))
  }
}



#################################
##### 2081-2100 HadGEM3 8.5 #####
#################################



#list WDs to be used in projections for 2081-2100 HadGEM3 8.5
wd_vars_cat_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2081-2100_HadGEM3_RCP8.5'
wd_vars_plant_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2081-2100_HadGEM3_RCP8.5'
wd_proj_cat_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_HadGEM3_RCP8.5'
wd_proj_plant_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_HadGEM3_RCP8.5'

#load variables 2081-2100 HadGEM3 8.5
setwd(wd_vars_cat_had_8.5_2090)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_had_8.5_2090)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#change name of LU var to match the name used to train models
names(vars_sel_cat)[7] <- 'global_LULC_2015'
names(vars_sel_plant)[6] <- 'global_LULC_2015'

#project models
setwd(wd_proj_cat_had_8.5_2090)

pred_cat_had_8.5_2090 <- list()
for(i in 1:250)
{
  if(i %in% cat_sel_TSS){
    pred_cat_had_8.5_2090[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                                          filename = paste0('Pred_P_bengalensis_', i, '.grd'))
  }
}


setwd(wd_proj_plant_had_8.5_2090)

pred_plant_had_8.5_2090 <- list()
for(i in 1:250)
{
  if(i %in% plant_sel_TSS){
    pred_plant_had_8.5_2090[[i]] <- predict(sdm_plant, w = i,
                                            newdata = vars_sel_plant, 
                                            filename = paste0('Pred_Z_prasina_', i, '.grd'))
  }
}



################################
##### 2041-2060 MIROC6 4.5 #####
################################



#list WDs to be used in projections for 2081-2100 MIROC6 4.5
wd_vars_cat_mir_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2041-2060_MIROC6_RCP4.5'
wd_vars_plant_mir_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2041-2060_MIROC6_RCP4.5'
wd_proj_cat_mir_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_MIROC6_RCP4.5'
wd_proj_plant_mir_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_MIROC6_RCP4.5'

#load variables 2081-2100 HadGEM3 4.5
setwd(wd_vars_cat_mir_4.5_2050)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_mir_4.5_2050)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#change name of LU var to match the name used to train models
names(vars_sel_cat)[7] <- 'global_LULC_2015'
names(vars_sel_plant)[6] <- 'global_LULC_2015'

#project models
setwd(wd_proj_cat_mir_4.5_2050)

pred_cat_mir_4.5_2050 <- list()
for(i in 1:250)
{
  if(i %in% cat_sel_TSS){
    pred_cat_mir_4.5_2050[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                                          filename = paste0('Pred_P_bengalensis_', i, '.grd'))
  }
}


setwd(wd_proj_plant_mir_4.5_2050)

pred_plant_mir_4.5_2050 <- list()
for(i in 1:250)
{
  if(i %in% plant_sel_TSS){
    pred_plant_mir_4.5_2050[[i]] <- predict(sdm_plant, w = i,
                                            newdata = vars_sel_plant, 
                                            filename = paste0('Pred_Z_prasina_', i, '.grd'))
  }
}



################################
##### 2041-2060 MIROC6 8.5 #####
################################



#list WDs to be used in projections for 2081-2100 MIROC6 8.5
wd_vars_cat_mir_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2041-2060_MIROC6_RCP8.5'
wd_vars_plant_mir_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2041-2060_MIROC6_RCP8.5'
wd_proj_cat_mir_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2041-2060_MIROC6_RCP8.5'
wd_proj_plant_mir_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_MIROC6_RCP8.5'

#load variables 2081-2100 HadGEM3 8.5
setwd(wd_vars_cat_mir_8.5_2050)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_mir_8.5_2050)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#change name of LU var to match the name used to train models
names(vars_sel_cat)[7] <- 'global_LULC_2015'
names(vars_sel_plant)[6] <- 'global_LULC_2015'

#project models
setwd(wd_proj_cat_mir_8.5_2050)

pred_cat_mir_8.5_2050 <- list()
for(i in 1:250)
{
  if(i %in% cat_sel_TSS){
    pred_cat_mir_8.5_2050[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                                          filename = paste0('Pred_P_bengalensis_', i, '.grd'))
  }
}


setwd(wd_proj_plant_mir_8.5_2050)

pred_plant_mir_8.5_2050 <- list()
for(i in 1:250)
{
  if(i %in% plant_sel_TSS){
    pred_plant_mir_8.5_2050[[i]] <- predict(sdm_plant, w = i,
                                            newdata = vars_sel_plant, 
                                            filename = paste0('Pred_Z_prasina_', i, '.grd'))
  }
}



################################
##### 2081-2100 MIROC6 4.5 #####
################################



#list WDs to be used in projections for 2081-2100 MIROC6 4.5
wd_vars_cat_mir_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2081-2100_MIROC6_RCP4.5'
wd_vars_plant_mir_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2081-2100_MIROC6_RCP4.5'
wd_proj_cat_mir_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_MIROC6_RCP4.5'
wd_proj_plant_mir_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_MIROC6_RCP4.5'

#load variables 2081-2100 MIROC6 4.5
setwd(wd_vars_cat_mir_4.5_2090)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_mir_4.5_2090)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#change name of LU var to match the name used to train models
names(vars_sel_cat)[7] <- 'global_LULC_2015'
names(vars_sel_plant)[6] <- 'global_LULC_2015'

#project models
setwd(wd_proj_cat_mir_4.5_2090)

pred_cat_mir_4.5_2090 <- list()
for(i in 1:250)
{
  if(i %in% cat_sel_TSS){
    pred_cat_mir_4.5_2090[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                                          filename = paste0('Pred_P_bengalensis_', i, '.grd'))
  }
}


setwd(wd_proj_plant_mir_4.5_2090)

pred_plant_mir_4.5_2090 <- list()
for(i in 1:250)
{
  if(i %in% plant_sel_TSS){
    pred_plant_mir_4.5_2090[[i]] <- predict(sdm_plant, w = i,
                                            newdata = vars_sel_plant, 
                                            filename = paste0('Pred_Z_prasina_', i, '.grd'))
  }
}



################################
##### 2081-2100 MIROC6 8.5 #####
################################



#list WDs to be used in projections for 2081-2100 MIROC6 8.5
wd_vars_cat_mir_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/2081-2100_MIROC6_RCP8.5'
wd_vars_plant_mir_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/2081-2100_MIROC6_RCP8.5'
wd_proj_cat_mir_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/2081-2100_MIROC6_RCP8.5'
wd_proj_plant_mir_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_MIROC6_RCP8.5'

#load variables 2081-2100 MIROC6 8.5
setwd(wd_vars_cat_mir_8.5_2090)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_mir_8.5_2090)
vars_plant <- lapply(list.files(), raster)

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#change name of LU var to match the name used to train models
names(vars_sel_cat)[7] <- 'global_LULC_2015'
names(vars_sel_plant)[6] <- 'global_LULC_2015'

#project models
setwd(wd_proj_cat_mir_8.5_2090)

pred_cat_mir_8.5_2090 <- list()
for(i in 1:250)
{
  if(i %in% cat_sel_TSS){
    pred_cat_mir_8.5_2090[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                                          filename = paste0('Pred_P_bengalensis_', i, '.grd'))
  }
}


setwd(wd_proj_plant_mir_8.5_2090)

pred_plant_mir_8.5_2090 <- list()
for(i in 1:250)
{
  if(i %in% plant_sel_TSS){
    pred_plant_mir_8.5_2090[[i]] <- predict(sdm_plant, w = i,
                                            newdata = vars_sel_plant, 
                                            filename = paste0('Pred_Z_prasina_', i, '.grd'))
  }
}


