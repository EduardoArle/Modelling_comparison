#load packages
library(raster); library(sdm)

#list WDs
wd_modelObjects <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/ENMs/modelObjects'
wd_vars_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Prionailurus bengalensis/Present'
wd_vars_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Variables/Processed/Zamia prasina/Present'
wd_proj_cat_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Prionailurus bengalensis/Present'
wd_proj_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/Present'
wd_evaluation <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Model_evaluation'

#load model objects
setwd(wd_modelObjects)
sdm_cat <- read.sdm('models_Prionailurus_bengalensis.sdm')
sdm_plant <- read.sdm('models_Zamia_prasina.sdm')

#load variables present
setwd(wd_vars_cat_pr)
vars_cat <- lapply(list.files(), raster)

setwd(wd_vars_plant_pr)
vars_plant <- lapply(list.files(), raster)

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

#make a rasterStack with selected variables for each species
vars_sel_cat <- stack(vars_cat[c(12,14,18,5,6,10,20)])
vars_sel_plant <- stack(vars_plant[c(13,15,17,7,10,20)])

#project models to present (only the ones with TSS higher than 0.5)
setwd(wd_proj_cat_pr)

pred_cat_pr <- list()
for(i in 1:250)
{
  pred_cat_pr[[i]] <- predict(sdm_cat, w = i, newdata = vars_sel_cat, 
                              filename = paste0('Pred_P_bengalensis_', i, '.grd'))
}

pred_cat_pr <- stack(pred_cat_pr)

setwd(wd_proj_plant_pr)

pred_plant_pr <- list()
for(i in 1:250)
{
  pred_plant_pr[[i]] <- predict(sdm_plant, w = i, newdata = vars_sel_plant, 
                              filename = paste0('Pred_Z_prasina_', i, '.grd'))
}

pred_plant_pr <- stack(pred_plant_pr)





