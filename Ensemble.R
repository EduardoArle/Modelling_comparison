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

wd_evaluation <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Model_evaluation'
wd_ensemble <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Ensembles'

#load evaluation metrics
setwd(wd_evaluation)
eval_cat <- read.csv('Eval_P_bengalensis.csv')

#list models with TSS higher than 0.5
cat_sel_TSS <- which(eval_cat$TSS >= 0.5)



##### PRESENT #####



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

plot(cat_pr_ens, main = 'Present')

#save raster layer
setwd(wd_ensemble)
writeRaster(cat_pr_ens, 'Prionailurus_bengalensis_present.tif',
            format ="GTiff")

cat_pr_ens <- raster('Prionailurus_bengalensis_present.tif')



##### 2050 #####



##################################################################. had_4.5_2050



#load models
setwd(wd_cat_had_4.5_2050)

#load selected projections
cat_had_4.5_2050 <- list()
for(i in 1:length(cat_sel_TSS))
{
  cat_had_4.5_2050[[i]] <- raster(paste0('Pred_P_bengalensis_',
                                         cat_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
cat_had_4.5_2050_bin <- list()
for(i in 1:length(cat_had_4.5_2050))
{
  cat_had_4.5_2050_bin[[i]] <- cat_had_4.5_2050[[i]]
  th <- eval_cat$threshold[cat_sel_TSS[i]]
  cat_had_4.5_2050_bin[[i]][] <- ifelse(cat_had_4.5_2050_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
cat_had_4.5_2050_sum <- sum(stack(cat_had_4.5_2050_bin))

plot(cat_had_4.5_2050_sum, main = 'had_4.5_2050')

#save raster layer
setwd(wd_ensemble)
writeRaster(cat_had_4.5_2050_sum, 'Prionailurus_bengalensis_had_4.5_2050.tif',
            format ="GTiff")

cat_had_4.5_2050_sum <- raster('Prionailurus_bengalensis_had_4.5_2050.tif')



##################################################################. had_8.5_2050



#load models
setwd(wd_cat_had_8.5_2050)

#load selected projections
cat_had_8.5_2050 <- list()
for(i in 1:length(cat_sel_TSS))
{
  cat_had_8.5_2050[[i]] <- raster(paste0('Pred_P_bengalensis_',
                                         cat_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
cat_had_8.5_2050_bin <- list()
for(i in 1:length(cat_had_8.5_2050))
{
  cat_had_8.5_2050_bin[[i]] <- cat_had_8.5_2050[[i]]
  th <- eval_cat$threshold[cat_sel_TSS[i]]
  cat_had_8.5_2050_bin[[i]][] <- ifelse(cat_had_8.5_2050_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
cat_had_8.5_2050_sum <- sum(stack(cat_had_8.5_2050_bin))

plot(cat_had_8.5_2050_sum, main = 'had_8.5_2050')

#save raster layer
setwd(wd_ensemble)
writeRaster(cat_had_8.5_2050_sum, 'Prionailurus_bengalensis_had_8.5_2050.tif',
            format ="GTiff")

cat_had_8.5_2050_sum <- raster('Prionailurus_bengalensis_had_8.5_2050.tif')



################################################################## ensemble 2050



#sum all layers and calculate percentage of agreement
cat_2050_had_ens <- sum(stack(cat_had_4.5_2050_sum, cat_had_8.5_2050_sum)) /
                   (length(cat_sel_TSS) * 2)  * 100

par(mfrow = c(1,2))
plot(cat_pr_ens, main = 'Present')
plot(cat_had_2050_ens, main = '2050')
par(mfrow = c(1,1))

#save raster layer
setwd(wd_ensemble)
writeRaster(cat_had_2050_ens, 'Prionailurus_bengalensis_had_2050.tif',
            format ="GTiff")



##### 2090 #####



##################################################################. had_4.5_2090



#load models
setwd(wd_cat_had_4.5_2090)

#load selected projections
cat_had_4.5_2090 <- list()
for(i in 1:length(cat_sel_TSS))
{
  cat_had_4.5_2090[[i]] <- raster(paste0('Pred_P_bengalensis_',
                                         cat_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
cat_had_4.5_2090_bin <- list()
for(i in 1:length(cat_had_4.5_2090))
{
  cat_had_4.5_2090_bin[[i]] <- cat_had_4.5_2090[[i]]
  th <- eval_cat$threshold[cat_sel_TSS[i]]
  cat_had_4.5_2090_bin[[i]][] <- ifelse(cat_had_4.5_2090_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
cat_had_4.5_2090_sum <- sum(stack(cat_had_4.5_2090_bin))

plot(cat_had_4.5_2090_sum, main = 'had_4.5_2090')

#save raster layer
setwd(wd_ensemble)
writeRaster(cat_had_4.5_2090_sum, 'Prionailurus_bengalensis_had_4.5_2090.tif',
            format ="GTiff")



##################################################################. had_8.5_2090



#load models
setwd(wd_cat_had_8.5_2090)

#load selected projections
cat_had_8.5_2090 <- list()
for(i in 1:length(cat_sel_TSS))
{
  cat_had_8.5_2090[[i]] <- raster(paste0('Pred_P_bengalensis_',
                                         cat_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
cat_had_8.5_2090_bin <- list()
for(i in 1:length(cat_had_8.5_2090))
{
  cat_had_8.5_2090_bin[[i]] <- cat_had_8.5_2090[[i]]
  th <- eval_cat$threshold[cat_sel_TSS[i]]
  cat_had_8.5_2090_bin[[i]][] <- ifelse(cat_had_8.5_2090_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
cat_had_8.5_2090_sum <- sum(stack(cat_had_8.5_2090_bin))

plot(cat_had_8.5_2090_sum, main = 'had_8.5_2090')

#save raster layer
setwd(wd_ensemble)
writeRaster(cat_had_8.5_2090_sum, 'Prionailurus_bengalensis_had_8.5_2090.tif',
            format ="GTiff")



################################################################## ensemble 2090



#sum all layers and calculate percentage of agreement
cat_had_2090_ens <- sum(stack(cat_had_4.5_2090_sum, cat_had_8.5_2090_sum)) /
                   (length(cat_sel_TSS) * 2)  * 100

par(mfrow = c(1,3))
plot(cat_pr_ens, main = 'Present')
plot(cat_had_2050_ens, main = '2050')
plot(cat_had_2090_ens, main = '2090')
par(mfrow = c(1,1))

#save raster layer
setwd(wd_ensemble)
writeRaster(cat_2090_had_ens, 'Prionailurus_bengalensis_had_2090.tif',
            format ="GTiff")



#################
##### PLANT #####
#################



#list WDs
wd_plant_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/Present'

wd_plant_had_4.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_HadGEM3_RCP4.5'
wd_plant_had_8.5_2050 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2041-2060_HadGEM3_RCP8.5'

wd_plant_had_4.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_HadGEM3_RCP4.5'
wd_plant_had_8.5_2090 <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Zamia prasina/2081-2100_HadGEM3_RCP8.5'

wd_evaluation <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Model_evaluation'
wd_ensemble <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Ensembles'

#load evaluation metrics
setwd(wd_evaluation)
eval_plant <- read.csv('Eval_Z_prasina.csv')

#list models with TSS higher than 0.5
plant_sel_TSS <- which(eval_plant$TSS >= 0.5)



##### PRESENT #####



#load models
setwd(wd_plant_pr)

#load selected projections
plant_pr <- list()
for(i in 1:length(plant_sel_TSS))
{
  plant_pr[[i]] <- raster(paste0('Pred_Z_prasina_', plant_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
plant_pr_bin <- list()
for(i in 1:length(plant_pr))
{
  plant_pr_bin[[i]] <- plant_pr[[i]]
  th <- eval_plant$threshold[plant_sel_TSS[i]]
  plant_pr_bin[[i]][] <- ifelse(plant_pr_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
plant_pr_bin <- stack(plant_pr_bin)

#sum all layers and calculate percentage of agreement
plant_pr_ens <- sum(plant_pr_bin) / length(plant_sel_TSS) * 100

#save raster layer
setwd(wd_ensemble)
writeRaster(plant_pr_ens, 'Zamia_prasina_present.tif')



##### 2050 #####



##################################################################. had_4.5_2050



#load models
setwd(wd_plant_had_4.5_2050)

#load selected projections
plant_had_4.5_2050 <- list()
for(i in 1:length(plant_sel_TSS))
{
  plant_had_4.5_2050[[i]] <- raster(paste0('Pred_Z_prasina_',
                                         plant_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
plant_had_4.5_2050_bin <- list()
for(i in 1:length(plant_had_4.5_2050))
{
  plant_had_4.5_2050_bin[[i]] <- plant_had_4.5_2050[[i]]
  th <- eval_plant$threshold[plant_sel_TSS[i]]
  plant_had_4.5_2050_bin[[i]][] <- ifelse(plant_had_4.5_2050_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
plant_had_4.5_2050_sum <- sum(stack(plant_had_4.5_2050_bin))

plot(plant_had_4.5_2050_sum, main = 'had_4.5_2050')

#save raster layer
setwd(wd_ensemble)
writeRaster(plant_had_4.5_2050_sum, 'Zamia_prasina_had_4.5_2050.tif',
            format ="GTiff")

plant_had_4.5_2050_sum <- raster('Zamia_prasina_had_4.5_2050.tif')



##################################################################. had_8.5_2050



#load models
setwd(wd_plant_had_8.5_2050)

#load selected projections
plant_had_8.5_2050 <- list()
for(i in 1:length(plant_sel_TSS))
{
  plant_had_8.5_2050[[i]] <- raster(paste0('Pred_Z_prasina_',
                                         plant_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
plant_had_8.5_2050_bin <- list()
for(i in 1:length(plant_had_8.5_2050))
{
  plant_had_8.5_2050_bin[[i]] <- plant_had_8.5_2050[[i]]
  th <- eval_plant$threshold[plant_sel_TSS[i]]
  plant_had_8.5_2050_bin[[i]][] <- ifelse(plant_had_8.5_2050_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
plant_had_8.5_2050_sum <- sum(stack(plant_had_8.5_2050_bin))

plot(plant_had_8.5_2050_sum, main = 'had_8.5_2050')

#save raster layer
setwd(wd_ensemble)
writeRaster(plant_had_8.5_2050_sum, 'Zamia_prasina_had_8.5_2050.tif',
            format ="GTiff")

plant_had_8.5_2050_sum <- raster('Zamia_prasina_had_8.5_2050.tif')



################################################################## ensemble 2050



#sum all layers and calculate percentage of agreement
plant_had_2050_ens <- sum(stack(plant_had_4.5_2050_sum, plant_had_8.5_2050_sum)) /
  (length(plant_sel_TSS) * 2)  * 100

par(mfrow = c(1,2))
plot(plant_pr_ens, main = 'Present')
plot(plant_had_2050_ens, main = '2050')
par(mfrow = c(1,1))

#save raster layer
setwd(wd_ensemble)
writeRaster(plant_had_2050_ens, 'Zamia_prasina_had_2050.tif',
            format ="GTiff")



##### 2090 #####



##################################################################. had_4.5_2090



#load models
setwd(wd_plant_had_4.5_2090)

#load selected projections
plant_had_4.5_2090 <- list()
for(i in 1:length(plant_sel_TSS))
{
  plant_had_4.5_2090[[i]] <- raster(paste0('Pred_Z_prasina_',
                                         plant_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
plant_had_4.5_2090_bin <- list()
for(i in 1:length(plant_had_4.5_2090))
{
  plant_had_4.5_2090_bin[[i]] <- plant_had_4.5_2090[[i]]
  th <- eval_plant$threshold[plant_sel_TSS[i]]
  plant_had_4.5_2090_bin[[i]][] <- ifelse(plant_had_4.5_2090_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
plant_had_4.5_2090_sum <- sum(stack(plant_had_4.5_2090_bin))

plot(plant_had_4.5_2090_sum, main = 'had_4.5_2090')

#save raster layer
setwd(wd_ensemble)
writeRaster(plant_had_4.5_2090_sum, 'Zamia_prasina_had_4.5_2090.tif',
            format ="GTiff")



##################################################################. had_8.5_2090



#load models
setwd(wd_plant_had_8.5_2090)

#load selected projections
plant_had_8.5_2090 <- list()
for(i in 1:length(plant_sel_TSS))
{
  plant_had_8.5_2090[[i]] <- raster(paste0('Pred_Z_prasina_',
                                         plant_sel_TSS[i], '.grd'))
  print(i)
}

#binarise projections according to threshold
plant_had_8.5_2090_bin <- list()
for(i in 1:length(plant_had_8.5_2090))
{
  plant_had_8.5_2090_bin[[i]] <- plant_had_8.5_2090[[i]]
  th <- eval_plant$threshold[plant_sel_TSS[i]]
  plant_had_8.5_2090_bin[[i]][] <- ifelse(plant_had_8.5_2090_bin[[i]][] >= th, 1, 0)
  print(i)
}

#stack all projections
plant_had_8.5_2090_sum <- sum(stack(plant_had_8.5_2090_bin))

plot(plant_had_8.5_2090_sum, main = 'had_8.5_2090')

#save raster layer
setwd(wd_ensemble)
writeRaster(plant_had_8.5_2090_sum, 'Zamia_prasina_had_8.5_2090.tif',
            format ="GTiff")



################################################################## ensemble 2090



#sum all layers and calculate percentage of agreement
plant_2090_had_ens <- sum(stack(plant_had_4.5_2090_sum, plant_had_8.5_2090_sum)) /
  (length(plant_sel_TSS) * 2)  * 100

par(mfrow = c(1,3))
plot(plant_pr_ens, main = 'Present')
plot(plant_had_2050_ens, main = '2050')
plot(plant_had_2090_ens, main = '2090')
par(mfrow = c(1,1))

#save raster layer
setwd(wd_ensemble)
writeRaster(plant_2090_had_ens, 'Zamia_prasina_had_2090.tif',
            format ="GTiff")



#visualise
setwd('/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Projections/Ensembles_HadGEM3')

cat <- lapply(list.files(pattern = 'bengalensis'), raster)
names(cat) <- gsub('.tif', '', gsub('Prionailurus_bengalensis_', '',
                                     list.files(pattern = 'bengalensis')))


plant <- lapply(list.files(pattern = 'prasina'), raster)
names(plant) <- gsub('.tif', '', gsub('Zamia_prasina_', '',
                                       list.files(pattern = 'prasina')))

par(mfrow = c(1,3))
plot(cat$present, main = 'Present')
plot(cat$had_2050, main = 'HadGEM3 2050')
plot(cat$had_2090, main = 'HadGEM3 2090')


plot(plant$present, main = 'Present')
plot(plant$had_2050, main = 'HadGEM3 2050')
plot(plant$had_2090, main = 'HadGEM3 2090')
