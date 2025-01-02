#list WDs

wd_evaluation <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Adam Smith/Model_evaluation'

#load tables with eval metrics
setwd(wd_evaluation)

eval_cat <- read.csv('Eval_P_bengalensis.csv')
eval_plant <- read.csv('Eval_Z_prasina.csv')

#calculate average of metrics considering all models
cat_meanTSS_all <- mean(eval_cat$TSS)
cat_meanAUC_all <- mean(eval_cat$AUC)

plant_meanTSS_all <- mean(eval_plant$TSS)
plant_meanAUC_all <- mean(eval_plant$AUC)


#calculate average of metrics considering selected models
cat_meanTSS_sel <- mean(eval_cat$TSS[which(eval_cat$TSS >= 0.5 &
                                           eval_cat$AUC >= 0.7)])
cat_meanAUC_sel <- mean(eval_cat$AUC[which(eval_cat$TSS >= 0.5 &
                                           eval_cat$AUC >= 0.7)])


plant_meanTSS_sel <- mean(eval_plant$TSS[which(eval_plant$TSS >= 0.5 &
                                             eval_plant$AUC >= 0.7)])
plant_meanAUC_sel <- mean(eval_plant$AUC[which(eval_plant$TSS >= 0.5 &
                                             eval_plant$AUC >= 0.7)])
