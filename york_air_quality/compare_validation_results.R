rm(list = ls())
if(!require(mlegp)){
  install.packages('mlegp')
}
if(!require(tgp)){
  install.packages('tgp')
}
if(!require(tidyverse)){
  install.packages('tidyverse')
}

load('samps_valid_tubes.RData')

preds_gp <- bgp(X = train_dat[,c(1,2)],
                     Z = train_dat[,3],
                     XX = valid_set[,c(1,2)],
                     corr='matern',
                     m0r1 = F,BTE = c(10000,20000,1),
                     zcov = T)

ml <- mlegp(X = train_dat[,c(1,2)],
                   Z = train_dat[,3],
                   nugget = 1,
                   nugget.known = F)
pred_ml <- predict.gp(ml,valid_set[,c(1,2)])
# to run the python code:
write.csv(train_dat,file = 'training_set.csv',row.names = F)
write.csv(valid_set,file = 'validation_set.csv',row.names = F)
# pred_ml_olds <- mlegp::predict.gp(ml,x)
bb1 <- read.csv('prediction_validation_set.csv',header = F)

pred_bb <- bb1$V3

# compares mean absolute error:
mean(abs(pred_bb - valid_set[,c(3)]))
mean(abs(pred_ml - valid_set[,c(3)]))
mean(abs(preds_gp$ZZ.med - valid_set[,c(3)]))
mean(abs(apply(samps$y_pred,2,median) - valid_set[,c(3)]))

