

## five is doing good, but nnet is failing, 
# might need a new nnet method
## seven is also doing good, similar issue with nnet

load('samps_valid_tubes.RData')
plot(apply(samps$y_pred,2,median),valid_set[,3])
abline(0,1)
preds_gp <- tgp::bgp(X = train_dat[,c(1,2)],
                     Z = train_dat[,3],
                     XX = valid_set[,c(1,2)],
                     corr='matern',
                     m0r1 = F,BTE = c(10000,20000,1),
                     zcov = T)
ml <- mlegp::mlegp(X = train_dat[,c(1,2)],
                   Z = train_dat[,3],
                   nugget = 1,
                   nugget.known = F)
write.csv(train_dat,file = 'training_set.csv',row.names = F)
write.csv(valid_set,file = 'validation_set.csv',row.names = F)
pred_ml <- mlegp::predict.gp(ml,valid_set[,c(1,2)])
# pred_ml_olds <- mlegp::predict.gp(ml,x)
bb1 <- read.csv('validation_set.csv',header = F)
# bb1 <- brnn::brnn(x = train_dat[,c(1,2)],
#                   y = train_dat[,c(3)],
#                   neurons = 10,
#                   normalize = FALSE)
pred_bb <- bb1$V3

mean(abs(pred_bb - valid_set[,c(3)]))
mean(abs(pred_ml - valid_set[,c(3)]))
mean(abs(preds_gp$ZZ.med - valid_set[,c(3)]))
mean(abs(apply(samps$y_pred,2,median) - valid_set[,c(3)]))


























