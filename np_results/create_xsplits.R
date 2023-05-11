x <- as.matrix(expand.grid(seq(-1,1,length.out = 7),
                          seq(-1,1,length.out = 7)))
  # matrix(runif(2*100,min=-1,max = 1),ncol = 2)
x_pred <- as.matrix(expand.grid(seq(-1,1,length.out = 9),
                                seq(-1,1,length.out = 9)))


source('bidim_sorting_modf.R')
source('compute_qpis.R')
source('qmat_indep_Q.R')
time_splits <- system.time(
  {
    splits <- sample_2d(x=x,y=NA,
                       xstar = x_pred,return_qs_only = TRUE)}
)

save(x,x_pred,splits,time_splits,file = 'x_splits_7_9.RData')