rm(list=ls())
# york tubes
source('./main_functions/qmat_functions.R')
source('./main_functions/compute_qpis.R')
source('./main_functions/bidimensional_sorting.R')

load('tubes.RData')

tubes <- tubes[tubes$month == month.abb[7],]
set.seed(1)
validation <- sample(1:length(tubes$value),60)
valid_set <- tubes[validation,c('X','Y','value')]

test_train <- tubes[-validation,c('X','Y','value')]
# scales inputs and outputs
train_dat <- scale(as.matrix(test_train))
valid_set <- scale(as.matrix(valid_set),
                  center = attr(train_dat,'scaled:center'),
                  scale =  attr(train_dat,'scaled:scale'))
library(mvtnorm)
# does splits
splits_0 <- sample_2d(x=train_dat[,c(1,2)],
                      y=train_dat[,3],
                      xstar = valid_set[,c(1,2)],
                      return_qs_only=T)
name0 <- paste0('splits_valid_tubes.RData')
save.image(file = name0)