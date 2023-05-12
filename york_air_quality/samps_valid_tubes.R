
name0 <- paste0('splits_valid_tubes.RData')
load(file = name0)
# optimal hyperparameters:
alpha0 <- 1.9
nu0 <- 1
# locations, standardized are the first two columns of 
# train dat and valid set, observations (standardized)
# are in the third column of the respective objects
samps <- sample_2d(x=train_dat[,c(1,2)], 
                   y=train_dat[,3],
                   xstar = valid_set[,c(1,2)],
                   alpha0 = alpha0,
                   known_sigma = F,
                   return_qs_only = F,
                   q_list_lambda = splits_0,
                   nu = nu0,
                   n_sim = 2000)

# computes mean absolute error on the validation set:
test_mae <- mean(
  abs(
    apply(samps$y_pred[1001:2000,],2,median) - valid_set[,3]
  )
)
# computes mean absolute error on the training set:
train_mae <- mean(
  abs(
    apply(samps$y_no_pred[1001:2000,],2,median) - train_dat[,3]
  )
)

name0 <- paste0('samps_valid_tubes.RData')
save.image(file = name0)
