rm(list = ls())
# example in 1 D
if(!require(mvtnorm)){
  install.packages('mvtnorm')
}
if(!require(mlegp)){
  install.packages('mlegp')
}
if(!require(tgp)){
  install.packages('tgp')
}
if(!require(tidyverse)){
  install.packages('tidyverse')
}

source('./main_functions/bidimensional_sorting.R')
source('./main_functions/compute_qpis.R')
source('./main_functions/qmat_functions.R')
## general functions test cases
# for reproducibility:
set.seed(1)

# change as appropriate:
alpha0 <- 1.1
nu0 <- 1
sigma1<- 0.5

# loads observations, and input locations,
x <- matrix(seq(-2,2,length.out = 40),ncol=1)
x_pred <- matrix(seq(-2,2,length.out = 100),ncol = 1)
f <- function(xx,sd=sigma1){
  nn <- length(xx)
  
  ifelse(xx >= 1 | (xx >= -1 & xx < 0) ,
         5,0) +
    rnorm(n = nn,sd = sd)
}

y0 <- f(x)

## to run the pytorch procedure needs the following two files:
# name1 <- paste0('./pytorch_results/input_data_1D_example.csv')
# write.csv(cbind(x,y0),file = name1,row.names = F)
# name2 <- paste0('./pytorch_results/pred_input_1D_example.csv')
# write.csv(x_pred,file = name2,row.names = F)

time_stab1 <- system.time(
  {
    samp1 <- sample_2d(x=x,y=y0,
                       xstar = x_pred,
                       sigma_sq = sigma1,
                       alpha0 = alpha0,
                       known_sigma = F,
                       nu = nu0,
                       n_sim = 3000)}
)
# pointwise median
preds_stab <- apply(samp1$y_pred[1001:3000,],median,MARGIN = 2)
# predictions at seen locations:
# no_preds_stab <- apply(samp1$y_no_pred[1001:3000,],median,MARGIN = 2)

time_bayes_gp <- system.time({
  preds_gp <- bgp(X = x,Z = y0,XX = x_pred,
                       corr='matern',
                       m0r1 = F,BTE = c(10000,20000,1),zcov = T)}
)

time_mle_gp <- system.time({
  ml <- mlegp(X = x,Z = y0,nugget = 1,
                     nugget.known = F)}
)
pred_ml <- predict.gp(ml,x_pred)

# reads predictions from pytorch procedures
# uses the pytorch 1D process to run the neural network
name_preds <- paste0('./pytorch_results/pytorch_result_1D.csv')
preds_nn <- read.csv(name_preds,header = F,sep = ',')
pred_bb <- preds_nn[,2]

## evaluate in 100 simulations the performance of the methods
errors <- matrix(NA,nrow = 100,ncol = 4)
for(i in 1:100){
  y_new <- f(x_pred)
  y_new_olds <- f(x)
  
  errors[i,] <- c(
    mean(abs(preds_gp$ZZ.med - y_new)),
    mean(abs(pred_ml - y_new)),
    mean(abs(pred_bb - y_new)),
    mean(abs(preds_stab - y_new)))
}


df_errors <- as.data.frame(errors) %>% 
  rename(Matern = V1,
         mleGP = V2,
         BayesNNet = V3,
         Stable = V4) %>% 
  gather(key = Type,value = errors)

df_errors %>% 
  mutate(Method = case_when(Type == 'BayesNNet'~'Bayes\nNNet',
                            Type == 'Matern'~'GP\nBayes',
                            Type == 'mleGP'~'GP\nMLE',
                            Type =='Stable'~'Stable')) %>% 
  ggplot(aes(x=Method,y=errors)) + 
  geom_boxplot() +
  xlab('') +
  theme_bw(base_size = 20)+
  ylab('MAE') + 
  theme(text = element_text(size = 20))

df1 <- cbind(x_pred,
             preds_stab) %>%
  as.data.frame() %>%
  rename(x=1,Stable=2) %>%
  mutate(Bayes = preds_gp$ZZ.med,
         Frequentist = pred_ml,
         BayesNNet = pred_bb) %>% 
  gather(-x,key = 'Method',value = 'Prediction')

original_data <- data.frame(x=x,y=y0)
df1 %>% 
  mutate(Method = case_when(Method == 'BayesNNet'~'Bayes\nNNet',
                            Method == 'Bayes'~'GP\nBayes',
                            Method == 'Frequentist'~'GP\nMLE',
                            Method =='Stable'~'Stable')) %>% 
  ggplot(aes(x=x,y=Prediction,color = Method,linetype = Method)) + 
  geom_line(size = 1) + 
  geom_point(data = original_data,aes(x=x,y=y),inherit.aes = F) + 
  scale_linetype_manual(values = c('Bayes\nNNet'=2,'GP\nBayes'=3,'GP\nMLE'=4,'Stable' = 1)) +
  xlab('') +
  theme_bw(base_size = 20) + 
  theme(legend.position = c(0.10,0.7),text = element_text(size = 20),
        legend.background = element_rect(fill='white',color='black'))

