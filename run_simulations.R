
source('./main_functions/bidimensional_sorting.R')
source('./main_functions/compute_qpis.R')
source('./main_functions/qmat_functions.R')
## general functions test cases
# for reproducibility:
set.seed(1)

# change as appropriate:
alpha0 <- 1
nu0 <- 1
sigma1<- 0.5
type <- 2
n_dim <- 2

invisible(eval(parse(text=commandArgs(TRUE)))) 
# loads observations, and input locations,
# generated from the file 'create all datasets.R'
load(paste0('./np_results/input_data_',type,'_',n_dim,'.RData'))

if(n_dim == 1){
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
} else {
  ## assumes it uses the 7x7 and 9x9 grids, to avoid recomputation 
  ## of values
  load('./np_results/x_splits_7_9.RData')
  time_stab1 <- system.time(
    {
      samp1 <- sample_2d(x=x,y=y0,
                         xstar = x_pred,
                         sigma_sq = sigma1,
                         alpha0 = alpha0,
                         known_sigma = F,
                         return_qs_only = F,
                         q_list_lambda = splits,
                         nu = nu0,
                         n_sim = 3000)}
  )
}
preds_stab <- apply(samp1$y_pred[1001:3000,],median,MARGIN = 2)

# no_preds_stab <- apply(samp1$y_no_pred[1001:3000,],median,MARGIN = 2)

time_bayes_gp <- system.time({
  preds_gp <- tgp::bgp(X = x,Z = y0,XX = x_pred,
                       corr='matern',
                       m0r1 = F,BTE = c(10000,20000,1),zcov = T)})
time_mle_gp <- system.time({
  ml <- mlegp::mlegp(X = x,Z = y0,nugget = 1,
                     nugget.known = F)}
  )
pred_ml <- mlegp::predict.gp(ml,x_pred)

# reads predictions from pytorch procedures
name_preds <- paste0('./np_results/prediction_',type,'_',n_dim,'.csv')
preds_nn <- read.csv(name_preds,header = F,sep = ',')
pred_bb <- preds_nn[,1 + n_dim]
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

plot_name <- paste('gen_case_alpha',alpha0,'sigma',
                   sigma1,'sigma_unk_nu',nu0,'type',
                   type,'dim',n_dim,sep ='_')
save(errors,alpha0,nu0,sigma1,type,n_dim,
     time_stab1,time_mle_gp,time_nn1,time_bayes_gp,
     file = plot_name)

library(tidyverse)

df_errors <- as.data.frame(errors) %>% 
  rename(Matern = V1,
         mleGP = V2,
         BayesNNet = V3,
         Stable = V4) %>% 
  gather(key = Type,value = errors)

df_errors %>% 
  ggplot(aes(x=Type,y = errors))+
  geom_boxplot() + 
  theme_bw(base_size = 20) + 
  xlab('') + 
  ylab('MAE')

plot_name <- paste('gen_case_alpha',alpha0,'sigma',
                   sigma1,'sigma_unk_nu',nu0,'type',
                   type,'dim',n_dim,sep ='_')

ggsave(filename = paste0('./images/bp_',plot_name,
                         '.jpg',sep=''),
       bg = 'white',height = 8,width = 15,
       units = 'cm')
if(n_dim==1){
  
  original_data <- data.frame(x=x,y=y0)
  
  df1 <- cbind(x_pred,
               preds_stab) %>%
    as.data.frame() %>%
    rename(x=1,Stable=2) %>%
    mutate(Bayes = preds_gp$ZZ.med,
           Frequentist = pred_ml,
           BayesNNet = pred_bb) %>% 
    gather(-x,key = 'Method',value = 'Prediction')
  df1 %>% 
    ggplot(aes(x=x,y=Prediction,color = Method)) +
    geom_line() + 
    geom_point(data= original_data,aes(x=x,y=y),inherit.aes = F) + 
    theme_bw()
  
  ggsave(filename = paste0('./images/line_',plot_name,'.jpg',sep=''),
         bg = 'white',height = 8,width = 15,
         units = 'cm')
  
}

if(n_dim == 2){
  
  df1 <- cbind(x_pred,
               preds_stab) %>%
    as.data.frame() %>%
    rename(x=1,y=2,z=3) %>%
    mutate(Matern = preds_gp$ZZ.med,
           Freq = pred_ml,
           BayesNNet = pred_bb)
  df1 %>% 
    rename(Stable = z,
           Bayes = Matern,
           Frequentist = Freq
    ) %>% 
    gather(-x,-y,key = 'type',value = 'prediction') %>% 
    ggplot(aes(x=x,y=y,fill=prediction)) + 
    geom_tile() + 
    theme_bw(base_size = 10) + 
    facet_grid(.~type )
  ggsave(filename = paste0('./images/contour_',plot_name,'.jpg',sep=''),
         bg = 'white',height = 8,width = 15,
         units = 'cm')
}

save.image(file = paste0('images/',plot_name,'.RData'))
