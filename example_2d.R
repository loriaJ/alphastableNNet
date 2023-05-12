# example in 2 D
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
x <- as.matrix(expand.grid(seq(-1,1,length.out = 7),
                           seq(-1,1,length.out = 7)))
# matrix(runif(2*100,min=-1,max = 1),ncol = 2)
x_pred <- as.matrix(expand.grid(seq(-1,1,length.out = 9),
                                seq(-1,1,length.out = 9)))
f <- function(x, sd = sigma1){
  nn <- nrow(x)
  ifelse(x[,1] > 0, 5,0) + 
    ifelse(x[,2] > 0, 5,0) + 
    rnorm(n=nn,sd=sd)
}

y0 <- f(x)

## to run the pytorch procedure needs the following two files:
# name1 <- paste0('./pytorch_results/input_data_2D_example.csv')
# write.csv(cbind(x,y0),file = name1,row.names = F)
# name2 <- paste0('./pytorch_results/pred_input_2D_example.csv')
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

preds_stab <- apply(samp1$y_pred[1001:3000,],median,MARGIN = 2)
# predictions at seen locations:
# no_preds_stab <- apply(samp1$y_no_pred[1001:3000,],median,MARGIN = 2)

time_bayes_gp <- system.time({
  preds_gp <- tgp::bgp(X = x,Z = y0,XX = x_pred,
                       corr='matern',
                       m0r1 = F,BTE = c(10000,20000,1),zcov = T)}
)
time_mle_gp <- system.time({
  ml <- mlegp::mlegp(X = x,Z = y0,nugget = 1,
                     nugget.known = F)}
)
pred_ml <- mlegp::predict.gp(ml,x_pred)

# reads predictions from pytorch procedures
# uses the pytorch 2D script to run the neural network
name_preds <- paste0('./pytorch_results/pytorch_result_2D.csv')
preds_nn <- read.csv(name_preds,header = F,sep = ',')
pred_bb <- preds_nn[,3]

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

cbind(x_pred,
      preds_stab) %>%
  as.data.frame() %>%
  rename(x=1,y=2,z=3) %>%
  mutate(Matern = preds_gp$ZZ.med,
         Freq = pred_ml,
         BayesNNet = pred_bb) %>% 
  rename(Stable = z) %>% 
  gather(-x,-y,key = 'Method',value = 'Prediction') %>% 
  mutate(Method = case_when(Method == 'BayesNNet'~'Bayes\nNNet',
                            Method == 'Matern'~'GP\nBayes',
                            Method == 'Freq'~'GP\nMLE',
                            Method == 'Stable'~'Stable')) %>% 
  ggplot(aes(x=x,y=y,fill=Prediction)) + 
  geom_tile() + 
  scale_fill_viridis_c(option = 'E',)+
  xlab(latex2exp::TeX('$x_1$'))+
  ylab(latex2exp::TeX('$x_2$'))+
  # labs(fill = '') + 
  facet_wrap('Method',strip.position = 'top') + 
  theme_bw(base_size = 20) +
  theme(strip.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 20),
        strip.text = element_text(face = "bold", 
                                  size = rel(0.5)))
