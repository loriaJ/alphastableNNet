
invisible(eval(parse(text=commandArgs(TRUE)))) 
create_and_save_data <- function(n_dim,type,sigma1=0.5){
  
  set.seed(1)
  
  
  if(n_dim == 1){
    x <- matrix(seq(-2,2,length.out = 40),ncol=1)
    x_pred <- matrix(seq(-2,2,length.out = 100),ncol = 1)
    # 1 jump
    f1 <- function(xx,sd=sigma1){
      nn <- length(xx)
      ifelse(xx >= 0,
             5,0) +
        rnorm(n = nn,sd = sd)
    }
    # 2 jumps
    f2 <- function(xx,sd=sigma1){
      nn <- length(xx)
      
      ifelse(xx < 2/3 & xx >= -2/3,
             0,5) +
        rnorm(n = nn,sd = sd)
    }
    # piecewise cuadratic
    f3 <- function(xx,sd=sigma1){
      nn <- length(xx)
      ifelse(xx>= 0,
             (-2*(xx)^2+8),-3*xx + 2) +
        rnorm(n = nn,sd = sd)
    }
    # 3 jumps
    f4 <- function(xx,sd=sigma1){
      nn <- length(xx)
      
      ifelse(xx >= 1 | (xx >= -1 & xx < 0) ,
             5,0) +
        rnorm(n = nn,sd = sd)
    }
    
    f5 <- function(xx,sd = sigma1){
      nn <- length(xx)
      -2 * cos(xx)^2 + 3 * tanh(xx) -2 * xx + 
        rnorm(n = nn,sd = sd)
    }
  } else if(n_dim == 2){
    
    x <- as.matrix(expand.grid(seq(-1,1,length.out = 7),
                               seq(-1,1,length.out = 7)))
    # matrix(runif(2*100,min=-1,max = 1),ncol = 2)
    x_pred <- as.matrix(expand.grid(seq(-1,1,length.out = 9),
                                    seq(-1,1,length.out = 9)))
    f1 <- function(x,sd=sigma1){
      nn <- nrow(x)
      ifelse(x[,1] + x[,2] > 0,5,0) + rnorm(n=nn,sd = sd)
      # x[,1]^2 + x[,2]^2 + x[,1] * x[,2] + rnorm(n = nn,sd = sd)
    }
    f2 <- function(x,sd=sigma1){
      
      nn <- nrow(x)
      ifelse(x[,1]^2 + 2*x[,2] -0.4 > 0,5,0) + rnorm(n=nn,sd = sd)
      
    }
    f3 <- function(x,sd = sigma1){
      nn <- nrow(x)
      sq1 <- pmax(abs(x[,1]), abs(x[,2]))
      ifelse(sq1 >sqrt(0.33),
             ifelse(sq1 > sqrt(0.67),
                    5,0),-5) + rnorm(n= nn,sd = sd)
    }
    f4 <- function(x, sd = sigma1){
      nn <- nrow(x)
      ifelse(x[,1] > 0, 5,0) + 
        ifelse(x[,2] > 0, 5,0) + 
        rnorm(n=nn,sd=sd)
    }
    f5 <- function(x, sd = sigma1){
      nn <- nrow(x)
      x[,1]^2 + x[,2]^2 - x[,1]*x[,2] + 
        rnorm(n=nn,sd=sd)
    }
  }
  f <- if(type == 1){
    f1
  } else if(type == 2) {
    f2
  } else if (type == 3){
    f3
  } else if (type == 4){
    f4
  } else {
    f5
  }
  y0 <- f(x)
  
  
  name0 <- paste0('../np_results/input_data_',type,'_',n_dim,'.RData')
  save(f,y0,x,x_pred,type,n_dim,file = name0)
  name1 <- paste0('../np_results/input_data_',type,'_',n_dim,'.csv')
  write.csv(cbind(x,y0),file = name1,row.names = F)
  name2 <- paste0('../np_results/pred_input_',type,'_',n_dim,'.csv')
  write.csv(x_pred,file = name2,row.names = F)
}


for(type in 1:5){
  for(n_dim in 1:2){
    create_and_save_data(n_dim = n_dim,type = type)
  }
}




















