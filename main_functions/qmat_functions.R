## q mat new!

# based on the method by Chambers Mallow and Stuck,
# but computed in the log-domain when beta == 1
ralpha_stable_true0 <- function(n,alpha,beta=1,scale=1){
  phi_0 <- runif(n,-pi/2,pi/2) # ? 
  E_i <- rexp(n)
  if(alpha == 1){
    scale * ( 2/pi * 
                ( (pi/2 + beta * phi_0)*tan(phi_0) - 
                    beta*log(E_i*cos(phi_0)/(pi/2 + beta*phi_0) ) )  )  + 
      (2/pi) * scale * beta *log(scale)
    
  } else if(beta == 1) { 
    b_0 <- atan(beta*tanpi(alpha/2))/alpha
    exp(-(1/alpha)*log(cos(phi_0)) + log(sin(alpha*phi_0+alpha*b_0)) +
          ((1-alpha)/alpha)*(log(cos(phi_0 -alpha*phi_0 -alpha*b_0 ))-log(E_i)) + 
          log(scale)/alpha)
    
  } else {
    b_0 <- atan(beta*tanpi(alpha/2))/alpha
    sin(alpha*phi_0+alpha*b_0)/cos(phi_0)^(1/alpha) *
      (cos(phi_0 -alpha*phi_0 -alpha*b_0 )/E_i)^((1-alpha)/alpha) *
      scale^(1/alpha)
  }
  
}

# s is indexed by lambda, the qs are also indexed
# by lambda
Qmat <- function(s,sigma_sqrd,lambda,qs_alph,ncomp){
  sq <- s * qs_alph
  
  t(sq * lambda) %*% lambda + diag(sigma_sqrd,ncomp)
}


full_array_sums <- function(lambda){
  dims0 <- dim(lambda)
  array0 <- array(NA,dim = c(dims0[2],dims0[2],dims0[1]))
  for(k in 1:dims0[1]){
    array0[,,k] <- t(lambda[k,]) %o% lambda[k,]
  }
  return(array0)
}

# simulate predictions
simulate_ystar <- function(qmat,q_new,k_mat,y,n_old,n_all){
  mean_pred <- t(k_mat) %*% 
    solve(qmat,y)
  var_pred <- q_new - 
    t(k_mat) %*% 
    solve(qmat,
          k_mat)
  # to ensure numerical stability
  var_pred <- (var_pred + t(var_pred))/2
  mvtnorm::rmvnorm(n=1, mean=mean_pred,sigma = var_pred)
}

# Acceptance/rejection step to simulate the random scales s_ell
# First part of Algorithm S2 of the main paper
simulate_sks <- function(s_olds,s_news,q_mat,y,lambda_mat,nu,
                         qs,sigma_sqrd,ncomp,full_array,n_obs){
  s_temp <- s_olds
  q_mat_temp <- q_mat[1:n_obs,1:n_obs]
  
  q_obs <- q_mat[1:n_obs,1:n_obs]
  k_obs <- q_mat[-(1:n_obs),1:n_obs]
  q_preds <- q_mat[-(1:n_obs),-(1:n_obs)]
  
  # compute the inverse with the svd
  cq_inv <- svd(q_obs)
  q_inv <- cq_inv$v %*% diag(1/cq_inv$d) %*% t(cq_inv$u)
  log_old_det <- sum(log(cq_inv$d))
  # log likelihood with the initial values
  log_lik_prev <- -.5 * t(y) %*%q_inv %*% y - 
    0.5 *log_old_det - n_obs*0.5*log(2*pi)
  for(k in seq_along(s_news)){
    s_temp[k] <- s_news[k]
    
    # update the q matrix with only the k-th new scale 
    q_mat_temp <- q_mat + (s_temp[k] - s_olds[k]) * 
      qs[k] * full_array[1:n_obs,1:n_obs,k]
    
    pi_k_qinv <- t(lambda_mat[k,1:n_obs]) %*% q_inv
    
    # to update the q matrix with only the k-value
    div_k <- c(1 + 
                 qs[k]*(s_temp[k] - s_olds[k]) * 
                 pi_k_qinv %*% lambda_mat[k,1:n_obs])
    
    # rank-one update of the inverse of the q matrix, using the new k-th scale 
    new_inv <- q_inv  -
      qs[k]*(s_temp[k] - s_olds[k]) *t(pi_k_qinv) %*% pi_k_qinv / div_k
    
    # rank-one update of the log determinant of q matrix, using the new k-th scale
    log_new_det <- log_old_det  + log(div_k)
    
    # log likelihood with the new matrix
    log_lik0 <- max(-0.5 * t(y) %*% new_inv %*% y - 
                      0.5*log_new_det - (n_obs/2)*log(2*pi),
                    -Inf,
                    na.rm = T)
    a <- (log_lik0 - log_lik_prev)
    if(rbinom(n = 1,size = 1,prob = min(exp(a),1))){
      # things to update when accept:
      # qmat
      # log lik
      # s, by default
      q_mat <- q_mat_temp
      q_inv <- new_inv
      log_old_det <- log_new_det
      
      log_lik_prev <- log_lik0
    } else {
      # qmat stays the same when reject
      # log_lik also stays the same
      # s
      s_temp[k] <- s_olds[k]
    }
  }
  return(list(s=s_temp,
              q = q_mat,
              log_lik = log_lik_prev))
}

# simulate the standard deviation sigma with accept/reject
# Second part of Algorithm S2 of the main paper
simulate_sigmasq <- function(prop_sigmasq,sigma_sq,y,q_mat,n_obs,
                             old_log_lik){
  
  new_q <- q_mat - diag(sigma_sq - prop_sigmasq,n_obs)
  
  log_like_new <- mvtnorm::dmvnorm(c(y),sigma = new_q,
                                   log = T)
  
  a <- log_like_new - old_log_lik
  if(rbinom(n=1,size = 1,prob = min(1,exp(a)))){
    
    return(list(sigma_sq = prop_sigmasq,
                q = new_q,
                log_lik = log_like_new))
    
  } else {
    return(list(sigma_sq = sigma_sq,
                q = q_mat,
                log_lik = old_log_lik))
    
  }
  
}

# compute the "k matrix" 
# referred in the paper as: $Q_{*,1:n}$
k_mat <- function(s,
      lambda,
      n_obs,
      qs_alph){
  
  t(s * qs_alph * lambda[,1:n_obs]) %*% lambda[,-(1:n_obs)]
}

# simulate predictions at the unoberserved values
simulate_y_nostar <- function(qmat,y,n_old,sigma_sq){
  
  ind_old <- 1:n_old
  q_old <- qmat[ind_old,ind_old]
  covs <- q_old - diag(sigma_sq,n_old)
  # svdcov <- svd(covs)
  mean_pred <- covs %*% solve(q_old,y)
  var_pred <- q_old - covs %*% solve(q_old,covs)
  # for numerical stability
  var_pred <- (var_pred + t(var_pred))/2
  mvtnorm::rmvnorm(n=1, mean=mean_pred,sigma = var_pred)
}

# main algorithm that simulates the predictions at xstar locations,
# given observations y at x locations. x and y must be matrices.
sample_2d <- function(x,y,xstar,sigma_sq = 1,alpha0 = 1,
                      known_sigma = TRUE,nu = 1,
                      n_sim = 1000, return_qs_only = FALSE,
                      q_list_lambda = list()){
  print(nu)
  n_old <- nrow(x)
  all_x <- rbind(x,
                 xstar)
  # when I = 1, the dimension of input variables, uses explicit values
  if(is.null(dim(x)) | ncol(x) == 1 ){
    
    qpis_ret <- qpis_1d(x = all_x)
    qs <- qpis_ret$probs
    lambda1 <- qpis_ret$lambda
    
  } else if(ncol(x) == 2){
    # when I = 2, the dimension of input variables, 
    # uses the algorithm by Goodman and Pollack for the partitions Lambda
    # and the Genz and Bretz method for obtaining the probabilities q_ell
    
    # gives the option of using an input Lambda with corresponding
    # probabilities q_ell
    if(length(q_list_lambda) == 0){
      
      # gives the option of returning the Lambda with corresponding
      # probabilities q_ell
      qpis_ret <- qpis(x = all_x)
      if (return_qs_only) {
        return(qpis_ret)
      }
      
      qs <- qpis_ret$probs
      lambda1 <- qpis_ret$lambda
      rm(qpis_ret)
      
    } else {
      
      qs <- q_list_lambda$probs
      lambda1 <- q_list_lambda$lambda
    }
    
  } else {
    # if there are more than two columns in the x matrix
    return('cannot handle more than 2 dimensions yet')
  }
  # removes the probabilities that are zero or NA
  qs <- ifelse(is.na(qs),0,qs)
  pos_qs <- qs > 0
  # removes the partitions that correspond to those probabilities removed
  lambda1 <- lambda1[pos_qs,]
  
  # to simplify a part of the simulation of the s_ell
  full_array <- full_array_sums(lambda = lambda1)
  
  # number of scales that will be used.
  n_comp <- sum(pos_qs,na.rm = T)
  # simulate scales from the prior
  s1 <- ralpha_stable_true0(n=n_comp,
                            alpha = alpha0/2,
                            beta = 1,scale = 1)
  qalph <- qs[pos_qs]^(2/alpha0)
  
  # number of observations + prediction locations
  # in the main paper this is n + m
  nys <- nrow(all_x)
  sigma_sq_v <- rep(NA,n_sim)
  
  # gives the option in case sigma is known
  if(known_sigma){
    sigma_sq_v <- rep(sigma_sq,n_sim)
  } else {
    # simulates it as a half-Cauchy
    sigma_sq_v[1] <- abs(rnorm(n = 1,mean = 0,sd = 1))/rgamma(1,1/2,1/2)
  }
  
  # Q_{1:n,1:n}
  q_mat <- Qmat(s = nu*s1,
                sigma_sqrd = sigma_sq_v[1],
                lambda = lambda1[,1:n_old],
                qs_alph = qalph,ncomp = n_old)
  # Q_{*,*}
  q_new <- Qmat(s = nu*s1,
                sigma_sqrd = sigma_sq_v[1],
                lambda = lambda1[,-(1:n_old)],
                qs_alph = qalph,
                ncomp = nys - n_old)
  # Q_{*,1:n}
  k_new <- k_mat(s = nu*s1,
                 lambda = lambda1,
                 n_obs = n_old,
                 qs_alph = qalph)
  
  ystar <- matrix(data = NA,nrow = n_sim,
                  ncol = nrow(xstar))
  y_nostar <- matrix(data = NA,nrow = n_sim,
                     ncol = nrow(x))
  ystar[1,] <- simulate_ystar(q_mat,q_new,k_new,y,n_old = n_old)
  y_nostar[1,] <- simulate_y_nostar(q_mat,y,n_old = n_old,
                                    sigma_sq = sigma_sq_v[1])
  s_matrix <- matrix(NA,nrow = n_sim,ncol = n_comp)
  s_matrix[1,] <- s1
  # log likelihood for traceplots and MCMC diagnostics
  log_lik0 <- rep(NA,n_sim)
  print('\n')
  pb <- txtProgressBar(min = 2,max = n_sim)
  for(t0 in 2:n_sim){
    setTxtProgressBar(pb,value = t0)
    # sample scales from the prior
    s_prop <- ralpha_stable_true0(n=n_comp,
                                  alpha = alpha0/2,
                                  beta = 1,scale = 1)
    
    # accept/reject the drawn scales samples
    s_ret<- simulate_sks(s_matrix[t0-1,],
                         s_news = s_prop,nu=nu,
                         q_mat = q_mat,y = y,
                         qs = qalph,sigma_sqrd = sigma_sq,
                         lambda_mat = lambda1,
                         ncomp = nys,
                         n_obs = n_old,
                         full_array = full_array)
    # updates accordingly, and saves outputs
    s_matrix[t0,] <- s_ret$s
    q_mat <- s_ret$q
    log_lik <- s_ret$log_lik
    
    # sigma_sq sample
    if(!known_sigma){
      
      prop_sigmasq <- abs(rnorm(n = 1,mean = 0,sd = 1))/rgamma(1,1/2,1/2)
      sigma_ret <- simulate_sigmasq(prop_sigmasq,sigma_sq_v[t0-1],
                                    y,
                                    q_mat,
                                    n_obs = n_old,
                                    old_log_lik=log_lik)
      sigma_sq_v[t0] <- sigma_ret$sigma_sq
      q_mat <- sigma_ret$q
      log_lik0[t0] <- sigma_ret$log_lik
    } else {
      log_lik0[t0] <- log_lik
    }
    # computes Q_{*,*} based on the sampled values of s and sigma
    q_new <- Qmat(s = nu*s_matrix[t0,],
                  sigma_sqrd = sigma_sq_v[t0],
                  lambda = lambda1[,-(1:n_old)],
                  qs_alph = qalph,
                  ncomp = nys - n_old)
    # computes Q_{*,1:n} based on the sampled values of s and sigma
    k_new <- k_mat(s = nu*s_matrix[t0,],
                   lambda = lambda1,
                   n_obs = n_old,
                   qs_alph = qalph)
    # samples from the conditional normal
    ystar[t0,] <- simulate_ystar(q_mat,q_new,k_new,y,n_old = n_old)
    y_nostar[t0,] <- simulate_y_nostar(q_mat,y,n_old = n_old,
                                       sigma_sq = sigma_sq_v[t0])
  }
  
  # returns results
  return(list(y_pred = ystar,
              y_no_pred = y_nostar,
              sigma_sq_v = sigma_sq_v,
              log_lik = log_lik0))
}