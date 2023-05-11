
qpis <- function(x){
  # appends the constant vector 1, to the matrix of inputs
  x0 <- cbind(1,x)
  n <- nrow(x0)
  # gets the sorting of the input values
  Lambda_set <- get_new_mat(x)
  zij <- vector('numeric',length = nrow(Lambda_set))
  
  pb <- txtProgressBar(min = 1,max = nrow(Lambda_set))
  # if parallelization is available it is much faster:
  if(require(parallel)){
    xx0 <- x0 %*% t(x0)
    d0 <- dim(xx0)[1]
    # function to make the computation,
    # uses the formula obtained in Section 2.1 of the paper
    # for the covariance matrix, using matrix operations to
    # compute it.
    get_zij <- function(cl,d0,Lambda_set,xx0){
      parSapply(
        cl,X = 1:nrow(Lambda_set),
        function(i){
          mvtnorm::pmvnorm(lower = rep(0,d0),
                  upper = rep(Inf,d0),
                  sigma = t(Lambda_set[i,] * 
                              t(Lambda_set[i,] * 
                                  xx0)))})
    }
    cl1 <- makeCluster(min(12,detectCores()))
    zij <- get_zij(cl1,d0,Lambda_set,xx0)
    stopCluster(cl1)
  } else {
    print('Not parallelizing the computation of the qpis\nFor a faster implementation install the "parallel" package')

    for(i in 1:nrow(Lambda_set)){
      setTxtProgressBar(pb,i)

      zij[i] <- mvtnorm::pmvnorm(lower = rep(0,d0),
                                 upper = rep(Inf,d0),
                                 sigma = t(Lambda_set[i,] * 
                                             t(Lambda_set[i,] * xx0)))
    }

  }
  return(list(probs = zij,
              lambda = Lambda_set))
}


# Explicit computation of q_pi when I = 1, following Section S1
qpis_1d <- function(x){
  n <- length(x)
  # first sort them, and then compute the rest
  rank_x <- rank(x)
  ordered_x <- sort(x)
  
  lambda <- matrix(1,nrow = n,ncol = n)
  lambda[lower.tri(lambda,diag = T)] <- -1
  
  q_pi <- diff(atan(ordered_x))/pi
  # probability of a + b x_n = -1, plus
  # probability of a + b x_1 = 1 
  # => all are positive or all are negative
  # since we are taking the square its the same!
  q_pi[n] <- 1+(atan(ordered_x[n]) -atan(ordered_x[1]))/pi
  return(list(probs = q_pi[rank_x],
              lambda = lambda[,rank_x]))
}
 