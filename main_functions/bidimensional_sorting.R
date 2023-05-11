

### Implements the 2D sorting algorithm  --------------------
### by Goodman and Pollack (1983)

# P is a matrix with 2 columns
# and n rows
bidim_sort <- function(P){
  n <- nrow(P)
  x <- P[,1]
  y <- P[,2]
  lambda <- matrix(NA,n,n)
  list_points <- vector(mode = 'list',length = n)
  u_i <- rep(NA,2*n)
  v_i <- rep(NA,2*n)
  m_j <- rep(NA,2*n)
  for(i in 1:n){
    list_points[[i]] <- vector(mode = 'list',length = n)
    for(j in 1:n){
      u_i[j] <- x[i] - x[j]
      v_i[j] <- y[i] - y[j]
      if(u_i[j] == 0 & v_i[j] == 0 ){
        lambda[i,j] <- 'w'
      } else{
        lambda[i,j] <- 'good'
        u_i[n+j] <- -u_i[j]
        v_i[n+j] <- -v_i[j]
        m_j[j] <- m_j[n+j] <- v_i[j]/u_i[j]
      }
    }
    goods <- which(lambda[i,] =='good')
    goods <- c(goods,
               goods + n)
    suborder <- function(subseti,m){
      rsubseti <- sort.int(x=m[subseti],
                           index.return = T)
      tab <- table(rsubseti$x)
      order_m <- rsubseti$ix
      subset_new <- subseti[order_m]
      ## reorder using mi
      count0 <- 1
      sub1 <- vector('list',length = length(tab))
      for(k in seq_along(tab)){
        # print(count0)
        sub1[[k]] <- subset_new[count0:(count0 + tab[k]-1)]
        count0<- count0 + tab[k]
      }
      return(list(sub1,
                  subset_new))
    }
    subset1 <- (1:(2*n))[goods][(u_i[goods] > 0)]
    sub_order1 <- suborder(subset1,m_j)
    subset1 <- sub_order1[[2]]
    sub1<- sub_order1[[1]]
    
    subset2 <- (1:(2*n))[goods][ u_i[goods] == 0 & v_i[goods] >0]
    
    subset3 <- (1:(2*n))[goods][ u_i[goods] < 0]
    sub_order3 <- suborder(subset3,m_j)
    subset3 <- sub_order3[[2]]
    sub3 <- sub_order3[[1]]
    
    subset4 <- (1:(2*n))[goods][ u_i[goods] == 0 & v_i[goods] < 0]
    
    subsets0 <- (c(subset1,subset2,subset3,subset4))
    
    which_ray <- function(j){
      if(j %in% c(subset2,subset4)){
        j
      } else if(j %in% subset1){
        return(sub1[[which(unlist(lapply(sub1,x=j,FUN=`%in%`)))]])
      } else {
        return(sub3[[which(unlist(lapply(sub3,x=j,FUN=`%in%`)))]])
      }
    }
    
    # now only need to check which of the $j$s share the same subsets
    for(j in (i):n){
      if(j == i){
        # this is a "not good" case, by definition
      } else {
        k_j <- which(subsets0 == j)
        k_n_j <- which(subsets0 == n+j)
        
        # checks 
        if( length(k_j) == 0 | length(k_n_j) == 0){
          
        }
        if(k_j < k_n_j){
          all <- subsets0[(k_j+1):(k_n_j-1)]
        } else {
          all <- subsets0[c(1:(k_n_j - 1),
                            (k_j + 1):length(subsets0))]
          all <- all[!is.na(all)]
          
        } 
        # finds which of the points lie in the same ray
        same <- c(which_ray(j),i,which_ray(n+j))
        same <- same[same <= n]
        positives <- all[all <= n]
        negatives <- all[all > n] - n
        
        # list the points that lie in the same ray,
        # above the ray (positives), and under
        # the ray (negatives)
        
        list_points[[i]][[j]] <- 
          list(positives = setdiff(positives,same),
               negatives = setdiff(negatives,same),
               same = same)
      }
    }
  }
  return(list_points)
}


# finds the index of the values 
# that are repeated
# to be used like:
# revers_dupls(unique(P),P)
revers_dupls <- function(P_uni,P_rep){
  n_uni <- nrow(P_uni)
  n_dupls <- nrow(P_rep)
  reps <- list()
  for(index0 in 1:n_uni){
    rep_0 <- c()
    for( j in 1:n_dupls){
      rep_0 <- c(rep_0,
                 all(P_uni[index0,] == P_rep[j,]))
    }
    reps[[index0]] <- which(rep_0)
  }
  return(reps)
}


# fills the Lambda as a matrix
# with +1 when the values are positive
# and -1 when the values are negative.
# iterating  with the points that lie on the 
# same ray.
fill_lambda <- function(x,sorts){
  x0 <- cbind(1,x)
  n <- nrow(x0)

  # n*(n-1)*4 is an upper bound of the possible dimension
  # of Lambda.
  # in terms of the paper: L <= n*(n-1)*4
  Lambda_set <- matrix(NA,nrow = n,ncol = n*(n-1)*4)
  counter <- 1
  for(i in 1:n){
    for(j in i:n){
      # number of values that are in the same ray
      n_same <- length(sorts[[i]][[j]]$same)
      # need to add them both as positives and negatives
      if(j > i){
        for( k in 0:n_same){
          pos_same <- sorts[[i]][[j]]$same[0:k]
          neg_same <- setdiff(sorts[[i]][[j]]$same,
                              pos_same)
          # positives are +1
          Lambda_set[c(sorts[[i]][[j]]$positives,
                       pos_same),counter] <- 1
          # negatives are -1
          Lambda_set[c(sorts[[i]][[j]]$negatives,
                       neg_same),counter] <- -1
          counter <- counter + 1
        }
        
        
      }
      
    }
  }
  # discards repeated values:
  Lambda_set <- unique(t(Lambda_set))
  # adds the - tau, in case it was missing
  # and discards possible repetitions that
  # were added because of that.
  Lambda_set <- unique(
    rbind(Lambda_set,
          -Lambda_set)
  )
  # remove those that were not required
  Lambda_set <- na.omit(Lambda_set)
  return(Lambda_set)
}

# Performs the last part of the Algorithm 2.1, based
# on the need to have both the positive and negative
get_new_mat <- function(P){
  # makes sure that we only have unique values, so that 
  # we only have a single "good" in the original algorithm
  # and uses revers_dup to match the corresponding signs (+1,-1)
  # for each of the repeated points.
  uni_P <- unique(P)
  bsp <- bidim_sort(uni_P)
  rev_dup <- revers_dupls(uni_P,P)
  lambda_mat <- fill_lambda(uni_P,sorts = bsp)
  
  new_mat <- matrix(NA,nrow = nrow(lambda_mat),ncol = nrow(P))
  for(j in 1:ncol(lambda_mat)){
    new_mat[,rev_dup[[j]]] <- lambda_mat[,j]
  }
  return(new_mat)
}

