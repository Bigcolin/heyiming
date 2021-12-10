#### Kernal ####
K <- function(x){
  if(abs(x)<=1){
    k <- 3/4*(1-x^2)
  }else{
    k = 0
  }
  return(k)
}

K.d <- function(x){
  if(abs(x)<=1){
    k.d <- -3/2*x
  }else{k.d=0}
  return(k.d)
}
#### functions ####
chi <- function(X,b,u){
  X <- t(X)
  Chi <- matrix(NA, ncol = 10, nrow = nrow(X)-1)
  for (i in 1: (nrow(X)-1)){
    Chi[i,]<- c(X[i+1,],1,X[i,]%*%b-u,(X[i,]%*%b-u)%*%X[i+1,])
  }
  return(Chi)
}


w <- function(X,b,u,h){
  w <- numeric(ncol(X)-1)
  for(i in 1:(ncol(X)-1)){
    w[i]<- K((t(X[,i])%*%b-u)/h)
  }
  return(w*diag(ncol(X)-1))
}
w.d <- function(X,b,u,h){
  w <- numeric(ncol(X)-1)
  for(i in 1:(ncol(X)-1)){
    w[i]<- K.d((t(X[,i]%*%b)-u)/h)
  }
  return(w*diag(ncol(X)-1))
}

a <- function(X,b,u,h){
  Chi <- chi(X,b,u)
  W <- w(X,b,u,h)
  q <- nrow(X)
  I <- c(rep(1,q),rep(0,q+2))
  SVD <- svd(t(Chi)%*%W%*%Chi)
  SVD$d[SVD$d<10^(-4)] <- 0
  inverse <- 1/SVD$d*diag(1,length(SVD$d))
  inverse[is.na(inverse)] <- 0
  inverse[is.infinite(inverse)] <- 0
  SVD <- SVD$v%*%inverse%*%t(SVD$u)
  result <- I%*%SVD%*%t(Chi)%*%W%*%y[-1]
  return(result)
}

a.d <- function(X,b,u,h){
  q <- nrow(X)
  n <- ncol(X)
  Z <- matrix(0,ncol=1+q,nrow=n-1)
  Z <- cbind(Z,rep(-1,n-1))
  Z <- cbind(Z,-t(X[,2:n]))
  I <- c(rep(1,q),rep(0,q+2))
  W.d <- w.d(X,b,u,h)
  Chi <- chi(X,b,u)
  SVD <- svd(t(Z)%*%W.d%*%Z)
  SVD$d[SVD$d<10^(-4)] <- 0
  inverse <- 1/SVD$d*diag(1,length(SVD$d))
  inverse[is.na(inverse)] <- 0
  inverse[is.infinite(inverse)] <- 0
  SVD <- SVD$v%*%inverse%*%t(SVD$u)
  result <- I%*% SVD%*%t(Chi)%*%W.d%*%y[-1]
  return(result)
}

g <- function(X,b,u,h){
  Chi <- chi(X,b,u)
  W <- w(X,b,u,h)
  q <- nrow(X)
  I <- matrix(c(rep(1,q),1,rep(1,q+1)),ncol=10)
  SVD <- svd(t(Chi)%*%W%*%Chi)
  SVD$d[SVD$d<10^(-4)] <- 0
  inverse <- 1/SVD$d*diag(1,length(SVD$d))
  inverse[is.na(inverse)] <- 0
  inverse[is.infinite(inverse)] <- 0
  SVD <- SVD$v%*%inverse%*%t(SVD$u)
  result <- I%*%SVD%*%t(Chi)%*%W%*%y[-1]
  return(result)
}

g.d <- function(X,b,u,h){
  q <- nrow(X)
  n <- ncol(X)
  Chi <- chi(X,b,u)
  Z <- matrix(0,ncol=1+q,nrow=n-1)
  Z <- cbind(Z,rep(-1,n-1))
  Z <- cbind(Z,-t(X[,2:n]))
  I <- c(rep(1,q),1,rep(1,q+1))
  W.d <- w.d(X,b,u,h)
  SVD <- svd(t(Z)%*%W.d%*%Z)
  SVD$d[SVD$d<10^(-4)] <- 0
  inverse <- 1/SVD$d*diag(1,length(SVD$d))
  inverse[is.na(inverse)] <- 0
  inverse[is.infinite(inverse)] <- 0
  SVD <- SVD$v%*%inverse%*%t(SVD$u)
  result <- I%*% SVD%*%t(Chi)%*%W.d%*%y[-1]
  return(result)
}

L <- function(b,y,X){
  b <- as.matrix(b)
  b <- b/sqrt(sum(b^2))
  if(b[1,1]<0){b[1,1] <- -b[1,1]}
  n <- ncol(X)
  res <- 0
  h <- 0.2*(max(t(X) %*% b)-min(t(X) %*% b))
  for(j in 1:(n-1)){
    for(i in 2:n){
       z <- t(X[,j]%*%b)
       p1 <- g(X,b,z,h)+g.d(X,b,z,h)*t(X[,i-1]-X[,j])%*%b
       p2 <- sum(a(X,b,z,h)%*%X[,i])+sum(a.d(X,b,z,h)%*%(t(X[,i-1]-X[,j])%*%b)%*%X[,i])
       res <- res + sqrt((y[i]-p1-p2)^2)*K(t(X[,i-1]-X[,j])%*%b/h)
    }
  }
  return(res)
}
