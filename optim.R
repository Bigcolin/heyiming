#### local discrepancy function ####
L <- function(b,y,X){
  b <- as.matrix(b)
  b <- b/sqrt(sum(b^2))
  if(b[1,1]<0){b[1,1] <- -b[1,1]}
  n <- ncol(X)
  sum <- 0
  h <- 0.2*(max(t(X) %*% b)-min(t(X) %*% b))
  for(j in 1:(n-1)){
    for(i in 2:n){
       z <- t(X[,j]%*%b)
       p1 <- g(X,b,z,h)+g.d(X,b,z,h)*t(X[,i-1]-X[,j])%*%b
       p2 <- sum(a(X,b,z,h)%*%X[,i])+sum(a.d(X,b,z,h)%*%(t(X[,i-1]-X[,j])%*%b)%*%X[,i])
       sum <- sum + sqrt((y[i]-p1-p2)^2)*K(t(X[,i-1]-X[,j])%*%b/h)
    }
  }
  return(sum)
}
#### optimization ####
b <- c(0.5,0.5,0.5,0.5)
optim(par=c(0.5,0.5,0.5,0.5),L,y=y,X=X )

