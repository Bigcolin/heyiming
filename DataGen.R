library(TSA)
#### simulation ####
#生成特定的随机数
yt<-function(n){
    y<-c()
    set.seed(1234567)
    error = garch.sim(alpha = c(0.5,0.1),beta =0.1, n)
    E<-runif(5,-1,1)
    x1<-runif(n,-1,1)
    x2<-runif(n,-1,1)
    x3<-runif(n,-1,1)
    x4<-runif(n,-1,1)
    for(i in 2:n){
      z<-1/3*x1[i-1]+2/3*x2[i-1]+2/3*x4[i-1]
      g1 = E[1]+3*exp(-z^2)
      a1 = E[2]+0.8*z
      a2 = E[3]
      a3 = E[4]+1.5*sin(pi*z)
      a4 = E[5]
      y[i-1] = g1 + a1*x1[i] + a2*x2[i] + a3*x3[i]+ a4*x4[i] + error[i]
    }

    x1t = x1[2:n]
    x2t = x1[2:n]
    x3t = x1[2:n]
    x4t = x1[2:n]
    data <-data.frame(y, x1t,x2t,x3t,x4t)

    return(data)
}
# data <- yt(40)
# y <- data[,1]
# X <- t(data[,2:5])
