source("DataGen.R")
source("functions.R")

t0 <- Sys.time()

data <- yt(40)
y <- data[,1]
X <- t(data[,2:5])

#### optimization ####
b <- c(0.5,0.5,0.5,0.5)
res = optim(par=c(0.5,0.5,0.5,0.5),L,y=y,X=X )

t1 <- Sys.time()

t = t1 - t0