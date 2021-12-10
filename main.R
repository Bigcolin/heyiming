source("DataGen.R") # simulation.R 改了名字，数据生成 DGP
source("functions.R") # 功能函数

t0 <- Sys.time()

data <- yt(40)
y <- data[,1]
X <- t(data[,2:5])

#### optimization ####
b <- c(0.5,0.5,0.5,0.5)
# 你这b是初始值的话，true model的参数在哪呢，如何对比？
# 我看你的DGP里模型的参数每个n都在变，是否有问题

res = optim(par=c(0.5,0.5,0.5,0.5),L,y=y,X=X)
t1 <- Sys.time()
t = t1 - t0


print(t)
cat('Optim. coef. result : ')
cat(res$par, "\n")