obs1 <- c(0,3,0)
obs2 <- c(2,0,0)
obs3 <- c(0,1,3)
obs4 <- c(0,1,2)
obs5 <- c(-1,0,1)
obs6 <- c(1,1,1)
obs0 <- c(0,0,0)
dist1 = sqrt(sum((obs1 - obs0)^2))
dist2 = sqrt(sum((obs2 - obs0)^2))
dist3 = sqrt(sum((obs3 - obs0)^2))
dist4 = sqrt(sum((obs4 - obs0)^2))
dist5 = sqrt(sum((obs5 - obs0)^2))
dist6 = sqrt(sum((obs6 - obs0)^2))
dist1
dist2
dist3
dist4
dist5
dist6
x <- data.frame(
  bias = c(620,580,420,210,90,50),
  vari = c(0,10,25,90,185,320),
  testerr = c(800,780,615,410,400,500),
  trainerr = c(650,605,500,360,210,180),
  bayes = c(210,210,210,210,210,210)
)
x
xrange <- 1:nrow(x)
plot(0,0,type = "n",xlim = range(xrange),ylim = range(0,x),xlab = "flexbility",ylab = "MSE")
cols <- c("red","orange","green","gray","blue")
for (i in 1:ncol(x)) {
    lines(xrange,x[,i],col=cols[i])
}
legend("topright",legend = colnames(x),col = cols)
png("ch2g.png",width = 800, height = 600)
lines(x,fitted(x),col=cols)
dev.off()
