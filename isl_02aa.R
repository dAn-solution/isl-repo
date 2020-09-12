x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x + y
ls()
rm(g,lev,n,opar,ple.sales)
rm(pie.sales,pin,scale,seki,usr,x,xadd,vdelta)
rm(xdelta,xcale,xx,y,yadd)
rm(xscale,ydelta,yscale,yy)
ls()
rm(list = ls())
ls()
?matrix
x=matrix(data = c(1,2,3,4),nrow = 2,ncol = 2)
x
x=matrix(data = c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE)
x
sqrt(x)
x^2
x=rnorm(50)
x
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
set.seed(1303)
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)
x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab = "x軸",ylab = "y軸",main = "XとYのプロット")
pdf("/home/dan/isl_lib/ISL_02/Figure.pdf")
plot(x,y,col="green")
dev.off()
x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=50)
x
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels = 45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels = 15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta = 30,phi = 20)
persp(x,y,fa,theta = 30,phi = 70)
persp(x,y,fa,theta = 30,phi = 40)
A=matrix(1:16,4,4)
A
matrix(1:16,2,8)
A[2,3]
A[c(1,3),c(2,4)]
A[-c(1,3),]
A[c(1,3),]
dim(A)
Auto=read.table("/home/dan/isl_lib/ISL_02/Auto.data")
fix(Auto)
Auto=read.table("/home/dan/isl_lib/ISL_02/Auto.data",header = T,na.strings = "?")
fix(Auto)
Auto=read.csv("/home/dan/isl_lib/ISL_02/Auto.csv",header = T,na.strings = "?")
fix(Auto)
dim(Auto)
Auto[1:4,]
Auto[!complete.cases(Auto),]
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
plot(Auto$cylinders,Auto$mpg)
attach(Auto)
plot(cylinders,mpg)
cylinders=as.factor(cylinders)
plot(cylinders,mpg)
plot(cylinders,mpg,col="red")
plot(cylinders,mpg,col="red",varwidth=T)
plot(cylinders,mpg,col="red",varwidth=T,horizontal=T)
plot(cylinders,mpg,col="red",varwidth=T,xlab="cylinders",ylab="mpg")
hist(mpg)
hist(mpg,col=2)
hist(mpg,col = 2,breaks = 15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
identify(horsepower,mpg,name)
summary(Auto)
summary(mpg)
