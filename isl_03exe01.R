# 3.7 演習問題
# (8)
Auto= read.csv("/home/dan/isl_lib/ISL_02/Auto.csv",header = T,na.strings = "?")
lm.fit <- lm(mpg~horsepower,data = Auto)
lm.fit
summary(lm.fit)
# (8) (a)(vi)
new_mpg <- data.frame(horsepower = 98)
predict(lm.fit, new_mpg)
predict(lm.fit, new_mpg, interval = "confidence", level = 0.95) 
predict(lm.fit, new_mpg, interval = "prediction", level = 0.95)
# (8) (b)
plot(Auto$horsepower,Auto$mpg)
abline(lm.fit, col="red")
png("ch3_8b.png", width = 800, height = 600)
dev.off()
# (8) (c)
par(mfrow=c(2,2))
plot(lm.fit)
png("ch3_8c.png", width = 800, height = 600)
dev.off()

# (9) (a)
pairs(Auto)
# (9) (b)
cor(subset(Auto,select = -name))
table(is.na(Auto))  # NA件数を表示
Auto[!complete.cases(Auto),] # NAのレコードを表示
Auto <- subset(Auto, complete.cases(Auto)) # NAレコードを削除
cor(subset(Auto,select = -name))　# 再度相関係数を計算
# (9) (c)
lm.fit <- lm(mpg~.-name,data = Auto)
summary(lm.fit)
# (9) (d)
par(mfrow=c(2,2))
plot(lm.fit)
predict_value <- predict(lm.fit)
residual_err <- residuals(lm.fit)
Auto.f <- data.frame(Auto,predict_value,residual_err)
Auto.f[14,]
Auto.f[323,]
Auto.f[326,]
Auto.f[327,]
summary(Auto.f)
# (9) (e)
fit.lm01 <- lm(mpg~weight+displacement*horsepower,data=Auto)
fit.lm02 <- lm(mpg~horsepower+displacement*weight,data=Auto)
fit.lm03 <- lm(mpg~displacement+weight*horsepower,data=Auto)
summary(fit.lm01)
summary(fit.lm02)
summary(fit.lm03)

# (10)
library(ISLR)
data("Carseats")
names(Carseats)
# (10) (a)
sales.lm <- lm(Sales~Price+Urban+US,data = Carseats)
summary(sales.lm)
# (10) (e)
sales.lm01 <- lm(Sales~Price+US,data = Carseats)
summary(sales.lm01)
# (10) (g)
confint(sales.lm01)
# (10) (h)
par(mfrow=c(2,2))
plot(sales.lm01)
par(mfrow=c(1,1))
plot(sales.lm01)

# (11)
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
# (11) (a)
Y.lm <- lm(y~x+0)
summary(Y.lm)
# (11) (b)
X.lm <- lm(x~y+0)
summary(X.lm)
# (11) (e)
x.sum2 <- sum(x^2)
y.sum2 <- sum(y^2)
xy.sum <- sum(x*y)
n <- 100
(sqrt(n-1) * xy.sum) / (sqrt((x.sum2 * y.sum2) - ((xy.sum)^2)))
# (11) (f)
Y.lm02 <- lm(y~x)
summary(Y.lm02)
X.lm02 <- lm(x~y)
summary(X.lm02)
#（12）(c)
set.seed(1)
x=rnorm(100)
y=x
fit.lm4 <- lm(y~x+0)
fit.lm5 <- lm(x~y+0)
summary(fit.lm4)
summary(fit.lm5)

# (13) (a)
set.seed(1)
x <- rnorm(100,mean = 0,sd = 1)
# (13) (b)
eps <- rnorm(100,mean = 0,sd = 0.25^0.5)
# (13) (c)
y <- -1 + 0.5*x + eps 
length(y)
# (13) (d)
plot(x,y)
# (13) (e)
fit.lm6 <- lm(y~x)
summary(fit.lm6)
# (13) (f)
plot(x,y)
abline(-1, 0.5, col="blue") #(c)で示された真のモデル
abline(fit.lm6, col="red") 　#(e)の最小2条モデル
legend(x = c(0.1,2.7),
       y = c(-2.5,-1.5),
       legend = c("真モデル", "最小2乗モデル"),
       col = c("blue","red"), lwd=2 )
# (13) (g)
fit.lm7 <- lm(y~x+I(x^2))
summary(fit.lm7)
#（13）(h)
eps2 <- rnorm(100, sd=0.1)　#ノイズ（標準偏差）を小さくする
y2 <- -1 + 0.5*x + eps2
fit.lm8 <- lm(y2 ~ x)
summary(fit.lm8)
# (13) (i)
eps3 <- rnorm(100, sd=1)　#ノイズ（標準偏差）を大きくする
y3 <- -1 + 0.5*x + eps3
fit.lm9 <- lm(y3 ~ x)
summary(fit.lm9)
# (13) (j)
confint(fit.lm6) #元データ
confint(fit.lm8) #小さいノイズ
confint(fit.lm9) # 大きいノイズ

#（14）(a)
set.seed(1)
x1 = runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2*rnorm(100)
# (14) (b)
cor(x1,x2)
plot(x1,x2)
# (14) (c)
fit.lm10 <- lm(y~x1+x2)
summary(fit.lm10)
#（14）(d)
fit.lm11 <- lm(y~x1)
summary(fit.lm11)
#（14）(e)
fit.lm12 <- lm(y~x2)
summary(fit.lm12)
# (14) (g)
X1 = c(x1,0.1)
x2 = c(x2,0.8)
y = c(y,6)
par(mfrow=c(2,2))
fit.lm13 <- lm(y~x1+x2)
fit.lm14 <- lm(y~x1)
fit.lm15 <- lm(y~x2)
summary(fit.lm13)
summary(fit.lm14)
summary(fit.lm15)
plot(fit.lm13)
plot(fit.lm14)
plot(fit.lm15)

# (15) (a)
fix(Boston)
names(Boston)
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") 
    stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
results <- combn(names(Boston), 2, 
                 function(x) { lmp(lm(Boston[, x])) }, 
                 simplify = FALSE)
vars <- combn(names(Boston), 2)
names(results) <- paste(vars[1,],vars[2,],sep="~")
results[1:13]
# (15) (b)
fit.lm <- lm(crim~.,data=Boston)
summary(fit.lm)
# (15) (c)
results <- combn(names(Boston), 2, 
                 function(x) { coefficients(lm(Boston[, x])) }, 
                 simplify = FALSE)
(coef.uni <- unlist(results)[seq(2,26,2)])
(coef.multi <- coefficients(fit.lm)[-1])
plot(coef.uni, coef.multi)
# (15) (d)
summary(lm(crim~poly(zn,3), data=Boston)) 