# 3.6.1 ライブラリ
library(MASS)
install.packages("ISLR")
library(ISLR)
# 3.6.2 線形単回帰
fix(Boston)
names(Boston)
?Boston
lm.fit=lm(medv~lstat,data=Boston)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "prediction")
plot(Boston$lstat,Boston$medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(Boston$lstat,Boston$medv,col="red")
plot(Boston$lstat,Boston$medv,pch=20)
plot(Boston$lstat,Boston$medv,pch="+")
plot(1:20,1:20,pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
# 3.6.3 線形重回帰
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data = Boston)
summary(lm.fit)
?summary.lm
summary(lm.fit)$r.sq
summary(lm.fit)$sigma
install.packages("car")
library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age,data = Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit,~.-age)
# 3.6.4 交互作用項
summary(lm(medv~lstat*age,data = Boston))
# 3.6.5 予測変数の非線形変換
lm.fit2=lm(medv~lstat+I(lstat^2),data = Boston)
summary(lm.fit2)
lm.fit=lm(medv~lstat,data = Boston)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5),data = Boston)
summary(lm.fit5)
summary(lm(medv~log(rm),data = Boston))
# 3.6.6 質的な予測変数
fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising + Price:Age,data = Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)
?contrasts
# 3.6.7 関数の定義
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
