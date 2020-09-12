# 4.6 実習
# 4.6.1 株価データ
library(ISLR)
names(Smarket)
fix(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[-9])
plot(Smarket$Year,Smarket$Volume)
plot(Smarket$Volume)

# 4.6.2 ロジスティック回帰
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs = predict(glm.fits,type = "response")
glm.probs[1:10]
contrasts(Smarket$Direction)
glm.pred = rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Smarket$Direction)
(507+145)/1250
mean(glm.pred==Smarket$Direction)
train = (Smarket$Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Smarket$Direction[!train]
dim(Direction.2005)

glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial,subset = train)
glm.probs = predict(glm.fits,Smarket.2005,type = "response")
glm.pred = rep("Down",252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fits = glm(Direction~Lag1+Lag2,data = Smarket,family = binomial,subset = train)
glm.probs = predict(glm.fits,Smarket.2005,type = "response")
glm.pred = rep("Down",252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred,Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)
predict(glm.fits,newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type = "response")

# 4.6.3 線形判別分析
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2,data = Smarket,subset = train)
lda.fit
plot(lda.fit)
