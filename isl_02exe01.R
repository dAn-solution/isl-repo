# 2.4演習問題　ー応用編ー
# (8) (a)
college=read.csv("/home/dan/isl_lib/ISL_02/College.csv")
str(college)
# (8) (b)
fix(college)
college[1:3,]
rownames(college)=college[,1]
fix(college)
college[1:3,]
college = college[,-1]
fix(college)
college[1:3,]
names(college)
# (8) (c)
# i.
summary(college)
# ii.
pairs(college[,1:10])
# iii.
attach(college)
boxplot(Outstate~Private,xlab = "Private",ylab = "Outstate")
# iv.
Elite = rep("No",nrow(college))
Elite
Elite[Top10perc>50]="Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)
summary(college)
boxplot(Outstate~Elite,xlab = "Elite",ylab = "Outstate")
# v.
par(mfrow=c(2,2))
hist(Apps,breaks = 50,xlim = c(0,20000),main = "Apps")
hist(Enroll,breaks = 25,main = "Enroll")
hist(Expend,breaks = 25,main = "Expend")
hist(Outstate,main = "Outstate")
# vi. 
# 寄付する学生割合を調査
par(mfrow=c(1,1))
hist(perc.alumni,main = "perc.alumni")
# 50%以上の大学をマーク
Dona = rep("No",nrow(college))
Dona[perc.alumni>=50]="Yes"
Dona = as.factor(Dona)
college = data.frame(college,Dona)
summary(Dona)
# 30%以上の大学をマーク
college <- college[,colnames(college) != "Dona"]
Dona = rep("No",nrow(college))
Dona[perc.alumni>=30]="Yes"
Dona = as.factor(Dona)
college = data.frame(college,Dona)
summary(Dona)
# 寄付率と博士号取得率
library("ggplot2")
qplot(college$perc.alumni,college$PhD)
cor(college$perc.alumni,college$PhD)
# 寄付率と高校成績がtop10のもの
qplot(college$perc.alumni,college$Top10perc)
cor(college$perc.alumni,college$Top10perc)
# 寄付率と州外授業料
qplot(college$perc.alumni,college$Outstate)
cor(college$perc.alumni,college$Outstate)
# 図の保存
library(tidyverse)
ggplot(data = college, mapping = aes(x = perc.alumni, y = PhD)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
g <- ggplot(data = college, mapping = aes(x = perc.alumni, y = PhD)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_8c1.png",
       dpi = 300, height = 20, width = 20, units = "cm")
ggplot(data = college, mapping = aes(x = perc.alumni, y = Top10perc)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
g <- ggplot(data = college, mapping = aes(x = perc.alumni, y = Top10perc)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_8c2.png",
       dpi = 300, height = 20, width = 20, units = "cm")
ggplot(data = college, mapping = aes(x = perc.alumni, y = Outstate)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
g <- ggplot(data = college, mapping = aes(x = perc.alumni, y = Outstate)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_8c3.png",
       dpi = 300, height = 20, width = 20, units = "cm")
# (9) (a)
Auto= read.csv("/home/dan/isl_lib/ISL_02/Auto.csv",header = T,na.strings = "?")
fix(Auto)
table(is.na(Auto))
Auto = na.omit(Auto)
Auto[1:5,]
fix(Auto)
str(Auto)
# (9) (b)
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)
sapply(Auto[,-(8:9)],range)
# (9) (c)
sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)
# (9) (d)
tmp <- Auto[,-(8:9)]
tmp <- tmp[-(10:85),]
sapply(tmp,range)
sapply(tmp,mean)
sapply(tmp,sd)
# (9) (e) 
pairs(Auto[,(1:7)])
png("ch2_9e.png", width = 800, height = 600)
pairs(Auto[,(1:7)])
dev.off()
# (10) (a)
library(MASS)
print(Boston)
?Boston
dim(Boston)
str(Boston)
# (10) (b)
pairs(Boston)
png("ch2_10.png", width = 800, height = 600)
pairs(Boston)
dev.off()
# 以下のパッケージが有効とのことだったが何故かインストール出来ず
# install.packages("psych")
# library("psych")
# pairs.panels(Boston) 
# https://stackoverflow.com/questions/62846405/not-able-to-install-mnormt-package-in-r-3-6-2
# 上記を実行したがRのversionが4.xでなければならないとのこと
ggplot(data = Boston, mapping = aes(x = lstat, y = crim)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
cor(Boston$crim,Boston$lstat)
ggplot(data = Boston, mapping = aes(x = rm, y = medv)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
cor(Boston$rm,Boston$medv)
ggplot(data = Boston, mapping = aes(x = nox, y = rad)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
cor(Boston$nox,Boston$rad)
summary(Boston$nox)
summary(Boston$rad)
# (10)(b) データ保存
g <- ggplot(data = Boston, mapping = aes(x = lstat, y = crim)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_10b1.png",
       dpi = 300, height = 20, width = 20, units = "cm")
g <- ggplot(data = Boston, mapping = aes(x = rm, y = medv)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_10b2.png",
       dpi = 300, height = 20, width = 20, units = "cm")
g <- ggplot(data = Boston, mapping = aes(x = nox, y = rad)) +
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_10b3.png",
       dpi = 300, height = 20, width = 20, units = "cm")
# (10)(c)
library(reshape2)
bosmelt = melt(Boston,id="crim")
ggplot(bosmelt, aes(x=value, y=crim)) +
  facet_wrap(~variable, scales="free") + 
  geom_point()
cor(Boston, use="complete.obs")[1,]
# 保存
g <- ggplot(bosmelt, aes(x=value, y=crim)) +
  facet_wrap(~variable, scales="free") + 
  geom_point()
ggsave(plot = g, filename = "ch2_10c1.png",
       dpi = 300, height = 20, width = 20, units = "cm")
# (10) (d)
ggplot(Boston, aes(x=1:nrow(Boston), y=crim)) + 
  geom_point()
cnt <- 0
for (i in 1:nrow(Boston)) {
    if (Boston[i,1] > mean(Boston$crim))
      cnt = cnt + 1
}
cnt / nrow(Boston)
ggplot(Boston, aes(x=1:nrow(Boston), y=tax)) + 
  geom_point()
cnt <- 0
for (i in 1:nrow(Boston)) {
  if (Boston[i,10] > 600)
    cnt = cnt + 1
}
cnt / nrow(Boston)
ggplot(Boston, aes(x=1:nrow(Boston), y=ptratio)) + 
  geom_point()
summary(Boston$ptratio)
cnt <- 0
for (i in 1:nrow(Boston)) {
  if (Boston[i,11] > 20.2)
    cnt = cnt + 1
}
cnt / nrow(Boston)
# 保存
g <- ggplot(Boston, aes(x=1:nrow(Boston), y=crim)) + 
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_10d1.png",
       dpi = 300, height = 20, width = 20, units = "cm")
g <- ggplot(Boston, aes(x=1:nrow(Boston), y=tax)) + 
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_10d2.png",
       dpi = 300, height = 20, width = 20, units = "cm")
g <- ggplot(Boston, aes(x=1:nrow(Boston), y=ptratio)) + 
  theme_classic(base_size = 25, base_family = "serif") +
  geom_point()
ggsave(plot = g, filename = "ch2_10d3.png",
       dpi = 300, height = 20, width = 20, units = "cm")
# (10) (e)
table(Boston$chas)
# (10) (f)
median(Boston$ptratio)
# (10) (g)
min(Boston$medv)
Boston[Boston$medv == min(Boston$medv),]
sapply(Boston,quantile)
# (10)(h)
nrow(Boston[Boston$rm >= 7,])
nrow(Boston[Boston$rm >= 8,])
rbind(sapply(Boston[Boston$rm > 8,], mean), sapply(Boston, mean))
