library(ggplot2)
library(psych)
HairEyeColor
male <- as.data.frame(HairEyeColor[,,1])
female <- as.data.frame(HairEyeColor[,,2])

#построение графиков
require(graphics)
mosaicplot(HairEyeColor)

ggplot(male,aes(x = Hair, y = Freq, col = Eye))+
  geom_point(alpha = 1)
ggplot(female,aes(x = Hair, y = Freq, col = Eye))+
  geom_point(alpha = 1)

ggplot(male,aes(x = Hair, y = Freq, col = Eye))+
  geom_col()
ggplot(female,aes(x = Hair, y = Freq, col = Eye))+
  geom_col()

t <- as.data.frame(cbind(male,"FreqF"=female$Freq))
ggplot(t,aes(x = t$Hair, y = Freq+FreqF, col = Eye))+
  geom_point(alpha = 1)

#-------------------

fit1 <- cor.test(male$Freq,female$Freq) #коэффициент корреляции и оценка статистической значимости коэффициента, проверка нулевую гипотезы о равенстве его нулю.
fit1
fit2 <- cor.test(HairEyeColor[,,"Male"],HairEyeColor[,,"Female"])
fit2

dt <-cbind("MaleFr" = male$Freq, "FemFr" = female$Freq)
pairs(dt, panel = panel.smooth) #сглаживающую кривую
pairs(HairEyeColor, panel = panel.smooth) #Матрицы графиков рассеяния

cor(dt) #Матрица кор

library("psych")
fit3 <- corr.test(dt)
fit3$r #значения корреляций
fit3$p #p-уровень значимости

#построение регрессионной модели
dt2 <- cbind(male,FreqF = female[,3])
fit4 <-lm(Freq ~ FreqF,dt2)
fit4
coef(fit4)
summary(fit4)
ggplot(dt2,aes(Freq,FreqF,col = Eye))+
  geom_point(size = 1)+
  geom_smooth(method = "lm")

fit4$fitted.values
fitted_values <-data.frame(Freq = dt2$Freq, fitted = fit4$fitted.values)
fitted_values
