###
# Taller 3

library(tidyverse)
library(readxl)
setwd("~/Documents/GitHub/semestre6_git/econometria_financiera/talleres/taller_3/bases_datos")

#Importación de los datos
Syp500_daily=read_excel("SyP500 daily.xlsx")
Syp500_weekly=read_excel("SyP500 weekly.xlsx")
Syp500_month=read_excel("SyP500 month.xlsx")

#---- Gráficos ----

#Gráficos de precio

Conf3x2 = matrix(c(1:3), nrow=1,byrow=TRUE)
layout(Conf3x2)

close_day=Syp500_daily[,2]
close_day=as.matrix(close_day)
plot(close_day,type="l",col="red",lwd=2,ylab="Adjusted close",main="Daily closing price of SyP500")

close_weekly=Syp500_weekly[,2]
close_weekly=as.matrix(close_weekly)
plot(close_weekly,type="l",col="red",lwd=2,ylab="Adjusted close",main="Weekly closing price of SyP500")

close_month=Syp500_month[,2]
close_month=as.matrix(close_month)
plot(close_month,type="l",col="red",lwd=2,ylab="Adjusted close",main="Month closing price of SyP500")

#Gráficos de retornos

Conf3x2 = matrix(c(1:3), nrow=1,byrow=TRUE)
layout(Conf3x2)

return_month=diff(log(close_month),lag=1)
plot(return_month,type="l",col="green",lwd=1,ylab="Returns",main="Retornos mensuales S&P500")
abline(h = 0)

return_weekly=diff(log(close_weekly),lag=1)
plot(return_weekly,type="l",col="green",lwd=1,ylab="Returns",main="Retornos semanales S&P500")
abline(h = 0)

return_day=diff(log(close_day),lag=1)
plot(return_day,type="l",col="green",lwd=1,ylab="Returns",main="Retornos diarios S&P500")
abline(h = 0)

###
# Hechos estilizados

#---- Hecho estilizado 1 retornos diarios ----

#1. Daily returns show weak autocorrelations
Conf3x2 = matrix(c(1:3), nrow=1,byrow=TRUE)
layout(Conf3x2)

acf(return_month,main='ACF mensual')
acf(return_weekly,main='ACF semanal')
acf(return_day,main='ACF diaria')

#---- Hecho estilizado 2 retornos diarios ----

#2. Returns are not normal distribution
Conf3x2 = matrix(c(1:3), nrow=1,byrow=TRUE)
layout(Conf3x2)

n=length(return_month)
hist(return_month,freq=F,breaks=20, main = "Retornos mensuales")
lines(density(return_month),col="green")
lines(seq(min(return_month),max(return_month),length=n),dnorm(seq(min(return_month),max(return_month),length=n),mean(return_month),sd(return_month)),col="blue")

n=length(return_weekly)
hist(return_weekly,freq=F,breaks=20, main = "Retornos semanales")
lines(density(return_weekly),col="green")
lines(seq(min(return_weekly),max(return_weekly),length=n),dnorm(seq(min(return_weekly),max(return_weekly),length=n),mean(return_weekly),sd(return_weekly)),col="blue")

n=length(return_day)
hist(return_day,freq=F,breaks=20,ylim=c(0,70), main = "Retornos diarios")
lines(density(return_day),col="green")
lines(seq(min(return_day),max(return_day),length=n),dnorm(seq(min(return_day),max(return_day),length=n),mean(return_day),sd(return_day)),col="blue")

#---- Hecho estilizado 3 retornos diarios ----

#3. Standard deviation dominates the mean
mean_month=mean(return_month)
sd_month=sd(return_month)

mean_week=mean(return_weekly)
sd_week=sd(return_weekly)

mean_day=mean(return_day)
sd_day=sd(return_day)

names=c('Mensual','Semanal','Diario')
total_means=c(mean_month, mean_week, mean_day)
total_sd=c(sd_month, sd_week, sd_day)
data.frame('Frecuencia'=names,'Media'=total_means,'Desviación estándar'=total_sd)

#---- Hecho estilizado 4 retornos diarios ----

#4. Positive correlation of squared returns

Conf3x2 = matrix(c(1:6),nrow=2,byrow=TRUE)
layout(Conf3x2)

return_month_2=(return_month)^2 
plot(return_month_2,type="l",col='green',main='Returns^2 Monthly')

return_week_2=(return_weekly)^2
plot(return_week_2,type="l",col='green',main='Returns^2 Weekly')

return_day_2=(return_day)^2
plot(return_day_2,type="l",col='green',main='Returns^2 Daily')

acf(return_month_2,main='Monthly')
acf(return_week_2,main='Weekly')
acf(return_day_2,main='Daily')

