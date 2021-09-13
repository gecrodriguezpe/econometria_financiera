### Financial Econometrics - Fall 2018 ###
### Karoll Gómez Portilla ###
### Master in Economics - Universidad Nacional de Colombia ###
### Lab 2: Testing EMH ###

library(tidyverse)
library(urca)

### Starbucks stock monthly price series from 1998 to 2009
setwd("~/Documents/GitHub/semestre6_git/econometria_financiera/Labs/Lab2")
starbx = read.delim("sbuxPrices.csv",header=TRUE,sep = ",", dec = ".") # precios de la acción de starbucks 


# Compute log prices
closing =starbx[, "Adj.Close"]  # Los datos son de frecuencia mensual 
logprice = log(closing)
plot(logprice, type="l")

# Compute the continuously compounded 1-month returns
sbux = as.matrix(closing)
n = nrow(sbux)
sbux.ccret = log(sbux[2:n,1]) - log(sbux[1:(n - 1),1])
plot(sbux.ccret, type="l")

#---- IID (RW1) => Independencia (RW2) => MARTINGALE DIFFERENCE (RW2.5) => WHITE NOISE (RW3) (comportamiento de los retornos) ----

###### Testing RW3
acf(sbux.ccret)
Box.test(sbux.ccret, lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(sbux.ccret, lag = 1, type = c("Box-Pierce"), fitdf = 0)

library(randtests)
# bartels.rank.test(sbux.ccret, pvalue="normal")  # Otro test para probar el supuesto de correlación serial en la serie

###### Testing RW1   (Test de razones de varianza)
library(vrtest)  # Librería diseñada exclusivamente para hacer test de razón de varianza
kvec = c(2,5,10) # k as k-th lag of your time series (eg. k=1 is the first difference of the time series, k=2 is the 2nd and so on).
Lo.Mac(sbux.ccret, kvec) # it checks whether the VR = 1 for a given k to see whether the series is a random walk (which is the null hypothesis).

# M1 is the Lo.Mac statistic under the homoscedastic assumption. M2 is the Lo.Mac statistic under the heteroscedastic assumption. Better to look at M2 than M1 as M2 is based on a more
#realistic assumption. M1 and M2 are assumed to follow a normal distribution. So if M1 or M2 is greater than 1.96, you can reject the null hypothesis at 5% significant level.

#---- Other example with different data set: Exchange rates ----
data(exrates)       # Datos sobre tasas de cambio
y = exrates$ca      # Datos de Canada 
nob <- length(y)
r <- log(y[2:nob])-log(y[1:(nob-1)])  # Calcular los retornos 
kvec1 <- c(2,5,10)    # Vector de adelantos 
Lo.Mac(r, kvec1)      # Prueba de Lo-MacKinlay para test de razón de varianza
# test que mira el grado de ineficiencia del mercado 
# valores muy grandes del test significan mucha ineficiencia (entre mayor sea el valor más ineficiente)
VR.minus.1(r,kvec1) # This value is sometimes used to measure the degree of market efficiency (que tan alejado es de un estadistico de 1)
VR.plot(r,kvec) #como se comporta el test de razón de varianza gráficamente 
# como se comporta gráficamente el valor del test a medida que se agregan rezagos

###### Homework 3
# Download data for a stock market index on daily as well as monthly frequency.
# 1. Discuss what the index measure and provide plots of the data series. Calculate returns and conduct a discussion of their time series