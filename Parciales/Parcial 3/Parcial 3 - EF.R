##### Parcial 3 #####

#Paquetes
library(xts)
library(quantmod)
library(TTR)
library(stats)
library(greybox)
library(smooth)
library(tstPkg1)
library(PerformanceAnalytics)
library(tidyverse)
library(rugarch)

#Importar datos
getSymbols("CHF=X",from="2005-01-01",to="2021-08-31",src="yahoo",adjust=TRUE)
data=Cl(`CHF=X`)
sum(is.na(data)) #Tenemos datos nulos
data=na.omit(data)

# 1. Estadísticas descriptivas ----

#Gr?fica hist?rica del USD/CHF
plot(Cl(data),type="l",col="black",lwd=2,ylab="Close price",main="Daily closing price of USD/CHF")
lines(SMA(Cl(data), n = 200), col = "red") #Media m?vil
lines(EMA(Cl(data), n = 200), col = "blue") #Med?a m?vil exponencial

#Gr?fica de los retornos
ret=diff(log(Cl(data)),lag=1)
ret=na.omit(ret)
plot(ret,type="l",col="black",lwd=2,ylab="Daily returns",main="Daily returns of USD/CHF")
lines(SMA(ret, n = 100), col = "red")
lines(EMA(ret, n = 100), col = "green") #Cl?sters de volatilidad

#Retornos al cuadrado
return_day_2=(ret)^2
plot(return_day_2,type="l",col='blue',main='Daily Returns^2')
acf(return_day_2,main='Daily')

#Gr?fico de autocorrelaci?n
Conf3x2 = matrix(c(1:2),nrow=1,byrow=TRUE)
layout(Conf3x2)
acf(ret,main='ACF of daily returns')
pacf(ret,main='PACF of daily returns')

#Gr?fico de autocorrelaci?n
Conf3x2 = matrix(c(1:2),nrow=1,byrow=TRUE)
layout(Conf3x2)
acf(ret^2,main='ACF of daily returns')
pacf(ret^2,main='PACF of daily returns')

#Retornos no son normales
n=length(ret)
hist(ret,freq=F,breaks=40,ylim=c(0,85),main='Histogram of Daily returns')
lines(density(ret),col="red")
lines(seq(min(ret),max(ret),length=n),dnorm(seq(min(ret),max(ret),length=n),mean(ret),sd(ret)),col="blue")

#Sd > mean
mean_day=mean(ret)
sd_day=sd(ret)

#Volatilidad anualizada m?vil (Ventan m?vil de tres meses)
chart.RollingPerformance(ret, width = 66, FUN = "sd.annualized", scale = 252, main = "Three months rolling volatility")
#Cl?sters de volatilidad, media no constante y volatilidad no constante.

# 2. Media condicional ----

# Función que permite seleccionar un modelo ARMA usando criterios de información
arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}

# 2. Creo la función arma_min_AIC para seleccionar el mejor modelo según AIC.
arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}

# 3. Creo la función arma_min_BIC para seleccionar el mejor modelo según BIC
arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}

#Ahora vamos a ver lo que muestran los criterios AIC y BIC
AR.m <- 6 #Supondremos que el rezago autorregresivo máximo es 6 (pmax)
MA.m <- 6 #Supondremos que el rezago de promedio móvil máximo es 6. (qmax)

# Selección del modelo usando criterios de información 
mod_media = arma_seleccion_df(ret, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_media)

# Selecciono el mejor modelos según criterios de información cuando d=1
min_aic = arma_min_AIC(mod_media); min_aic #ARMA(0,1)
min_bic = arma_min_BIC(mod_media); min_bic #ARMA(0,0)

# Usando el ARMA(0,1) seleccionado por AIC
garchspec_1 = ugarchspec(variance.model = list(model = "sGARCH"),
                        mean.model = list(armaOrder=c(0,1)),
                        distribution.model = "sstd")

garchfit1 = ugarchfit(data = ret, spec = garchspec_1)

garchforecast1 = ugarchforecast(fitORspec = garchfit1, n.ahead = 10)

fitted(garchforecast1)

# Usando el ARMA(0,0) seleccionado por BIC
garchspec_2 = ugarchspec(variance.model = list(model = "sGARCH"),
                         mean.model = list(armaOrder=c(0,0)),
                         distribution.model = "sstd")

garchfit2 = ugarchfit(data = ret, spec = garchspec_2)

garchforecast2 = ugarchforecast(fitORspec = garchfit2, n.ahead = 10)

fitted(garchforecast2)

# 3. Modelación de varianza condicional (efecto leverage) ----

# sGARCH(0, 1) 
garchspec0_1 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0,1)),
                         mean.model = list(armaOrder=c(0,1)),
                         distribution.model = "sstd")

garchfit0_1 = ugarchfit(data = ret, spec = garchspec0_1)

# sGARCH(1, 1) 
garchspec1_1 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                          mean.model = list(armaOrder=c(0,1)),
                          distribution.model = "sstd")

garchfit1_1 = ugarchfit(data = ret, spec = garchspec1_1)

# sGARCH(1, 0) 
garchspec1_0 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
                          mean.model = list(armaOrder=c(0,1)),
                          distribution.model = "sstd")

#garchfit1_0 = ugarchfit(data = ret, spec = garchspec1_0)

# 3. Varianza condicional (verifique la existencia de efectos leverage o asimetrías) ----

egarch1_1 = ugarchspec(variance.model = list(model = "eGARCH", 
                                          garchOrder = c(1,1), 
                                          variance.targeting = TRUE),
                    mean.model = list(armaOrder=c(0,1)),
                    distribution.model = "sstd")

egarchfit1_1 = ugarchfit(data = ret, spec = egarch1_1)
egarchfit1_1

# Ajuste para la varianza
varianza_egarch_1_1 = sigma(egarchfit1_1)
varianza_egarch_1_1
plot(varianza_egarch_1_1)

# 4. Pronóstico de la volatilidad a 10 dı́as ----

egarchforecast1_1 <- ugarchforecast(egarchfit1_1, n.ahead = 10)
egarchforecast1_1

plot(sigma(egarchforecast1_1))

# Plot the excess returns forecast.
plot(egarchforecast1_1, which=1)

# Plot the volatility forecast
plot(egarchforecast1_1, which=3)

# 5. VaR con 99 %% de confianza ----

garchroll = ugarchroll(egarch1_1, data = ret, n.start = 1500, refit.window = "moving", refit.every = 90)
garchVaR <- quantile(garchroll, probs = 0.05)
garchVaR 
actual = xts(as.data.frame(garchroll)$Realized, time(garchVaR))
VaRplot(alpha = 0.01, actual = actual, VaR = garchVaR)

# En qué porcentaje se viola el VaR en promedio
mean(actual < garchVaR)

# 6. Cambio la pandemia del COVID-19 la estructura de la volatilidad de la tasa de cambio? ----

# Retornos desde el 2018
ret2 = ret["2018::2021"]

# Selección del modelo usando criterios de información 
mod_media2018 = arma_seleccion_df(ret2, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_media2018)

# Selecciono el mejor modelos según criterios de información
min_aic = arma_min_AIC(mod_media2018); min_aic #ARMA(0,0)
min_bic = arma_min_BIC(mod_media2018); min_bic #ARMA(0,0)

# Usando el ARMA(0,0) seleccionado por BIC
garchspec2018 = ugarchspec(variance.model = list(model = "sGARCH"),
                         mean.model = list(armaOrder=c(0,0)),
                         distribution.model = "sstd")

garchfit2018 = ugarchfit(data = ret2, spec = garchspec2018)

garchforecast2018 = ugarchforecast(fitORspec = garchfit2018, n.ahead = 10)

fitted(garchforecast2018)

# sGARCH(0, 1) 
garchspec2018_0_1 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0,1)),
                          mean.model = list(armaOrder=c(0,0)),
                          distribution.model = "sstd")

# garchfit2018_0_1 = ugarchfit(data = ret2, spec = garchspec2018_0_1)

# sGARCH(1, 0) 
garchspec2018_1_0 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
                               mean.model = list(armaOrder=c(0,0)),
                               distribution.model = "sstd")

garchfit2018_1_0 = ugarchfit(data = ret2, spec = garchspec2018_1_0)

# sGARCH(1, 1) 
garchspec2018_1_1 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                          mean.model = list(armaOrder=c(0,0)),
                          distribution.model = "sstd")

garchfit2018_1_1 = ugarchfit(data = ret2, spec = garchspec2018_1_1)


egarch2018_1_1 = ugarchspec(variance.model = list(model = "eGARCH", 
                                             garchOrder = c(1,1), 
                                             variance.targeting = TRUE),
                       mean.model = list(armaOrder=c(0,0)),
                       distribution.model = "sstd")

egarchfit2018_1_1 = ugarchfit(data = ret2, spec = egarch2018_1_1)

# Ajuste en muestra

# Ajuste para la varianza
varianza_egarch_2018 = sigma(egarchfit2018_1_1)
varianza_egarch_2018
plot(varianza_egarch_2018)
