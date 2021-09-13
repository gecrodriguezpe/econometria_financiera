#Script de obtencion de datos
#Obtenemos los datos solicitados
library(tidyquant)
library(tidyverse)

# tq_get: get quantitative data in tidy format from the web (yahoo finance)

# Datos diarios
sp500D<- tq_get("^GSPC", get = "stock.prices", from = "2016-03-021",to='2021-03-21')
# Datos semanales
sp500W<- tq_get("^GSPC", get = "stock.prices", from = "2016-03-021",periodicity="weekly")
# Datos mensuales
sp500M<- tq_get("^GSPC", get = "stock.prices", from = "2016-03-021",periodicity="month")


#Usaremos los precios de cierre ajustados
# diarios
sp500Daj<- as.matrix(sp500D[,8])
# semanales
sp500_Waj<- as.matrix(sp500W[,8])
# mensuales
sp500_Maj<- as.matrix(sp500M[,8])