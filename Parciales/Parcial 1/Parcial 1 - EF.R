##### Parcial 1 - Econometría financiera #####

#Integrantes:
### Carlos Galindo
### Germán Rodríguez
### Santiago Hernández

#Paquetes
library(tidyquant)
library(tidyverse)
library(ggfortify)
library(urca)
library(forecast)
library(vrtest)
library(lmtest)
library(xts)
library(dynlm)

#---- Importación y procesamiento de los datos ----

# Nota: Tanto el indice accionario con periodicidad mensual como el indice con periodicidad diaria fueron tomadas directamente de yahoo finance por medio de la API proveía 
#       por el paquete tidyquant para descargar información financiera de dicha página
#       Ambas series van del 2000 al 2021

# DAX PERFORMANCE-INDEX
# Se trabajara con el DAX PERFORMANCE-INDEX cuyo código de identificación es: ^GDAXI
# It is a blue chip stock market index consisting of the 30 major German companies trading on the Frankfurt Stock Exchange.

# Serie diaria 
DAX_daily=tq_get("^GDAXI",get="stock.prices",from="2000-01-01",to='2021-04-30',periodicity='daily')

# Serie mensual
DAX_monthly=tq_get("^GDAXI",get="stock.prices",from="2000-01-01",to='2021-04-30',periodicity='month')

# Usaremos los precios de cierre ajustados
DAX_daily=DAX_daily %>% 
  select(date,adjusted) %>% 
  na.omit(adjusted) #Eliminamos datos nulos

DAX_monthly=DAX_monthly %>% 
  select(date,adjusted)

# Logaritmo de los precios 
DAX_log_daily = xts(log(DAX_daily$adjusted), order.by = DAX_daily$date)
DAX_log_monthly = xts(log(DAX_monthly$adjusted), order.by = DAX_monthly$date)

# Retornos continuos 
DAX_retornos_diarios = diff(DAX_log_daily)
DAX_retornos_mensuales = diff(DAX_log_monthly)

# Datos adicionales para corroborar el RW2

# Series diarias 
# SP&500 índice accionario de las 500 compañías más valiosas de los EE.UU.
SP500_daily = tq_get("^GSPC",get="stock.prices",from="2000-01-01",to='2021-04-30',periodicity='daily')
SP500_retornos_diarios = diff(xts(log(SP500_daily$adjusted), order.by = SP500_daily$date))

# tasa de cambio nominal entre el euro y el dolar
exchange_rate_daily = tq_get("EURUSD=X",get="stock.prices",from="2000-01-01",to='2021-04-30',periodicity='daily')
exchange_rate_daily = xts(exchange_rate_daily$adjusted, order.by = exchange_rate_daily$date)

# Activo libre de riesgo para inversionistas de largo plazo: bonos a 10 años
bund_daily = tq_get("^TNX",get="stock.prices",from="2000-01-01",to='2021-04-30',periodicity='daily')
bund_daily = xts(bund_daily$adjusted, order.by = bund_daily$date)



# Series mensuales
# SP&500 índice accionario de las 500 compañías más valiosas de los EE.UU.
SP500_monthly = tq_get("^GSPC",get="stock.prices",from="2000-01-01",to='2021-04-30',periodicity='month')
SP500_retornos_mensual = diff(xts(log(SP500_monthly$adjusted), order.by = SP500_monthly$date))

# tasa de cambio nominal entre el euro y el dolar
exchange_rate_monthly = tq_get("EURUSD=X",get="stock.prices",from="2000-01-01",to='2021-04-30',periodicity='month')
exchange_rate_monthly = xts(exchange_rate_monthly$adjusted, order.by = exchange_rate_monthly$date)

# Activo libre de riesgo para inversionistas de largo plazo: bonos a 10 años
bund_monthly = tq_get("^TNX",get="stock.prices",from="2000-01-01",to='2021-04-30',periodicity='month')
bund_monthly = xts(bund_monthly$adjusted, order.by = bund_monthly$date)

#---- 1. Análisis estadístico del índice y de sus retornos ----

# 1.1 Gráficas del logaritmo de los precios ----

# Gráfica del logaritmo de los precios diarios
plot(DAX_log_daily, main = "precios diarios en escala logartimica DAX alemán", xlab ="Fecha", ylab = "log-retornos diarios", lwd = 0.7)

# Gráfica del logaritmo de los precios mensuales
plot(DAX_log_monthly, main = "precios mensuales en escala logaritmica DAX alemán", xlab ="Fecha", ylab = "log-retornos mensuales")

# 1.2 Gráficas del los log-retornos ----

# Gráfica de los log-retornos diarios
plot(DAX_retornos_diarios, main = "log-retornos diarios DAX alemán", ylab = "log-retornos diarios", lwd = 0.45)

# Gráfica de los log-retornos mensuales
plot(DAX_retornos_mensuales, main = "log-retornos mensuales DAX alemán", ylab = "log-retornos mensuales")

# 1.3 Hechos estilizados de los retornos del DAX alemán ----

# 1.3.1 ACF retornos diarios y mensuales ----

lags = 24
# Pareciera que la correlación serial de los retornos diarios luego de 24 días es baja
ggAcf(DAX_retornos_diarios,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF retornos diarios DAX") + theme_light()
# Pareciera que la correlación serial de los retornos mensuales luego de 24 meses es baja
ggAcf(DAX_retornos_mensuales,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF retornos mensuales DAX") + theme_light()

# 1.3.2 Histograma de la distribución de los retornos diarios y mensuales ----

hist_log_returns = function(df, titulo, x_lab, color_llenado, num_bins = 30){
  df2 = df %>% 
    slice(2:nrow(df)) %>% 
    mutate(log_retornos = diff(log(df$adjusted)))
  log_retornos.mean = mean(df2$log_retornos)
  log_retornos.sd = sd(df2$log_retornos)
  histog = df2 %>% 
    ggplot(aes(x = log_retornos)) +
    geom_histogram(color = "black", fill = color_llenado, bins = num_bins, aes(y = ..density..)) + 
    geom_density() +
    stat_function(fun = dnorm, 
                  args = list(mean = log_retornos.mean, sd = log_retornos.sd),
                  color = "red") +
    theme_light() + 
    ggtitle(titulo) +
    ylab("Densidad") +
    xlab(x_lab)
  return(histog)
}
  
# Histograma log-retornos diarios
hist_daily = hist_log_returns(DAX_daily, 
                 "log-retornos diarios DAX alemán",
                 "log-retornos", "white",
                 num_bins = 50); hist_daily

# Histograma log-retornos mensuales
hist_monthly = hist_log_returns(DAX_monthly, 
                              "log-retornos mensuales DAX alemán",
                              "log-retornos", "white",
                              num_bins = 30); hist_monthly

# 1.3.3 La desviación estándar domina la media ----

# Retornos diarios 
media_diaria = mean(diff(log(DAX_daily$adjusted))); media_diaria
sd_diaria = sd(diff(log(DAX_daily$adjusted))); sd_diaria

media_diaria < sd_diaria

# Retornos mensuales  
media_mensual = mean(diff(log(DAX_monthly$adjusted))); media_mensual
sd_mensual = sd(diff(log(DAX_monthly$adjusted))); sd_mensual

media_mensual < sd_mensual

# 1.3.4 Correlación positiva de retornos al cuadrado ----

# Gráfica retornos diarios y mensuales al cuadrado
DAX_retornos_diarios_cuadrado = DAX_retornos_diarios^2
plot(DAX_retornos_diarios_cuadrado, 
     main = "log-retornos darios al cuadrado DAX alemán", 
     xlab ="Fecha", ylab = "log-retornos diarios al cuadrado", lwd = 0.7)

DAX_retornos_mensuales_cuadrado = DAX_retornos_mensuales^2
plot(DAX_retornos_mensuales_cuadrado, 
     main = "log-retornos mensuales al cuadrado DAX alemán", 
     xlab ="Fecha", ylab = "log-retornos mensuales al cuadrado")

lags = 24
# Pareciera que la correlación serial de los retornos diarios luego de 24 días es baja
ggAcf(DAX_retornos_diarios_cuadrado,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF retornos diarios al cuadrado DAX") + theme_light()
# Pareciera que la correlación serial de los retornos mensuales luego de 24 meses es baja
ggAcf(DAX_retornos_mensuales_cuadrado,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF retornos mensuales al cuadrado DAX") + theme_light()

#---- 2. Test estadísticos para la EMH ----

# 2.1 Daily returns tests ----

# 2.1.1 Test for RW3 ----

# Pruebas de correlación serial: Box Pierce y Ljung Box

## Acf retornos diarios 
lags = 24
x11()
ggAcf(DAX_retornos_diarios,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF retornos diarios DAX") + theme_light()

## Test Box-Pierce para autocorrelación en los residuales
Box.test(DAX_retornos_diarios, lag = 720, type = c("Box-Pierce"))
Box.test(DAX_retornos_diarios, lag = 90, type = c("Box-Pierce"))
Box.test(DAX_retornos_diarios, lag = 60, type = c("Box-Pierce"))
Box.test(DAX_retornos_diarios, lag = 30, type = c("Box-Pierce"))
Box.test(DAX_retornos_diarios, lag = 10, type = c("Box-Pierce"))
Box.test(DAX_retornos_diarios, lag = 1, type = c("Box-Pierce"))

# Test Ljung-Box para autocorrelación en los residuales.
Box.test(DAX_retornos_diarios, lag = 720, type = c("Ljung-Box"))
Box.test(DAX_retornos_diarios, lag = 90, type = c("Ljung-Box"))
Box.test(DAX_retornos_diarios, lag = 60, type = c("Ljung-Box"))
Box.test(DAX_retornos_diarios, lag = 30, type = c("Ljung-Box"))
Box.test(DAX_retornos_diarios, lag = 10, type = c("Ljung-Box"))
Box.test(DAX_retornos_diarios, lag = 1, type = c("Ljung-Box"))

# Test de Dickey fuller 

# Prueba de ADF con tendencia y deriva (trend & drift)
adf.trend = ur.df(DAX_log_daily, type = "trend", lags = 1); plot(adf.trend)
summary(adf.trend)

# Prueba de ADF deriva (drift)
adf.drift = ur.df(DAX_log_daily, type = "drift", lags = 1); plot(adf.drift)
summary(adf.drift)

# Prueba de ADF sin tendencia y deriva (none)
adf.none = ur.df(DAX_log_daily, type = "none", lags = 1) ; plot(adf.none)
summary(adf.none)

# Prueba KPSS
summary(ur.kpss(DAX_log_daily))


# 2.1.2 Test for RW1 ----

library(vrtest)
kvec = c(2, 10, 30, 60, 90, 700)
nob = length(DAX_log_daily)
retornos_diarios = log(DAX_daily$adjusted[2:nob]) - log(DAX_daily$adjusted[1:(nob-1)])
Lo.Mac(retornos_diarios, kvec)

# 2.1.3 Test for RW2 ----

rw2_daily_2 = dynlm(L(as.zoo(DAX_retornos_diarios), -2) ~ as.zoo(SP500_retornos_diarios) + as.zoo(exchange_rate_daily) + as.zoo(bund_daily)); summary(rw2_daily_2)
rw2_daily_10 = dynlm(L(as.zoo(DAX_retornos_diarios), -10) ~ as.zoo(SP500_retornos_diarios) + as.zoo(exchange_rate_daily) + as.zoo(bund_daily)); summary(rw2_daily_10)
rw2_daily_30 = dynlm(L(as.zoo(DAX_retornos_diarios), -30) ~ as.zoo(SP500_retornos_diarios) + as.zoo(exchange_rate_daily) + as.zoo(bund_daily)); summary(rw2_daily_30)
rw2_daily_60 = dynlm(L(as.zoo(DAX_retornos_diarios), -60) ~ as.zoo(SP500_retornos_diarios) + as.zoo(exchange_rate_daily) + as.zoo(bund_daily)); summary(rw2_daily_60)
rw2_daily_90 = dynlm(L(as.zoo(DAX_retornos_diarios), -90) ~ as.zoo(SP500_retornos_diarios) + as.zoo(exchange_rate_daily) + as.zoo(bund_daily)); summary(rw2_daily_90)
rw2_daily_700 = dynlm(L(as.zoo(DAX_retornos_diarios), -700) ~ as.zoo(SP500_retornos_diarios) + as.zoo(exchange_rate_daily) + as.zoo(bund_daily)); summary(rw2_daily_700)



# 2.2 Monthly returns tests ----

# 2.2.1 Test for RW3 ----

# Pruebas de correlación serial: Box Pierce y Ljung Box

## Acf retornos mensuales
lags = 24
x11()
ggAcf(DAX_retornos_mensuales,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF retornos diarios DAX") + theme_light()

## Test Box-Pierce para autocorrelación en los residuales
Box.test(DAX_retornos_mensuales, lag = 84, type = c("Box-Pierce"))
Box.test(DAX_retornos_mensuales, lag = 24, type = c("Box-Pierce"))
Box.test(DAX_retornos_mensuales, lag = 12, type = c("Box-Pierce"))
Box.test(DAX_retornos_mensuales, lag = 4, type = c("Box-Pierce"))
Box.test(DAX_retornos_mensuales, lag = 2, type = c("Box-Pierce"))
Box.test(DAX_retornos_mensuales, lag = 1, type = c("Box-Pierce"))

# Test Ljung-Box para autocorrelación en los residuales.

Box.test(DAX_retornos_mensuales, lag = 84, type = c("Ljung-Box"))
Box.test(DAX_retornos_mensuales, lag = 24, type = c("Ljung-Box"))
Box.test(DAX_retornos_mensuales, lag = 12, type = c("Ljung-Box"))
Box.test(DAX_retornos_mensuales, lag = 4, type = c("Ljung-Box"))
Box.test(DAX_retornos_mensuales, lag = 2, type = c("Ljung-Box"))
Box.test(DAX_retornos_mensuales, lag = 1, type = c("Ljung-Box"))

# Test de Dickey fuller 

# Prueba de ADF con tendencia y deriva (trend & drift)
adf.trend = ur.df(DAX_log_monthly, type = "trend", lags = 2); plot(adf.trend)
summary(adf.trend)

# Prueba de ADF deriva (drift)
adf.drift = ur.df(DAX_log_monthly, type = "drift", lags = 1); plot(adf.drift)
summary(adf.drift)

# Prueba de ADF sin tendencia y deriva (none)
adf.none = ur.df(DAX_log_monthly, type = "none", lags = 1) ; plot(adf.none)
summary(adf.none)

# Prueba KPSS
summary(ur.kpss(DAX_log_monthly))

# 2.2.2 Test for RW1 ----

library(vrtest)
kvec = c(2, 4, 12, 24, 84)
nob = length(DAX_log_monthly)
retornos_diarios = log(DAX_monthly$adjusted[2:nob]) - log(DAX_monthly$adjusted[1:(nob-1)])
Lo.Mac(retornos_diarios, kvec)

# 2.2.3 Test for RW2 ----

rw2_monthly_2 = dynlm(L(as.zoo(DAX_retornos_mensuales), -2) ~ as.zoo(SP500_retornos_mensual) + as.zoo(exchange_rate_monthly) + as.zoo(bund_monthly)); summary(rw2_monthly_2)
rw2_monthly_4 = dynlm(L(as.zoo(DAX_retornos_mensuales), -4) ~ as.zoo(SP500_retornos_mensual) + as.zoo(exchange_rate_monthly) + as.zoo(bund_monthly)); summary(rw2_monthly_4)
rw2_monthly_12 = dynlm(L(as.zoo(DAX_retornos_mensuales), -12) ~ as.zoo(SP500_retornos_mensual) + as.zoo(exchange_rate_monthly) + as.zoo(bund_monthly)); summary(rw2_monthly_12)
rw2_monthly_24 = dynlm(L(as.zoo(DAX_retornos_mensuales), -24) ~ as.zoo(SP500_retornos_mensual) + as.zoo(exchange_rate_monthly) + as.zoo(bund_monthly)); summary(rw2_monthly_24)
rw2_monthly_84 = dynlm(L(as.zoo(DAX_retornos_mensuales), -84) ~ as.zoo(SP500_retornos_mensual) + as.zoo(exchange_rate_monthly) + as.zoo(bund_monthly)); summary(rw2_monthly_84)

