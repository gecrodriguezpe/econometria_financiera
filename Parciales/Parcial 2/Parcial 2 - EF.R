##### Parcial 2 - Econometría financiera #####

#Integrantes:
### Carlos Galindo
### Germán Rodríguez
### Santiago Hernández

# Paquetes
library(zoo)
library(tidyverse)
library(sandwich)
library(gmm)
library(PerformanceAnalytics)
library(stargazer)
library(tidyquant)
library(xts)

# Datos 
data("Finance")
finance2 = rownames_to_column(Finance) %>% 
  mutate(fecha = as.Date(rowname))

# 5.1 ----
Finance = finance2 %>%  
  mutate(portfolio = 0.25 * rm + 0.25 * WMK + 0.2 * UIS + 0.2 * ORB + 0.1 * MAT,
         exceso_port = portfolio - rf, 
         exceso_rm = rm - rf)

Finance_mini = Finance %>% 
  select(fecha, rm, WMK, UIS, ORB, MAT)

Finance_xts = xts(Finance_mini[2:6], order.by = Finance_mini$fecha)

return_portfolio_daily = Return.portfolio(Finance_xts, weights = c(0.25, 0.25, 0.2, 0.2, 0.1), rebalance_on = "days")

# Modelo CAPM
capm = lm(exceso_port ~ exceso_rm, data = Finance)

# Prueba por serie de tiempo
summary(capm)

# Prueba de corte transversal 

# Resumen de los resultados del modelo CAPM para el ejercicio 5.1
stargazer(capm, type = "latex", style = "AER")

# stargazer(fatalities_pooled, fatalities_FE, fatalities_EA, fatalities_FD, digits = 3,
#           header = F, type = "text", 
#           title = "Modelos de catastrofes de tránsito por conducir ebrio", 
#           column.labels = c("Pooled","EF","RE","Primeras Diferencias"))

# 5.2 ----

# Modelo Fama-French
fama_french = lm(exceso_port ~ exceso_rm + hml + smb, data = Finance)
summary(fama_french)

# Resumen de los resultados del modelo Fama French para el ejercicio 5.2
stargazer(fama_french, type = "latex", style = "AER")

# 5.3 ----

portfolio_daily_xts = xts(Finance$portfolio, order.by = finance2$fecha)

Finance = Finance %>% 
  mutate(fecha = finance2$fecha,
         month = as.numeric(format(finance2$fecha,'%m')),
         year = as.numeric(format(finance2$fecha,'%Y')))

resumen = Finance %>% 
  group_by(year, month) %>% 
  summarise(excesos_retornos_mens_port = mean(exceso_port), 
            excesos_retornos_mens_rm = mean(exceso_rm), 
            WMK = mean(WMK),
            UIS = mean(UIS),
            ORB = mean(ORB),
            MAT = mean(MAT),
            rm = mean(rm),
            rf = mean(rf))

resumen_final = resumen %>% 
  group_by(year) %>% 
  summarize(excesos_retornos_year_port = mean(excesos_retornos_mens_port),
            excesos_retornos_year_rm = mean(excesos_retornos_mens_rm), 
            WMK = mean(WMK),
            UIS = mean(UIS),
            ORB = mean(ORB),
            MAT = mean(MAT),
            rm = mean(rm),
            rf = mean(rf))

# 5.3.1 Método manual ----

CAPM_anual = lm(excesos_retornos_year_port ~ excesos_retornos_year_rm, 
                data = resumen_final)

summary(CAPM_anual)

# 5.3.2 Método Return.portfolio ----

resumen_Return_port = resumen_final %>% 
  select(year, rf, rm, WMK, UIS, ORB, MAT)

Return_port_ts = ts(resumen_Return_port[3:7], start = 1993, frequency = 1)

resumen_final = resumen_final %>% 
  mutate(return_portfolio_yearly = Return.portfolio(Return_port_ts, 
                                                    weights = c(0.25, 0.25, 0.2, 0.2, 0.1),
                                                    rebalance_on = "years"),
         excesos_port_year_Returnportfolio = return_portfolio_yearly - rf,
         excesos_merc_year_Returnportfolio = rm - rf)

CAPM_anual_Return_portfolio = lm(excesos_port_year_Returnportfolio ~ excesos_merc_year_Returnportfolio,
                                 data = resumen_final)

summary(CAPM_anual_Return_portfolio)

# Resumen de los resultados del modelo CAPM con retornos anuales para el ejercicio 5.3
stargazer(CAPM_anual, CAPM_anual_Return_portfolio, type = "latex", style = "AER")