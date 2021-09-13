#################### Financial Econometrics - Spring 2021 ######################
#################### Karoll Gómez Portilla ######################
#################### Universidad Nacional de Colombia ######################
#################### Lab 4: Uncondictional volatility measures ######################
library(xts)
library(quantmod) # package has a function to obtain data from various sources
library(TTR)
# Technical Trading Rules
install.packages("FinTS", repos="http://R-Forge.R-project.org")
install.packages("USlobbying", repos="http://R-Forge.R-project.org")
install.packages("tstPkg1", repos="http://R-Forge.R-project.org")
install.packages("tstPkg2", repos="http://R-Forge.R-project.org")
library(stats)
library(greybox)
library(smooth)
library(PerformanceAnalytics)
library(tidyverse)

getSymbols("SPY", from = "2000-01-03", to = "2013-12-31", src = "yahoo", adjust = TRUE)
head(SPY) # Open, high, low, close, volume and ADJUSTED price
# Plot the closing prices of SPY
plot(Cl(SPY))

# Add a 200-day SMA using lines()
lines(SMA(Cl(SPY), n = 200), col = "red")
lines(EMA(Cl(SPY), n = 200), col = "green")

# Returns
ret = diff(log(Cl(SPY)), lag=1)
plot(ret)

# Add a 200-day SMA using lines()
lines(SMA(ret, n = 100), col = "red")
lines(EMA(ret, n = 100), col = "green")

# Returns
ret = diff(log(Cl(SPY)), lag=1)
plot(ret)

# Add a 200-day SMA using lines()
lines(SMA(ret, n = 100), col = "red")
lines(EMA(ret, n = 100), col = "green")

# Compute the rolling 1 month estimate of annualized volatility
chart.RollingPerformance(ret, width = 22, FUN = "sd.annualized", scale = 252, main = "One month rolling volatility")

# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(ret, width = 66, FUN = "sd.annualized", scale = 252, main = "Three months rolling volatility")

### Note: highfrequency package allows to manage highfrequency trades and quotes data, calculate various liquidity measures, estimate and forecast volatility, detect price jumps and investigate microstructure noise and intraday periodicity.

#################### Financial Econometrics - Fall 2018 ######################
#################### Karoll Gómez Portilla ######################
#################### Master in Economics - Universidad Nacional de Colombia ######################
#################### Lab 5: Condictional volatility measures ######################

##### GARCH Exercise 1: Mark 

# Q1. Load the rugarch package and the dmbp dataset (Bollerslev, T. and Ghysels, E. 1996, Periodic Autoregressive Conditional Heteroscedasticity, Journal of Business and Economic Statistics, 14, 139–151).
# This dataset has daily logarithmic nominal returns for Deutsche-mark / Pound. There is also a dummy variable to indicate non-trading days.
library(rugarch)
data("dmbp")

# Q2: Define the daily return as a time series variable and plot the return against time.
rets <- ts(dmbp$V1)
plot(rets)

# Q3:Plot the graph of the autocorrelation function of returns.
acf(rets)

# Q4: Plot the graph of the autocorrelation function of squared returns
acf(rets^2)

# Q5: We will simulate and analyze an ARCH process.
# Use the ugarchspec function to define an ARCH(1) process. The return has a simple mean specification with mean=0. The variance follows as AR-1 process with constant=0.2 and AR-1 coefficient = 0.7.
arch1.mod = ugarchspec(variance.model = list(garchOrder=c(1,0)), mean.model = list(armaOrder=c(0,0)), fixed.pars=list(mu=0, omega=0.2, alpha1=0.7))
arch1.mod

# Q6: Simulate the ARCH process.
arch1.sim = ugarchpath(arch1.mod,n.sim=500)

# Q7: Plot the returns vs time and note the apparent unpredictability. Plot the path of conditional sigma vs time and note that there is some persistence over time.
plot(arch1.sim, which=2) # Returns
plot(arch1.sim, which=1) # Volatility

# Q8: Plot the ACF of returns and squared returns.
acf(arch1.sim@path$seriesSim, main="returns")
acf(arch1.sim@path$seriesSim^2, main="squared returns")

# Q9: Test for ARCH effects using the Ljung Box test for the simulated data and currency returns data.
Box.test(arch1.sim@path$seriesSim^2, type = "Ljung-Box", lag = 12)
Box.test(rets^2, type = "Ljung-Box", lag = 12)
