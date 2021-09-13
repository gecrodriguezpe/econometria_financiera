#################### Financial Econometrics - Spring 2021 ######################
#################### Karoll Gómez Portilla ######################
#################### Universidad Nacional de Colombia ######################
#################### Lab 6: Determine the value-at-risk ######################
################# 1. Become more familiar with random variables and probability distributions.

# Suppose Y is a normally distributed random variable with a mean 0.05 and a variance (0.10)^2
y = rnorm(100, 0.05, 0.10)

# Y ~ N(0.05, (0.10)^2)
mu_y <- 0.05
sigma_y <- 0.10

# Pr(Y > 0.10) ¿Cuál es la probabilidad de que la variabe Y tome valores mayores a 0.10 ?
1 - pnorm(0.10, mean = mu_y, sd = sigma_y) # acumulated distribution function

# Rta: la probabilidad es del 30,85%
pnorm(0.10, mean = mu_y, sd = sigma_y) # Esto represneta la probabilidad de que Y tome valores menores o iguales a 0,1

# Pr(Y < -0.10)
pnorm(-0.10, mean = mu_y, sd = sigma_y)

# 1%, 5%, 95% and 99% quantile
qnorm(c(0.01, 0.05, 0.95, 0.99), mean = mu_y, sd = sigma_y)

## Example: Suponga que una máquina que empaqueta arroz dentro de cajas. El proceso sigue una distribución normal y se sabe que la media del peso de cada caja es de 1000 gramos y la desviación típica es 10 gramos

# Media y desviación estándar
mu <- 1000
sigma <- 10

# Grid para una distribución normal no estándar
x <- seq(-3, 3, length = 100) * sigma + mu

# Función de densidad
f <- dnorm(x, mu, sigma)
plot(x, f, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "Weight")
abline(v = mu) # Línea vertical en la media

# ¿Cuál es la probabilidad de que una caja pese menos de 1010 gramos ?
pnorm(1010, mu, sigma) # 0.8413447 o 84.13%
1 - pnorm(1010, mu, sigma, lower.tail = FALSE) # Equivalente
1- pnorm(1010, mu, sigma) # prob de pesar más de 1010 gramos
lb <- min(x) # Límite inferior
ub <- 1010

# Límite superior
x2 <- seq(min(x), ub, length = 100) # Nueva rejilla
y <- dnorm(x2, mu, sigma) # Densidad
plot(x, f, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "Peso")
abline(v = ub)
polygon(c(lb, x2, ub), c(0, y, 0), col = rgb(0, 0, 1, alpha = 0.5))
text(995, 0.01, "84.13%")

# Normally distributed monthly returns
x_vals <- seq(-0.25, 0.35, length.out = 100)
MSFT <- dnorm(x_vals, mean = 0.05, sd = 0.1)
SBUX <- dnorm(x_vals, mean = 0.025, sd = 0.05)

# Normal curve for MSFT
plot(x_vals, MSFT, type = "l", col = "blue", ylab = "Normal curves", ylim = c(0, 8))
# Add a normal curve for SBUX
lines(x_vals, SBUX, col = "red")
# Add a plot legend
legend("topleft", legend = c("Microsoft", "Starbucks"), col = c("blue", "red"), lty = 1)

##################### Value at risk of simple monthly returns
# Determine the 1% and the 5% value-at-risk (VaR) over the month on the investment. That is, determine the loss in investment value that may occur over the next month with a 1% probability and with a 5% probability.
# Consider again the Microsoft stock. Assume that the simple monthly return is distributes with mean 0.04 and variance (0.09)^2.
# The initial wealth to be invested over the month is $100,000.
# R ~ N(0.04, (0.09)^2)
mu_R <- 0.04
sigma_R <- 0.09
# Initial wealth W0 equals $100,000
W0 <- 100000
# 1% value-at-risk
W0 * qnorm(0.01, mean = mu_R, sd = sigma_R)
# 5% value-at-risk
W0 * qnorm(0.05, mean = mu_R, sd = sigma_R)

##################### Value at risk of cc monthly returns
# Use the fact that the continuously compounded return quantile can be transformed to a simple return quantile with the transformation R= exp(r)-1.
# r ~ N(0.04, (0.09)^2)
mu_r <- 0.04
sigma_r <- 0.09
# Initial wealth W0 equals $100,000
W0 <- 100000
# The 1% value-at-risk
W0 * (exp(qnorm(0.01, mean = mu_r, sd = sigma_r)) - 1)
