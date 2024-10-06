# Qing Ou
# Econ-423
# Professor Maung
# Term Paper
# Topic: How do foreign exchange rates, interest rates, GDP 
# durable product consumption and unit labor cost affect unemployment in the forcast? 

install.packages("urca", repos="http://R-Forge.R-project.org")
install.packages("quadprog")
install.packages("xts")
install.packages("forecast", dependencies = TRUE)
install.packages("fredr")
install.packages("MSwM")
install.packages("tvReg")
install.packages("roll")
install.packages("gglasso")
install.packages("glmnet")
install.packages("vars")
install.packages("plotly")

library(urca)
library(ggplot2)
library(tseries)
library(tidyverse)
library(fredr)
update.packages()
library(forecast)
library(MSwM)
library(tvReg)
library(BVAR)
library(roll)
library(gglasso)
library(glmnet)
library(dplyr)
library(xts)
library(vars)
library(plotly)
library(ggplot2)


api_key <- "7745b3743c56d45d401d513bc39742b6"
fredr_set_key(api_key)

rm(list=ls())
# unemployment
unemp <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)

# foreign exchange rate USD to CNY from 1981-01-01 ???

ferCn <- fredr(
  series_id = "EXCHUS",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)

# foreign exchange rate USD to CA

ferCa <- fredr(
  series_id = "EXCAUS",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)

# interest rate ???? discount interest rate
intr <- fredr(
  series_id = "TB3MS",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)

# GDP
gdp <- fredr(
  series_id = "gdp",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)

# durable goods consumption
dgc <- fredr(
  series_id = "PCEDG",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)

# Currency Conversions: Real Effective Exchange Rates: Overall Economy: Unit Labor Costs (ULC) for United States
ulc <- fredr(
  series_id = "CCRETT02USQ661N",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)

# dependent variable: unemployment
data_frame_unempWdate <- data.frame(unemp$date, unemp$value)
date = unemp$date
unemp = unemp$value

# independent variables: 
gdp<-gdp$value #GDP
intr<-intr$value #interest rate
ferCa<-ferCa$value #foreign exchange rate of Canada Dollar
ferCn<-ferCn$value #foreign exchange rate of CNY
dgc<-dgc$value #durable goods consumption
ulc<-ulc$value #unit labor cost

# x variables
data_frame <- data.frame(gdp, intr, ferCa, ferCn, dgc, ulc)
data_frame_0 <- data.frame(unemp, gdp, intr, ferCa, ferCn, dgc, ulc)

# plot with graph
graph <- data.frame(unemp) %>% 
  plot_ly(x=~date) %>% 
  add_trace(y=~unemp, type="scatter", mode="lines")

graph

# Plot
plot(unemp, ylab="x", xlab="Time", main=(expression(AR(1)~~~theta==+1.2)), type='l', lwd=2, col="navy")#abline(a=0, b=0, lty=2)
# test stationarity
adf_result <- adf.test(unemp, alternative = "stationary")
adf_result
# test seasonality
acf(unemp, main="ACF of Unemployment")
pacf(unemp, main="PACF of Unemployment")
# Detrend by log difference
N <- length(unemp)
unemp_chg <- (log(unemp[-1]) - log(unemp[-N])) * 100

graph <- data.frame(unemp_chg) %>% 
  plot_ly(x=~date[-1]) %>% 
  add_trace(y=~unemp_chg, type="scatter", mode="lines")

graph
# test stationarity
adf_result <- adf.test(unemp_chg, alternative = "stationary")
adf_result

#Plot with AR(1)
plot(unemp_chg, ylab="x", xlab="Time", main=(expression(AR(1)~~~theta==+1.2)), type='l', lwd=2, col="navy")#abline(a=0, b=0, lty=2)


# test seasonality
acf(unemp_chg, main="ACF of Unemployment")
pacf(unemp_chg, main="PACF of Unemployment")

# Model Choosing
#AR(1)
fit <- arima(unemp_chg, order=c(1, 0, 0), include.mean=FALSE)
summary(fit)
checkresiduals(fit)
#AR(2)
fit <- arima(unemp_chg, order=c(2, 0, 0), include.mean=FALSE)
summary(fit)
checkresiduals(fit)
# forecast
autoplot(forecast(fit))
#AR(3)
fit <- arima(unemp_chg, order=c(3, 0, 0), include.mean=FALSE)
summary(fit)
#AR(4)
fit <- arima(unemp_chg, order=c(4, 0, 0), include.mean=FALSE)
summary(fit)

#d)ARMA(1,1) 
acf(arima.sim(list(order=c(1,0,1), ar=0.0062, ma=0.0818), n=500), main="ACF of ARMA(1,1)")
arima(unemp, order=c(1,0,1), include.mean=FALSE)
#e)ARIMA
arima(unemp, order=c(1,0,1), include.mean=FALSE)
#e)
aic <- auto.arima(unemp, ic="aic", stationary = TRUE)
AIC(aic)
bic <- auto.arima(unemp, ic="bic", stationary = TRUE)
BIC(bic)
# Although standard errors in AR(1) in ARMA, ACF shows multiple lag spikes.


# alternative way to have stationarity
Time <- c(1:167)
Rate <- rnorm(167, mean = 0)
y_unemp <- data.frame(unemp, Rate)
fig <- plot_ly(y_unemp, x = ~ Time, y = ~ Rate, type = 'scatter', mode = 'lines')
fig 


Time <- c(1:166)
Rate_chg <- rnorm(166, mean = 0)
y_unemp_chg <- data.frame(unemp_chg, Rate_chg)
fig <- plot_ly(y_unemp_chg, x = ~ Time, y = ~ Rate, type = 'scatter', mode = 'lines')
fig 

####


# unemp,gdp,intr,ferCa,ferCn,dgc,ulc
N <- length(gdp)
gdp_chg <- (log(gdp[-1]) - log(gdp[-N])) * 100

N <- length(intr)
intr_chg <- (log(intr[-1]) - log(intr[-N])) * 100

N <- length(ferCa)
ferCa_chg <- (log(ferCa[-1]) - log(ferCa[-N])) * 100

N <- length(ferCn)
ferCn_chg <- (log(ferCn[-1]) - log(ferCn[-N])) * 100

N <- length(dgc)
dgc_chg <- (log(dgc[-1]) - log(dgc[-N])) * 100

N <- length(ulc)
ulc_chg <- (log(ulc[-1]) - log(ulc[-N])) * 100


# Lasso Regression
y <- data.frame(unemp_chg,gdp_chg,intr_chg,ferCa_chg,ferCn_chg,dgc_chg,ulc_chg)

x_lasso <- y %>% select(-unemp_chg) %>% as.matrix()
y_lasso <- y[,"unemp_chg"]

lambdas <- 10^seq(2, -3, by = -0.1)
lasso_reg <- cv.glmnet(x_lasso, y_lasso, alpha=1, lambda=lambdas)

lambda_best <- lasso_reg$lambda.min
lasso_model <- glmnet(x_lasso, y_lasso, alpha = 1, lambda = lambda_best)

coefficients(lasso_model)

# Orthogonal Impulse Response to Foreign Exchange Rate on CanadaD
y0 <- data.frame(unemp_chg, gdp_chg, ferCa_chg)
y_aic0 <- VAR(y0, type="const",lag.max=6, ic="AIC")
shock_FXR <- irf(y_aic0, n.ahead = 12, ortho = TRUE, impulse = "ferCa_chg")
plot(shock_FXR)

y1 <- data.frame(unemp_chg, gdp_chg)
y_aic1 <- VAR(y1, type="const",lag.max=6, ic="AIC")
shock_gdp <- irf(y_aic1, n.ahead = 12, ortho = TRUE, impulse = "gdp_chg")
plot(shock_gdp)

y2 <- data.frame(unemp_chg, gdp_chg, intr_chg, dgc_chg)
y_aic2 <- VAR(y2, type="const",lag.max=6, ic="AIC")
shock_intr <- irf(y_aic2, n.ahead = 12, ortho = TRUE, impulse = "intr_chg")
plot(shock_intr)

y3 <- data.frame(unemp_chg, gdp_chg, intr_chg, dgc_chg)
y_aic3 <- VAR(y3, type="const",lag.max=6, ic="AIC")
shock_dgc <- irf(y_aic3, n.ahead = 12, ortho = TRUE, impulse = "dgc_chg")
plot(shock_dgc)

y4 <- data.frame(unemp_chg, gdp_chg, intr_chg, ulc_chg)
y_aic4 <- VAR(y4, type="const",lag.max=6, ic="AIC")
shock_ulc <- irf(y_aic4, n.ahead = 12, ortho = TRUE, impulse = "ulc_chg")
plot(shock_ulc)

reg_1 <- arima(unemp_chg, order=c(2,0,0), optim.method="Nelder-Mead")
summary(reg_1)


# plot(reg_1)
acf(unemp_chg)
pacf(unemp_chg)
autoplot(fit)# stationarity within the unit circle

# Forecast 1 by AR(1)
data_frame_chg<- data.frame(unemp_chg, gdp_chg, intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg)
x <- data_frame_chg %>% select(gdp_chg, intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg) %>% as.matrix()
data_frame_unemp_chg <- data.frame(unemp_chg)
y <- data_frame_unemp_chg[,"unemp_chg"]

#fit_1 <- Arima(y, order = c(1, 0, 0), xreg = x)
fit_1 <- forecast::Arima(y, order = c(1, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_1, xreg = x, h = 10)
forecast

# Test models
AIC(fit_1)
BIC(fit_1)

# test residuals of the forecast by AR(1)
residuals <- residuals(fit_1)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")

#A significant p-value suggests the residuals are not random, indicating a problem with the model.
#If there are significant spikes at lags other than zero, it may indicate autocorrelation.

#Forecast by AR(2)
fit_1 <- forecast::Arima(y, order = c(2, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_1, xreg = x, h = 10)
forecast

# Test models
AIC(fit_1)
BIC(fit_1)

# test residuals of the forecast
residuals <- residuals(fit_1)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")

#Forecast by AR(3)
fit_1 <- forecast::Arima(y, order = c(3, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_1, xreg = x, h = 10)
forecast

# Test models
AIC(fit_1)
BIC(fit_1)

# test residuals of the forecast
residuals <- residuals(fit_1)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")

# Forecast 1 by AR(4)
data_frame_1 <- data.frame(unemp_chg, gdp_chg, intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg)
data_frame_chg <- data.frame(gdp_chg, intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg)
x <- data_frame_chg %>% select(gdp_chg, intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg) %>% as.matrix()
data_frame_unemp_chg <- data.frame(unemp_chg)
y <- data_frame_unemp_chg[,"unemp_chg"]

#fit_1 <- Arima(y, order = c(4, 0, 0), xreg = x)
fit_1 <- forecast::Arima(y, order = c(4, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_1, xreg = x, h = 10)
forecast

# Test models
AIC(fit_1)
BIC(fit_1)

# test residuals of the forecast
residuals <- residuals(fit_1)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")

# test residuals of the forecast, the same as the above
residuals <- residuals(forecast)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")

#A significant p-value suggests the residuals are not random, indicating a problem with the model.
#If there are significant spikes at lags other than zero, it may indicate autocorrelation.

# test causal effects on x and y
data_frame_chg <- data.frame(gdp_chg, intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg)
model <- lm(y ~ x, data = data_frame_chg)
summary(model)

# Forecast 3 by drop one variable (GDP) in AR(4)
data_frame_chg <- data.frame(intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg)
x <- data_frame_chg %>% select(intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg) %>% as.matrix()
data_frame_unemp_chg <- data.frame(unemp_chg)
y <- data_frame_unemp_chg[,"unemp_chg"]

#fit_1 <- Arima(y, order = c(4, 0, 0), xreg = x)
fit_1 <- forecast::Arima(y, order = c(1, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_1, xreg = x, h = 10)
forecast

# test residuals of the forecast by AR(4)
residuals <- residuals(fit_1)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")

#None significant p-value suggests the residuals are not random, indicating a problem with the model.
#If there are significant spikes at lags other than zero, it may indicate autocorrelation.

install.packages("ivreg")
library(ivreg)

# Assume y is the dependent variable, x is the potentially endogenous regressor,
# z is the instrumental variable, and w are other exogenous regressors.

# OLS model
CPI_1 <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)
CPI_1 = CPI_1$value

# Federal Funds Effective Rate as IV
ffer <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency='q'
)
ffer<- ffer$value

N <- length(CPI_1)
CPI_1_chg <- (log(CPI_1[-1]) - log(CPI_1[-N])) * 100
N <- length(ffer)
ffer_chg <- (log(ffer[-1]) - log(ffer[-N])) * 100
#data_frame_chg_0 <- data.frame(gdp_chg, intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg, ffer_chg, CPI_1_chg)

z <- CPI_1_chg
w <- ffer_chg

ols_model <- lm(y ~ x + w, data = data_frame_chg)
summary(ols_model)

# IV model: z as an instrument for x
#iv_model <- ivreg(y ~ x + w | z + w, data = data_frame_chg)

# Durbin-Wu-Hausman test: endogeneity
#library(lmtest)
#hausman_test <- bptest(iv_model, ~ x | z, data =data_frame_chg)
#print(hausman_test)

# Ordinary Least Squares
data_frame_chg <- data.frame(intr_chg,gdp_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg)
x <- data_frame_chg %>% select(intr_chg, gdp_chg,ferCa_chg, ferCn_chg, dgc_chg, ulc_chg) %>% as.matrix()
y <- data_frame_unemp_chg[,"unemp_chg"]
ols_model <- lm(y ~ x, data = data_frame_chg)
summary(ols_model)
# intr gdp dgc are 95% statistic significant to unemployment as p-value is < 0.05 

# Forecast 3 by drop one variable (GDP) in AR(2)
data_frame_chg <- data.frame(intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg)
x <- data_frame_chg %>% select(intr_chg, ferCa_chg, ferCn_chg, dgc_chg, ulc_chg) %>% as.matrix()
data_frame_unemp_chg <- data.frame(unemp_chg)
y <- data_frame_unemp_chg[,"unemp_chg"]

#fit_1 <- Arima(y, order = c(2, 0, 0), xreg = x)
fit_1 <- forecast::Arima(y, order = c(2, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_1, xreg = x, h = 10)
forecast

# test residuals of the forecast by AR(1)
residuals <- residuals(fit_1)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")

#None significant p-value suggests the residuals are not random, indicating a problem with the model.
#If there are significant spikes at lags other than zero, it may indicate autocorrelation.

# Forecast 4 
# Forecast 3 by keep intr, gdp, dgc only drop rest variables in AR(2)
data_frame_chg <- data.frame(intr_chg, gdp_chg, dgc_chg)
x <- data_frame_chg %>% select(intr_chg, gdp_chg, dgc_chg) %>% as.matrix()
data_frame_unemp_chg <- data.frame(unemp_chg)
y <- data_frame_unemp_chg[,"unemp_chg"]

#fit_1 <- Arima(y, order = c(2, 0, 0), xreg = x)
fit_1 <- forecast::Arima(y, order = c(2, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_1, xreg = x, h = 10)
forecast

# test residuals of the forecast by AR(2)
residuals <- residuals(fit_1)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")

# Forecast 5 
# Forecast 5 by keep intr, gdp, dgc only drop rest variables in AR(4)
data_frame_chg <- data.frame(intr_chg, gdp_chg, dgc_chg)
x <- data_frame_chg %>% select(intr_chg, gdp_chg, dgc_chg) %>% as.matrix()
data_frame_unemp_chg <- data.frame(unemp_chg)
y <- data_frame_unemp_chg[,"unemp_chg"]

#fit_1 <- Arima(y, order = c(2, 0, 0), xreg = x)
fit_1 <- forecast::Arima(y, order = c(4, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_1, xreg = x, h = 10)
forecast

# test residuals of the forecast by AR(4)
residuals <- residuals(fit_1)
Box.test(residuals, lag=10, type="Ljung-Box")
acf(residuals, main="ACF of Residuals")
#None significant p-value suggests the residuals are not random, indicating a problem with the model.
#If there are significant spikes at lags other than zero, it may indicate autocorrelation.

# The forecast values
forecast_values <- forecast$mean

# Plot the forecast
plot(forecast_values)
lines(y, col = "blue")

# Load the necessary libraries
library(forecast)
library(ggplot2)

forecast_df <- data.frame(
  time = as.Date(as.yearmon(time(forecast$mean))),
  forecast = as.numeric(forecast$mean),
  lower_80 = forecast$lower[, "80%"],
  upper_80 = forecast$upper[, "80%"],
  lower_95 = forecast$lower[, "95%"],
  upper_95 = forecast$upper[, "95%"]
)

# Create the plot
ggplot(data = forecast_df, aes(x = time)) +
  geom_line(aes(y = forecast), color = "red") +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.1) +
  geom_line(data = unemp_df, aes(x = time, y = actual), color = "blue") +
  theme_minimal() +
  labs(title = "Forecast vs Actual Data in Unemployment", y = "Value")

### 

fit_2 <- forecast::Arima(y, order = c(1, 0, 0), xreg = x)
forecast <- forecast::forecast(fit_2, xreg = x, h = 10)
forecast

forecast_df <- data.frame(
  time = as.Date(as.yearmon(time(forecast$mean))),
  forecast = as.numeric(forecast$mean),
  lower_80 = forecast$lower[, "80%"],
  upper_80 = forecast$upper[, "80%"],
  lower_95 = forecast$lower[, "95%"],
  upper_95 = forecast$upper[, "95%"]
)

# Create the plot
ggplot(data = forecast_df, aes(x = time)) +
  geom_line(aes(y = forecast), color = "red") +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.1) +
  geom_line(data = unemp_df, aes(x = time, y = actual), color = "blue") +
  theme_minimal() +
  labs(title = "Forecast vs Actual Data in Unemployment", y = "Value")