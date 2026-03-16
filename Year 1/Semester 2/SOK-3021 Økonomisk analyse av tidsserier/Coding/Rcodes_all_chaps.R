# Load data from the textbook
# i.e., usmacro

rm(list = ls())

library(tidyverse)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

View(usmacro)

names(usmacro)

# let us work with the variable, u
usmacro %>% 
  ggplot(aes(x=dateid01, y= u))+
  geom_line()+
  
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y")+
  
  ylab("U.S unemployment rate")+
  xlab("")+
  theme_bw()


# compute lag (previous values) of u
usmacro %>% 
  select(dateid01, u) %>% 
  mutate(
    u1=lag(u,1),  # first lag 
    u2 = lag(u,2) # the second lag 
  ) %>% View()

# remove NA and save 
df <- usmacro %>% 
  select(dateid01, u) %>% 
  mutate(
    u1=lag(u,1),  
    u2 = lag(u,2) ) %>% drop_na() # remove NA's 

head(df)

# Let us model u by using AR(2)

# option 1

fit1 <- lm(u ~ u1+u2, data = df)
summary(fit1)

# Task: Interpret the estimated coefficients? 
#The intercept: is the value of the unemployment rate 
# int the current quarter(i.e., 2026Q1) when the unemployment rate 
# in the previous two quarters are zero.

# u1: the unemployment rate in the current quarter
# increases by 1.61% if the unemployment rate in the previous quarter 
# increases by 1%,

# u2: the unemployment rate in the current quarter 
# decreases by 0.66% if the unemployment rate in 
# two quarter ago increased by 1%. 

# option 2
install.packages("dynlm")
library(dynlm)
# L(u,1)- the first lag of u
# L(u,2)- the second lag of u
fit2 <- dynlm(u ~ L(u,1)+L(u,2), data = usmacro )
summary(fit2)

fit2 <- dynlm(u ~ L(u,1)+L(u,2), data = ts(usmacro)) # notice ts()
summary(fit2)

# compact writing 
fit2 <- dynlm(u ~ L(u,1:2), data = ts(usmacro))
summary(fit2)

# if you want to add more lags 
#fit_2 <- dynlm(u ~ L(u,1:10), data = ts(usmacro))

# option 3
install.packages("forecast")
library(forecast)

fit3 <- arima(usmacro$u, order = c(2,0,0))
summary(fit3)

# In this course, we basically and mostly use 
# option 3, i.e., arima()

rm(list=ls())
library(broom)
library(tidyverse)

#' Example 9.1

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

str(usmacro)

head(usmacro)

usmacro %>% 
  ggplot(aes(x=dateid01, y=u)) + 
  geom_line() + ylab("Unemployment Rate") + 
  xlab("Year") +
  labs(title = "Figure 9.2a: U.S. Quarterly unemployment rate 1948:Q1 to 2016:Q1")

# Forecasting using ARIMA model 

rm(list = ls())
library(tidyverse)
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

head(usmacro)

# Forecasting 
library(forecast)

# Forecasting using AR(2) model using arima()

#?arima()

u <- ts(usmacro$u, frequency = 4, start = c(1948,1))
#u

fit_ar <- arima(u, order = c(2,0,0))  # AR(2)
summary(fit_ar)


#Use the model to forecast the unemployment rate in the next 10 quarters 
# forcast next 10 time points 
ar_forecast  <- forecast(fit_ar,h=10)
ar_forecast


#plot the data and forecast
plot(ar_forecast)

# Alternatively 
arima(usmacro$u, order = c(2,0,0)) %>% forecast(h=10) %>% autoplot + theme_bw()


# Forcasting using MA(2)
arima(usmacro$u, order = c(0,0,2))  %>% forecast(h=10) %>% autoplot + theme_bw()


# Forecasting using ARMA(2,0,2)
arima(usmacro$u, order = c(2,0,2))  %>% forecast(h=10) %>% autoplot + theme_bw()


# notice the difference between the following two lines of codes
arima(usmacro$u, order = c(2,0,0)) %>% forecast(h=10)   

# defining u as ts() object 
u <- ts(usmacro$u, frequency = 4, start = c(1948,1))
arima(u, order = c(2,0,0)) %>% forecast(h=10) 

#arima(u, order = c(2,0,0)) %>% forecast(h=10) %>% autoplot





# Example 9.7 Forecasting unemployment with an ARDL(2,1) model

usmacro.lag <- cbind( u = usmacro[,"u"],
                      g = usmacro[,"g"],
                      gLag1 = dplyr::lag(usmacro[,"g"],1))

head(usmacro.lag)

#
Arima(usmacro.lag[,"u"], order=c(2,0,0), xreg = usmacro.lag[,"gLag1"])

library(broom)
Arima(usmacro.lag[,"u"], order=c(2,0,0), xreg = usmacro.lag[,"gLag1"]) %>% tidy()

#  remove tidy() at the end and  save
fit2 <- Arima(usmacro.lag[,"u"], order=c(2,0,0), xreg = usmacro.lag[,"gLag1"]) 


# Forecasting 
fc2 <- forecast(fit2, h=3,xreg=cbind(xreg = c(usmacro[,"g"][273],0.869,1.069))) 

fc2

# plot the forecast value and interval 
autoplot(fc2) + ylab("Unemployment") +
  ggtitle("Forecast unemployment with future GDP growth")+
  theme_bw()

#' Lag selection criteria  
#' ARDL(p, q)  p, q = ?


rm(list=ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

head(usmacro)

# Example 9.8
library(tidyverse)
library(broom)
library(forecast)

usmacro %>% dplyr::select(u) %>% Arima(., order=c(0,0,0)) %>% glance()   
usmacro %>% dplyr::select(u) %>% Arima(., order=c(1,0,0)) %>% glance()

# auto.arima() = the auto.arima() function from the forecast package 
usmacro %>% dplyr::select(u) %>% auto.arima()
auto.arima(usmacro[,"u"], xreg = usmacro[,"g"]) 

## selects best model based on information criteria , aic and bic
auto.arima(usmacro[,"u"], xreg = usmacro[,"g"], ic = "aic") 
auto.arima(usmacro[,"u"], xreg = usmacro[,"g"], ic = "bic")

# 
library(dynlm)
fit <- dynlm(u~L(u,1)+L(g,0:8),data=ts(usmacro))

BIC(fit)
AIC(fit)


# loop 'BIC()' over multiple ADL models 
order <- 1:8

BICs <- sapply(order, function(x) 
  BIC(dynlm(u ~ L(u, 1:x),data = ts(usmacro,frequency = 4,start=c(1948,1)))))

BICs

# select the AR model with the smallest BIC
BICs[ which.min(BICs)]

#
BICs <- sapply(order, function(x) 
  BIC(dynlm(u~ L(u, x) + L(g, 1:x),data = ts(usmacro,frequency = 4,start=c(1948,1))))) 

BICs[ which.min(BICs)] 


################################################
# Select optimal lag length in ARDL(p,q), more advanced  
####################################

# Initialize variables
p_max <- 12  # Maximum lag for dependent variable
q_max <- 12  # Maximum lag for independent variable


# Initialize storage for model summaries
aic_values <- matrix(NA, nrow = p_max, ncol = q_max)
bic_values <- matrix(NA, nrow = p_max, ncol = q_max)


# Loop through possible lag lengths
for (p in 1:p_max) {
  for (q in 1:q_max) {
    # Define the model formula
    model_formula <- as.formula(paste(
      "u ~", 
      paste(paste0("L(u, ", 1:p, ")"), collapse = " + "), "+",
      paste(paste0("L(g, ", 0:q, ")"), collapse = " + ")
    ))
    
    # Fit the model
    model <- dynlm(model_formula, data = ts(usmacro))
    
    # Calculate AIC and BIC
    aic_values[p, q] <- AIC(model)  # Correctly using AIC
    bic_values[p, q] <- BIC(model)  # Correctly using BIC
  }
}

# The optimal lag lengths based on AIC
optimal_aic_index <- which(aic_values == min(aic_values, na.rm = TRUE), arr.ind = TRUE)
optimal_bic_index <- which(bic_values == min(bic_values, na.rm = TRUE), arr.ind = TRUE)

# optimal lag, based on AIC
cat("Optimal lag length based on AIC: p =", optimal_aic_index[1], ", q =", optimal_aic_index[2], "\n")

# optimal lag, based on BIC
cat("Optimal lag length based on BIC: p =", optimal_bic_index[1], ", q =", optimal_bic_index[2], "\n")



best_model_SC <- dynlm(u ~ L(u,1:2 )+L(g,0:4), data = ts(usmacro))
summary(best_model_SC)


# Testing for Serial Correlation

# H0: no correlation/serial correlation 
# H1: there is correlation 

require(lmtest)
bgtest(best_model_SC ,1) 
bgtest(best_model_SC , order=2) 
bgtest(best_model_SC , order=3)
bgtest(best_model_SC , order=4) 

# extract the residual 
res <- resid(best_model_SC )

# corrorogram 
ggAcf(res) +
  labs(title = "the residul from the best model")


# Testing for Serial Correlation 

# Example 9.10, page 439 
require(dynlm)
require(mosaic)

#' Create ts data
g <- usmacro %>% dplyr::select(g) %>% ts(., start = c(1948,1), frequency = 4)
u <- usmacro %>% dplyr::select(u) %>% ts(., start = c(1948,1), frequency = 4)

#using ts() function will help us to let R know  we are working with 
# time series data 

#'Estimate ARDL(2,1)
fit1 <- dynlm(u~L(u,1)+L(u,2)+L(g,1)) 
summary(fit1)

library(forecast)
residuals(fit1) %>% ggAcf() +
  labs(title = "Figure 9.7: Correlogram from ARDL(2,1) model")

#The correlations for its residuals are generally small and insignificant. 
#There are some correlation at lag 7 , 8 and 17
#however, these correlations are at long lags and barely insignificant.


#' Lagrange multiplier(Lm)/Breusch-Godfrey test for serial correlation
#' H0: No autocorrelation vs H1: there is autocorrelation/Serial correlation 
require(lmtest)
bgtest(fit1) 
bgtest(fit1, order=2) 
bgtest(fit1, order=3)
bgtest(fit1, order=4) 

#' The Durbin-Watson test(the test statistic does not rely on large samples like that of LM test)
#' , HO:no serial correlation 
dwtest(fit1)   



#' Estimate ARDL(1,1)

fit2 <- dynlm(u~L(u,1)+L(g,1))
summary(fit2)


residuals(fit2) %>% ggAcf() +
  labs(title = "Figure 9.8: Correlogram from ARDL(1,1) model")+ theme_bw()

#The first two autocorrelations are significant.
#We conclude that that the errors are serially correlated. 
#More lags are needed to improve the forecasting specifcation, 
#and the least squares standard errors. 



#Lagrange multiplier(Lm)/Breusch-Godfrey test for seral correlation
# Example 9.12 LM test
# H0: No autocorrelation
bgtest(fit2) #order one by deafualt, null is rejected
bgtest(fit2, order=2) 
bgtest(fit2, order=3) 
bgtest(fit2, order=4) 
bgtest(fit2, order=40) 

#' The Durbin-Watson test
#' , HO:no serial correlation 
dwtest(fit2)  #Ho is rejected 
#conclusion: Model 1 is preferred than model 2.



# Example 9.9 Testing for Granger causality

#H0: x doesn't granger cause y (i.e., x doesn't contribute to the forecast of y)

library(lmtest)
grangertest(u ~ g, order = 1, data = usmacro)
grangertest(u ~ g, order = 2, data = usmacro)

#' g does granger cause u

# Chapter 12

rm(list=ls())
library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/gdp5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/gdp5.rdata"))

# Obs: 132 quarterly observations, U.S. data from 1984Q1 to 2016Q4
head(gdp5)
str(gdp5)
gdp.ts <- ts(gdp5$gdp, start = c(1984,1), frequency = 4)


browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usdata5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usdata5.rdata"))

# Obs: 749   Monthly U.S. Data from 1954M8 to 2016M12
# 
# infn =	Annual inflation rate for each month (obtained using infn = 100*(ln(cpi)-ln(cpi(-12)))
#        where CPI is the consumer price index from FRED series CPIAUCSL
# br =	3-year bond rate, percent (3-Year Treasury Constant Maturity Rate, FRED series G3)
# ffr	= Federal funds rate, percent (FRED series FEDFUNDS) 

head(usdata5)                                           

br.ts <- ts(usdata5$br, start = c(1954,8), frequency = 12)
ffr.ts <- ts(usdata5$ffr, start = c(1954,8), frequency = 12)
infn.ts <- ts(usdata5$infn, start = c(1954,8), frequency = 12)

# Copy Figure 12.1
par(mfrow=c(4,2))
# GDP
plot(gdp.ts)
plot(diff(gdp.ts))
# INF
plot(infn.ts)
plot(diff(infn.ts))
# FFR
plot(ffr.ts)
plot(diff(ffr.ts))
# BR
plot(br.ts)
plot(diff(br.ts))
#
par(mfrow=c(1,1))
####                                           

# Table 12.1
round(mean(window(gdp.ts, start=c(1984,2), end=c(2000,3))),2)
round(mean(window(gdp.ts, start=c(2000,4), end=c(2016,4))),2)

round(mean(window(infn.ts, start=c(1954,8), end=c(1985,10))),2)
round(mean(window(infn.ts, start=c(1985,11), end=c(2016,12))),2)

round(mean(window(ffr.ts, start=c(1954,8), end=c(1985,10))),2)
round(mean(window(ffr.ts, start=c(1985,11), end=c(2016,12))),2)

round(mean(window(br.ts, start=c(1954,8), end=c(1985,10))),2)
round(mean(window(br.ts, start=c(1985,11), end=c(2016,12))),2)

# diffs
round(mean(window(diff(gdp.ts), start=c(1984,2), end=c(2000,3))),3)
round(mean(window(diff(gdp.ts), start=c(2000,4), end=c(2016,4))),3)

round(mean(window(diff(infn.ts), start=c(1954,9), end=c(1985,10))),2)
round(mean(window(diff(infn.ts), start=c(1985,11), end=c(2016,12))),3)

round(mean(window(diff(ffr.ts), start=c(1954,9), end=c(1985,10))),2)
round(mean(window(diff(ffr.ts), start=c(1985,11), end=c(2016,12))),2)

round(mean(window(diff(br.ts), start=c(1954,9), end=c(1985,10))),2)
round(mean(window(diff(br.ts), start=c(1985,11), end=c(2016,12))),2)




#  Example 12.2 A deterministic trend for wheat yield

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/toody5.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/toody5.rdata"))
head(toody5)
str(toody5)


# Data on wheat yield over time, in Australia.
# Look how the yield is fluctuating an increasing trend

library(rockchalk) 

fit1 <- lm(y~t, data = toody5) 
summary(fit1)
plotCurves(fit1, plotx = "t", type="l",lty=2, main="a) Yield") 

#' Alternatively 
toody5 %>% ggplot(aes(x=dateid01,y=y))+geom_line()+geom_smooth(method = lm,se=FALSE)

#' Look the graph, the observations fluctuate around the increasing trend with 
#' a particularly bad (very low yield) year in 1969( see the graph for the rainfall in the fig. below;
#' we discover there is a slight downward trend and very little rainfall in 1969)

fit2 <- lm(rain~t, data = toody5)
summary(fit2)
plotCurves(fit2, plotx = "t", type="l", lty=2, main="b) Rain")

toody5 %>% ggplot(aes(x=dateid01,y=rain))+geom_line()+geom_smooth(method = lm,se=FALSE)


#' Find the relationship between yield and rainfall.

#' There are two alternatives to estimate the relationship. 


#' Alternative 1: 

#' Just include trend in the regression.
#' Furthermore, there are decreasing returns to rainfall,so we include RAIN^2 as well as RAIN in the model
#' leading to the following estimated equation.

fit3 <- lm(y~t+rain+I(rain^2), data = toody5)
summary(fit3) 

# If the dependent variable is given in log form
fit4 <- lm(log(y)~t+rain+I(rain^2), data = toody5)
summary(fit4) # 12.10




#  Alternative 2

#' Detrend Yield, RAIN, and RAIN^2 and 
#' estimate the detrended model.

#' First, estimating the trends.
#' That is detrend the variables, y, RAIN, and RAIN^2 

summary(lm(y~t, data = toody5))
summary(lm(rain~t, data = toody5))
summary(lm(I(rain^2)~t, data = toody5))

# Equation 12.11

#' Now apply OLS using the detrended variables 
#' Notices that the estimates should be identical with model "fit3" above.
summary(lm(resid(lm(y~t, data = toody5))~0+resid(lm(rain~t, data = toody5))+resid(lm(rain^2~t, data = toody5))))

#' The standard errors from the two different models are not exactly equal. 
#' The standard error discrepancy arises from the different degrees of freedom used to estimate the error variance. 



#############################################################
### Simulation of different types of AR(1) and random walk model 
##############################################################

set.seed(1234)

#' Simulate an AR1 model, with no intercept (or drift)
a=arima.sim(list(order=c(1,0,0), ar=.7), n=500)

plot(a, main=expression(paste("(a) ",y[t],"=","0.7",y[t-1]+v[t])))
abline(h=mean(a), col="red")

#' ARMAacf () function from stats package 
#' compute the theoretical autocorrelation function or partial autocorrelation function 
#' for ARMA process for an ARMA process
ARMAacf(ar=c(.7),lag.max=10) 
.7    # p y0
.7^2  # p^2 y0
.7^3  # p^3 y0

acfa=acf(a, lag.max=10,type="correlation")
acfa

#install.packages("astsa")
library(astsa)
acf2(a)
cbind(acfa$acf,ARMAacf(ar=c(.7),lag.max=10))

# Estimation of AR(1) model.
#Note that R presents the "mean" of the series in the ARIMA output,not the intercept.

arima(a, order = c(1, 0, 0))  
#' ar(a, aic = T)

#' In the forecast package, the Arima function labels the mean correctly
#install.packages("forecast")
library(forecast)
Arima(a, order = c(1, 0, 0))  

set.seed(1234)
# Simulate an AR1 model, with intercept (or drift)
b=arima.sim(list(order=c(1,0,0), ar=.7), n=500) + 1 

mean(b)
plot(b, main=expression(paste("(b) ",y[t],"=","1+0.7",y[t-1]+v[t])))
abline(h=mean(b), col="blue")

# Estimation of AR(1) model
Arima(b, order = c(1, 0, 0))  

# The result is telling you that the estimated model is 
# b(t) = 1.0113 + .7303*b(t-1) + v(t)
# whereas, it should be telling you the estimated model is
# b(t) - 1.0113 = .7303*[b(t-1) - 1.0113] + v(t)
# or 
# b(t) = 0.2727476 + .7303*x(t-1) + v(t).
# Note that 0.2727476 = 1.0113*(1-.7303), 
# see page 572, the equation before 12.13. R presents the "mean" of the series in the ARIMA output,
# not the intercept.


# It is possible to have the algorithm find the optimal model specification
fit=auto.arima(b)
fit
#tsdiag(fit)
plot(forecast(fit, h = 50))
abline(h=mean(b), col="red")

t <- 1:length(b)
set.seed(1234)
c=arima.sim(list(order=c(1,0,0), ar=.7), n=500) + 1 + 0.01*t

plot(c, main=expression(paste("(c) ",y[t],"=","1+0.01t+0.7",y[t-1]+v[t])))

arima(c, order = c(1,0,0), xreg=1:length(t)) 
mean(c)

Arima(c, order = c(1, 0, 0), xreg=t)  # Estimation of AR(1) model
auto.arima(c)

set.seed(3234)
d=ts(cumsum(rnorm(500)))

plot(d, main=expression(paste("(d) ",y[t],"=",y[t-1]+v[t])))
mean(d)
Arima(d, order = c(1,0,0))

set.seed(1234)
e=ts(cumsum(rnorm(500))+0.1)

plot(e, main=expression(paste("(e) ",y[t],"=","0.1+",y[t-1]+v[t])))

Arima(e, order = c(1,0,0))


set.seed(1234)
f=ts(cumsum(rnorm(500)+0.1+ 0.01*t))

plot(f, main=expression(paste("(f) ",y[t],"=","0.1+0.01t+",y[t-1]+v[t])))
Arima(f, order = c(1,0,0), xreg=1:length(t)) 
mean(f)

# Copy Figure 12.4
par(mfrow=c(3,2))
plot(a, main=expression(paste("(a) ",y[t],"=","0.7",y[t-1]+v[t])))
plot(b, main=expression(paste("(b) ",y[t],"=","1+0.7",y[t-1]+v[t])))
plot(c, main=expression(paste("(c) ",y[t],"=","1+0.01t+0.7",y[t-1]+v[t])))
plot(d, main=expression(paste("(d) ",y[t],"=",y[t-1]+v[t])))
plot(e, main=expression(paste("(e) ",y[t],"=","0.1+",y[t-1]+v[t])))
plot(f, main=expression(paste("(f) ",y[t],"=","0.1+0.01t+",y[t-1]+v[t])))
par(mfrow=c(1,1))



#  Spurious Regression 


#Example 12.3:  Regression with two random walks

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/spurious.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/spurious.rdata"))

head(spurious)

summary(lm(rw1~rw2, data = spurious))

library(lattice)
Data.ts <- ts(spurious)
xyplot(Data.ts[, 1:2], superpose = TRUE)
names(spurious)
plot(spurious$rw2,spurious$rw1, xlab="rw2", ylab="rw1") #scatter plot 

require(lmtest)
mod1=lm(rw1~rw2, data = spurious)
summary(mod1)

#' This result suggests that the simple regression model fits the data well (look at the R^2)
#' and the estimated slope is significantly different from zero.In fact, the t-statistic is huge!
#' These results are, however, completely meaningless, or spurious. The apparent significance of the 
#' relationship is false. Because the two series are have nothing in common, nor are they causally related in any way.
#' look at the serial correlation of the model(very high correlation), which is an indication that
#' something is wrong with the regression.

bgtest(mod1)
require(car)
durbinWatsonTest(lm(rw1~rw2, data = spurious))

#-----------------------------------------
require(astsa)
rw1 <- ts(spurious$rw1)
rw2 <- ts(spurious$rw2)

acf2(rw1)
Arima(rw1, order = c(1,0,0), xreg=rw2) 

#--------------------------------------------




#   Unit Root Tests for Stationarity 


#Ho: unit root (non-stationary) vs H1: stationary

rm(list = ls())
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usdata5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usdata5.rdata"))
head(usdata5)
str(usdata5)
tail(usdata5)
# Obs: 749   Monthly U.S. Data from 1954M8 to 2016M12
# 
# infn=	Annual inflation rate for each month (obtained using infn = 100*(ln(cpi)-ln(cpi(-12)))
# where CPI is the consumer price index from FRED series CPIAUCSL
# br=	3-year bond rate, percent (3-Year Treasury Constant Maturity Rate, FRED series G3)
# ffr= Federal funds rate, percent (FRED series FEDFUNDS) 

br.ts <- ts(usdata5$br, start = c(1954,8), frequency = 12) 
ffr.ts <- ts(usdata5$ffr, start = c(1954,8), frequency = 12)
infn.ts <- ts(usdata5$infn, start = c(1954,8), frequency = 12)


library(dynlm)

#' Example 12.4:  PP. 579 

#' Augmented Dickey-Fuller test with intercept, No trend 

#' H0:unit root (non-stationary).

summary(dynlm(d(ffr.ts) ~ L(ffr.ts)+d(L(ffr.ts,1:2)))) 
summary(dynlm(d(br.ts) ~ L(br.ts)+d(L(br.ts,1:2))))

#' For checking stationary, the usual t-critical values and p-values cannot be used
#' Instead we compare the t-critical value for the first lag of the dep.var. with the critical value from Table 12.2. (see the Text) 
#' Reject Ho if tau(t-value) <= t_Cv, (t_Cv=-2.86 at 5% level)
#' Notice here two augmentation terms have been included for both variables, to account for serial correlation. 


# Example 12.5: Is GDP trend stationary?

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/gdp5.rdata"))
# Obs: 132 quarterly observations, U.S. data from 1984Q1 to 2016Q4
head(gdp5)

gdp.ts <- ts(gdp5$gdp, start = c(1984,1), frequency = 4)

plot.ts(gdp.ts)

t <- 0:length(gdp.ts)
t <- ts(t, start = c(1984,1), frequency = 4)

summary(dynlm(d(gdp.ts) ~ t + L(gdp.ts)+d(L(gdp.ts,1:2))))

#' two augmentation terms minimized the SC, eliminated major autocorrelation in 
#' the residuals.

#' H0:unit root, Reject Ho if tau(t-value) <= t_Cv, (t_Cv=-3.41 at 5% level). 
#' conclusion: tau=-1.999 <=-3.41,  False!
#'  Hence, GDP follows a non-stationary random walk. 
#' Thus,there is insufficient evidence to conclude that GDP is trend stationary. 


#' Example 12.6. Is wheat yield trend stationary? 

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/toody5.rdata"))
head(toody5)

yield.ts <- ts(toody5$y, start = 1950, frequency = 1)

plot.ts(yield.ts)

t <- ts(toody5$t, start = 1950, frequency = 1)
summary(dynlm(d(log(yield.ts)) ~ t + L(log(yield.ts))))


#' Thus, we reject the null hypothesis of non-stationarity and
#' conclude that ln(yield) is trend stationary.



#' Example 12.7: Order of Integration of variables

#' Above, we showed that ffr.ts, and br.ts are non-stationary.
#' To find their order of integration,
#' we ask the question: Are their first differences stationary?
#' Their plots fluctuate around zero.

#' Manually (on levels), p. 581
summary(dynlm(d(d(ffr.ts)) ~ 0 + L(d(ffr.ts))+d(d(L(ffr.ts)))))
summary(dynlm(d(d(br.ts)) ~ 0 + L(d(br.ts))+d(d(L(br.ts)))))

#' Given their plots fluctuate around zero, we use Dickey-Fuller 
#' test equation with no intercept and no trend.
plot(diff(ffr.ts))
plot(diff(br.ts))


#'Conclusion:  
#' The null is rejected in either series.
#'  and conclude that both series are stationary at their first difference

#' Hence, we say that the series ffr.ts and br.ts are I(1) because they had to be 
#' differences once to make them stationary. 


#' Stationarity test directly using R packages  

#' install.packages("urca")
library(urca)
#' helpful link 
browseURL("https://stats.stackexchange.com/questions/24072/interpreting-rs-ur-df-dickey-fuller-unit-root-test-results")

??ur.df

summary(ur.df(ffr.ts, type = "drift", lags = 2)) # do not reject H0 on Nonstationary at 1%
summary(ur.df(br.ts, type = "drift", lags = 2)) # do not reject H0 on Nonstationary at 5%


# The null hypothesis is nonstationarity, it now rejected on both series
# The series are stationary on first diff, hence they are I(1).
summary(ur.df(diff(ffr.ts), type = "none", lags = 0)) #none=neither an intercept nor a trend is included in the test regression 
summary(ur.df(diff(br.ts), type = "none", lags = 0))

#install.packages("tseries")
library(tseries)

# Automatic ADF test
adf.test(ffr.ts)
adf.test(diff(ffr.ts))

?adf.test
#k- is the optimal lag of the differenced term on the right side of the equation, added to account serial correlation 

adf.test(br.ts)
adf.test(diff(br.ts))

# Phillips-Perron test
# A nonparametric correction for autocorrelation (essentially employing a HAC
# estimate of the long-run variance in a Dickey-Fuller-type test
?pp.test
pp.test(ffr.ts, type = "Z(t_alpha)")
pp.test(diff(ffr.ts), type = "Z(t_alpha)")

pp.test(br.ts, type = "Z(t_alpha)")
pp.test(diff(br.ts), type = "Z(t_alpha)")



#  Cointegration test 


#' Co-integration:- the relationship between I(1) variables 
#' such as the residuas are I(0)
#' 
#' H0: no cointegration vs cointegration 

#' Two step procedure by Engle and Granger
summary(dynlm(br.ts~ffr.ts))
# Extract the residuals from the model 
e=resid(dynlm(br.ts~ffr.ts)) 

# Stationarity test of the residuals 
summary(dynlm(d(e) ~ 0+L(e,1)+L(d(e),1:2)))
# Compare the t-value of first lag of e with the 5% critical value
# T_c = 3.37 (See Table 12.4 in the text book).
# Reject H0: no-cointegration when t-value <= t_c


#' Alternatively, just check whether the error is stationary or not 
summary(ur.df(e, type = "none", lags = 2))
adf.test(e)
pp.test(e, type = "Z(t_alpha)")

# Still another method, direct method, br is column 2, and ffr is column 3 of usa data
head(usdata5)

library(tseries)
po.test(usdata5[,3:2])  #Phillips-Ouliaris Cointegration Test, H0: no cointegration vs H1: Cointegration


#' Another cointegration approach, Johansen test for co-integration, 
#' H0: no co-integration vs H1: Co-integration
library(urca)
?ca.jo
johansen <- ca.jo(usdata5[,3:2], ecdet = "const", type = "trace")
summary(johansen)



#  Error Correction Model

# A relationship between I(1) variables (or co-integration) is often referred to as 
# long-run relationship while a relationship between I(0) variables is often referred to as a short-run relationship.
# Error correction model is a dynamic relationship between I(0) variables, which embeds a cointegrating relationship


#' Several ways to estimate Error correction model 

B <- br.ts
F <- ffr.ts

# Error correction model, when B is the dep.variable 
Error_corr_B=dynlm(diff(B)~0+L(e,1)+L(diff(B),1:2)+L(diff(F),0:4))
summary(Error_corr_B)
# Error correction model, when F is the dep.variable 
Error_corr_F=dynlm(diff(F)~0+L(e,1)+L(diff(F),1:2)+L(diff(B),0:4))
summary(Error_corr_F)

#Using VECM function 
library(tsDyn)
Vector_Error <- VECM(cbind(B,F), lag = 4, include = "none")
summary(Vector_Error)

#' Alternatively, Estimate directly using non-linear least squares or 
#' use OLS to estimate the modified model and then retrieve the parameters  

Data <- ts.intersect(dB=diff(B),lagB=stats::lag(B,-1),lagF=stats::lag(F,-1),dF=diff(F),lagdF=stats::lag(diff(F),-1), dframe=TRUE)

require(mosaic)

#' options(scipen=999)
beta2 <- coef(lm(dB~lagB+lagF+dF+lagdF, data=Data))  #see page 585 and 584 
beta2
#
g <- fitModel(dB~A*lagB+A*B1+A*B2*lagF+D*dF+D1*lagdF, data=Data, start=list(A=beta2[2],B1=beta2[1]/beta2[2],B2=beta2[3],D=beta2[4],D1=beta2[5]))
summary(g)

# Example 13.1

#' Estimating a VEC model
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/gdp.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/gdp.rdata"))
str(gdp)
head(gdp)

usa <- ts(gdp$usa, start=c(1970,1), end=c(2000,4), frequency=4)
aus <- ts(gdp$aus, start=c(1970,1), end=c(2000,4), frequency=4)

ts.plot(usa,aus, type="l", lty=c(1,2), col=c(1,2), main="GDP")
legend("topleft", border=NULL, legend=c("USA","AUS"), lty=c(1,2), col=c(1,2))

#' They have a common trend, and a non constant mean, 
#' the series are probably nonstationary.
library(urca)
summary(ur.df(usa, type = "trend", selectlags = "BIC"))#keep Ho: non-stationary
summary(ur.df(aus, type = "trend", selectlags = "BIC"))#keep Ho

# first difference 
summary(ur.df(diff(usa), type = "none", selectlags = "BIC"))# Reject Ho
summary(ur.df(diff(aus), type = "none", selectlags = "BIC"))# Reject H0
#' The stationarity tests indicate that both series are I(1).
#' Hence, we can perform cointegration test 


#' Check for cointegration.
#' Estimate the long-run relationship
library(dynlm)
fit1 <- dynlm(aus~0+usa)
summary(fit1) 
#' The intercept term is omitted because it has 
#' no economic meaning.
#' If USA were to increase by 1 unit, Australia would increase 
#' by 0.985 unit. 

#' residuals 
ehat <- resid(fit1)
plot(ehat)

# check the stationarity of the residual
# 1. Autocorrelation 
library(forecast)
ggAcf(ehat) + labs(title = "Correlogram for the residual")
#' A visual inspection of the the time series suggests that 
#' the residuals may be stationary 

#' A formal test
# 2) Engle-Granger test 
fit2 <- dynlm(d(ehat)~L(ehat)-1)
summary(fit2)
# A 5% level critical value is -2.76.
#' Our test rejects the null of no cointegration, meaning that the series are cointegrated.
#' With cointegrated series we can construct a VEC model to better understand the causal relationship between the two variables.

# Alternatively 
summary(ur.df(ehat, type = "none", selectlags = "BIC"))# Reject H0

#' A cointegration test using the Johansen Approach 

library(vars)

joh=ca.jo(cbind(aus,usa), type = "trace", ecdet = c("const"), K = 2, spec = "longrun")
summary(joh)

#Example 13.9: Vector (Error correction model)
vecaus<- dynlm(d(aus)~L(ehat))
vecusa <- dynlm(d(usa)~L(ehat))

summary(vecaus)
summary(vecusa)

#' The coefficient on the error correction term e_(t-1) is significant for Australia, 
#' suggesting that changes in the US (large) economy do affect Australian (small) economy.
#' 
#' The error correction coefficient in the US equation is not statistically significant,
#' suggesting that changes in Australia do not influence American economy.
#' 

#Fit a VECM with Engle-Granger 2OLS estimator:
vecm.eg <- VECM(cbind(aus,usa), lag=1, estim = "2OLS")
vecm.eg
summary(vecm.eg)

??VECM

#Fit a VECM with Johansen MLE estimator:
vecm.jo <- VECM(gdp, lag=1, estim="ML")
vecm.jo
summary(vecm.jo)

#' The VEC model is a multivariate dynamic model that incorporates a cointegration equation.
#' It is relevant when we have two variables that are both I(1), but are cointegrated.
#' 
#' r <- read.table("http://www.principlesofeconometrics.com/poe4/data/dat/byd.dat")
names(r) <- "r"
head(r)
plot(r$r, type="l")
abline(h=mean(r$r), col="red", lwd=2)
#The time series shows evidence of time-varying volatility and clustering

plot(density(r$r))
#Summary of the data 
summary(r)
sd(r$r) 

#Normality test 
#install.packages("tseries")
library(tseries)
jarque.bera.test(ts(r$r))

#So, the time series shows evidence of time-varying volatility and clustering, and 
#the unconditional distribution of is non-normal. 


#-------------------------------------
#random.r <- rnorm(500,1,1)
#summary(random.r)
#plot(r$r, type="l")
#lines(random.r, type="l", col="red")
#-------------------------------------------------

#
#

#Testing ARCH effects and estimating volatility

#First, estimate the mean equation(in this example we take, rt=b0 +et) 
#where rt is the monthly return.
#Second, retrieve the estimated residuals, and fit a model for the residuals 
#Third, estimate (14.3) and us LM test 
#Fourth, estimate the variance equation, if the data has an ARCH effect

rt=ts(r, freq=1)

# Step-1: the mean equation 
m1 <- lm(rt~1)
summary(m1)

#step-2:Regress residual square ~ constant plus lagged residual square (eq14.3) 
library(dynlm)
e2=ts(resid(m1)^2,freq=1)  
m2 <- dynlm(e2~L(e2, 1))
summary(m2)

#step-3: Use LM test, Ho:no ARCH vs H1: ARCH effect  
#LM-statistic=(T-q)*R^2, compare with  chisqr(0.95,q=1), where q - is the order of lag in the rhS

#Reject H0 if LM-stastic is greater than or equal to chisqr(0.95,q=1)
out=summary(m2)
names(out)

out$df[2]*out$r.squared   
## [1] 61.91036
# Since  61.91036 >chisqr(0.95,1)=3.841
# we reject the null hypothesis.

out$df[2]*out$r.squared  > qchisq(0.95,1)


#You can also Check Autocorrelation
# We reject the null hypothesis of no autocorrelation
# if the p-value is smaller than alpha.
Box.test(r^2)
Box.test(r^2, type="Ljung-Box") 

#step-4: Estimating the variance model 

#Function GARCH(p,q) from the tseries package, becomes an ARCH model when
#used with the order= c(0,1). 

#This function can be used to estimate and plot the variance ht 
?garch

require(tseries) 
byd.arch <- garch(ts(resid(m1),freq=1) ,c(0,1))
summary(byd.arch)

#Fitted values, volatility/variance
head(byd.arch$fitted.values)
hhat <- ts(byd.arch$fitted.values[-1,1]^2)
plot.ts(hhat)


#Both the mean and the variance models can be estimated 
#simultaneously using the package "rugarch".
# There are two steps to follow to fit GARCH or ARCH model using rugarch pachage.

library(rugarch)

# First step, specify the mean and the variance model using the function "ugarchspec"
#ugarchspec - is method for creating a univariate GARCH specification object prior to fitting.
??ugarchspec

garchSpec <- ugarchspec(
  mean.model=list(armaOrder=c(0,0)),
  variance.model=list(model="sGARCH",garchOrder=c(1,0)),  # Standard GARCH model (sGARCH)
  distribution.model="std")

#Second step, fit the model using the function "ugarchfit"
#ugarchfit - method for fitting a variety of univariate GARCH models.
#??ugarchfit

garchFit <- ugarchfit(spec=garchSpec, data=rt)
coef(garchFit)
pint(garchFit)

names(garchFit@fit) # @ call-the call of the garch function

#Fitted value of the mean model
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)

#Fitted values of the variance model
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat, main="Standard GARCH model (sGARCH) with dataset 'byd'")

#
# There are different GRACH models such as:
# T_GARCH, GARCH-in-Mean, Exponential GARCH (EGARCH) model, etc. 

# T-GARCH model-  allows us to model asymmetric effect treating bad news and good 
# news terms in the model

#In R, the "fGARCH model" subsumes many GARCH models. 

# TGARCH 
garchMod <- ugarchspec(variance.model=list(model="fGARCH",
                                           garchOrder=c(1,1),
                                           submodel="TGARCH"),
                       mean.model=list(armaOrder=c(0,0)), 
                       distribution.model="std")

garchFit <- ugarchfit(spec=garchMod, data=rt)
coef(garchFit)
print(garchFit)

#Fitted mean model 
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)

#Fitted variance 
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat, main="The tGARCH model with dataset 'byd' ")


# GARCH-in-mean
garchMod <- ugarchspec(
  variance.model=list(model="fGARCH",
                      garchOrder=c(1,1),
                      submodel="APARCH"),
  mean.model=list(armaOrder=c(0,0),
                  include.mean=TRUE,
                  archm=TRUE,
                  archpow=2), 
  distribution.model="std")

garchFit <- ugarchfit(spec=garchMod, data=rt)
coef(garchFit)

#Fitted mean model
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)

#Fitted variance model
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat, main="Version of the GARCH-in-mean model with dataset 'byd' ")

#---------------------------------------------------------------------
require(fGarch) 

# eq 14.4a & b
summary(garchFit(~garch(1,0),r)) 

#' r=b0=mu
#' a0=omega
#' a1=alpha1

#plot(garchFit(~garch(1,0),r))

summary(garchFit(~garch(1,1),r)) # chap 14.4.1 eq.14.7
#' r=mu
#' a0=omega
#' a1=alpha1
#' b1=beta1
#plot(garchFit(~garch(1,1),r))
#----------------------------------------------------------------------

#install.packages("rugarch")
library(rugarch)
r_garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(0, 0)))
r_garch11_fit <- ugarchfit(spec = r_garch11_spec, data = r)
r_garch11_fit

#------------------------------------------------------------------------
#Backtesting VaR
r_garch11_roll <- ugarchroll(r_garch11_spec, r, n.start = 120, refit.every = 1,
                             refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                             VaR.alpha = 0.01, keep.coef = TRUE)

report(r_garch11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)

# Forecast
r_garch11_fcst <- ugarchforecast(r_garch11_fit, n.ahead = 20)
r_garch11_fcst

#plot(r_garch11_fcst)

# Threshold Garch
# Tsay
# Source/run these functions first
# http://faculty.chicagobooth.edu/ruey.tsay/teaching/introTS/Tgarch11.R
#m3=Tgarch11(r)

## 4) Garch in mean 
# Tsay
# Source/run these functions first
# http://faculty.chicagobooth.edu/ruey.tsay/teaching/introTS/garchM.R
#m4=garchM(r)


#Prediction can be obtained using the function ugarchboot() from the package ugarch.
#Supplementary:
