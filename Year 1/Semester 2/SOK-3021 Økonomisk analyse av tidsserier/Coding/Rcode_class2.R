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