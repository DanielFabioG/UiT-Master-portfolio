

# Autocorrelation

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




# autocorrelation function (acf)

# default function for corrologram
# shows corrolation between time series and lagged values, (y_t, y_t-k) 
# if the bars cross the blue lines, the corrolation is significant
# if it goes under the corrolation between lagged values and current values are not different from zero
acf(usmacro$u)

library(forecast)
usmacro %>%
  select(u) %>%
  ggAcf() +
  labs(title = "Corrologram for the U.S. quarterly unemployment")

