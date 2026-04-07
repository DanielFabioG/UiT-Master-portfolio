
library(quantmod) # extract stock price data from Yahoo Finance
library(readxl)   # Import xlsx data
library(tseries)
library(FinTS) # for ARCH test
library(rugarch) # ARCH GARCH model
library(rmgarch) # DCC GARCH


#$ Read and merge data
###################

D.Oil <- read_excel("PET_PRI_SPT_S1_D.xls", sheet='tilR') # download the Excel with oil price from Canvas
D.Oil = as.data.frame (D.Oil); nrow(D.Oil) # head(D.Oil)                                            
D.Oil = D.Oil[complete.cases(D.Oil),]; nrow(D.Oil)

#❗ Replace XOM with the oil company you selected.

getSymbols("XOM", src = "yahoo", from = "2010-01-01", to = "2025-12-31") # head(XOM); nrow(XOM)
XOM1 = data.frame(row.names(data.frame(XOM)), XOM[,6]); head(XOM1)
names(XOM1) = c('Date', 'XOM'); row.names(XOM1) = NULL
XOM1 = XOM1[complete.cases(XOM1),]; nrow(XOM1)

#❗ Replace NEE with the new energy company you selected.

getSymbols("NEE", src = "yahoo",from = "2010-01-01", to = "2025-12-31")
     # head(NEE); nrow(NEE)
NEE1 = data.frame(row.names(data.frame(NEE)), NEE[,6]); head(NEE1)
names(NEE1) = c('Date', 'NEE'); row.names(NEE1) = NULL
NEE1 = NEE1[complete.cases(NEE1),]; nrow(NEE1)

nrow(XOM1); nrow(NEE1);  nrow(D.Oil)

Da = merge(XOM1,NEE1,by='Date' ); nrow(Da); head(Da)
D.Oil$Date = as.character(D.Oil$Date)
    
Da2 = merge(Da,D.Oil,by='Date' ); nrow(Da2)
Da2 = Da2[Da2$Oil > 0, ]
Da2 = Da2[order(Da2$Date), ]

summary(Da2)


#$ Calculate returns
###################

X0 = diff(log(Da2$Oil))*100;
Y0 = diff(log(Da2$XOM))*100;
Z0 = diff(log(Da2$NEE))*100;
Date = as.Date(Da2$Date[2:nrow(Da2)])

Da3 = data.frame(Date,X0, Y0, Z0);
summary(Da3)

#$ delete outliers
sort(Da3$X)[1:50]
sort(Da3$X, decreasing = TRUE)[1:50]

Da4 = Da3[Da3$X0 < 10 & Da3$X0 > -10,]
Da4 = Da4[Da4$Y0 < 10 & Da4$Y0 > -10,]
Da4 = Da4[Da4$Z0 < 10 & Da4$Z0 > -10,]

summary(Da4)

# Convert variables into a Time Series Object
#$ Cannot use TS for the reason of missing values
###################

X <- zoo(Da4$X0, order.by = Da4$Date)
X <- as.xts(X)

Y <- zoo(Da4$Y0, order.by = Da4$Date)
Y <- as.xts(Y)

Z <- zoo(Da4$Z0, order.by = Da4$Date)
Z <- as.xts(Z)


#$ Time-invarying variance and correlation
##################

var(Y);
var(Z)
cor(Y,X);
cor(Z,X)

par(mar = c(5, 5, 4, 2)+ 0.1)
plot(X, type = "l", main="log returns Oil price", xlab="",col = "black",); #abline(h=0, col="red", lwd=2)
plot(Y, type = "l", main="log returns Oil company", xlab="",col = "blue",); #abline(h=0, col="red", lwd=2)
plot(Z, type = "l", main="log returns New Energy company", xlab="",col = "green",); #abline(h=0, col="red", lwd=2)



##  ARCH Test
##  H0: There are no ARCH effects in the residuals.

arch_test <- ArchTest(X, lags = 5); print(arch_test)
arch_test <- ArchTest(Y, lags = 5); print(arch_test)
arch_test <- ArchTest(Z, lags = 5); print(arch_test)


##  ARCH model
###################

arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), 
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))

arch_fit <- ugarchfit(spec = arch_spec, data = Y)
print(arch_fit)
cond_var <- sigma(arch_fit)^2
plot( cond_var, type = "l", col = "blue",
      main = "Conditional Variance (ARCH model)", xlab = "")


arch_fit <- ugarchfit(spec = arch_spec, data = Z)
print(arch_fit)
cond_var <- sigma(arch_fit)^2; summary(cond_var)
plot( cond_var, type = "l", col = "green",
      main = "Conditional Variance (ARCH model)", xlab = "")



# GARCH Model
###################

garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
     # sGARCH = standard GARCH

garch_fit <- ugarchfit(spec = garch_spec, data = Y)
print(garch_fit)
cond_var <- sigma(garch_fit)^2
plot( cond_var, type = "l", col = "blue",
      main = "Conditional Variance (GARCH model)", xlab = "")

garch_fit <- ugarchfit(spec = garch_spec, data = Z)
print(garch_fit)
cond_var <- sigma(garch_fit)^2
plot( cond_var, type = "l", col = "green",
      main = "Conditional Variance (GARCH model)", xlab = "")




# T-GARCH Model
###################

tgarch_spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))

tgarch_fit <- ugarchfit(spec = tgarch_spec, data = Y)
print(tgarch_fit)
cond_var <- sigma(tgarch_fit)^2
plot( cond_var, type = "l", col = "blue",
      main = "Conditional Variance (T-GARCH model)", xlab = "")


tgarch_fit <- ugarchfit(spec = tgarch_spec, data = Z)
print(tgarch_fit)
cond_var <- sigma(tgarch_fit)^2
plot( cond_var, type = "l", col = "green",
      main = "Conditional Variance (T-GARCH model)", xlab = "")



# DCC-GARCH Model
###################

# Define a univariate GARCH(1,1)
uspec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
)

# Define the multivariate DCC-GARCH
dccspec <- dccspec(
  uspec = multispec(replicate(2, uspec)),  
  dccOrder = c(1, 1),                      
  distribution = "mvnorm"             
)

# Fit the DCC-GARCH model for X and Y
dccfit <- dccfit(dccspec, data = data.frame(X,Y))
print(dccfit)
dcc_cor <- data.frame ( rcor(dccfit)[1,2,]); # head(dcc_cor)

XY <- zoo(dcc_cor[,1], order.by = Date)
XY <- as.xts(XY)

# Fig
par(mar = c(5, 5, 4, 2)+ 0.1)
plot( XY, type = "l", col = "blue",
     main = "Dynamic Conditional Correlation", xlab = "Date", ylab = "Correlation")


# Fit the DCC-GARCH model for X and Z
dccfit <- dccfit(dccspec, data = data.frame(X,Z))
print(dccfit)
dcc_cor <- data.frame ( rcor(dccfit)[1,2,]); # head(dcc_cor)

# Fig
XZ <- zoo(dcc_cor[,1], order.by = Date)
XZ <- as.xts(XZ)

par(mar = c(5, 5, 4, 2)+ 0.1)
plot( XZ, type = "l", col = "green",
      main = "Dynamic Conditional Correlation", xlab = "Date", ylab = "Correlation")

