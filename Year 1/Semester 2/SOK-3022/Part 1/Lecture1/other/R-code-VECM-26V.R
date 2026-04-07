
                                                      ###  VECM
                                                      ######################
                                                      

# Install/Load necessary libraries
                                                      
library(readxl)   # Import xlsx data
library(tseries)  # For ADF test
library(car)      # Choose lag in VAR
library(urca)     # For Johansen test
library(vars)     # For VECM wo SE
library(tsDyn)    # For VECM with SE for Alpha

                                                      
##########
##### Import data and clean/organize data
##########
                                                      
# D.CI <- read.csv("path_to_your_file/Data name.csv")
# or                                                       
# D.CI <- read_excel("path_to_your_file/Data name.xlsx", sheet='name of the sheet')
# D.CI = as.data.frame (D.CI); nrow(D.CI) # head(D.CI)                                            
                                                      
D.CI =  read_excel("Data_World_Development_Indicators.xlsx",sheet='tilR');
D.CI = as.data.frame (D.CI); nrow(D.CI) # head(D.CI)                                            

D.CI1 = D.CI[D.CI[,4]%in%c('EN.GHG.CO2.MT.CE.AR5','NE.GDI.TOTL.KD','NY.GDP.MKTP.KD'), ]; nrow(D.CI)
D.CI1[,1]=D.CI1[,3]=NULL

varialbe = as.factor(D.CI1[,2]); levels(varialbe)
levels(varialbe) = c('CO2','Fin','GDP')                                                   
D.CI1[,2] = varialbe

names(D.CI1) = c('Country','Series',c(1960:2023))# str(D.CI1)

D.CI2 = reshape(D.CI1, direction ='long', varying=list (names(D.CI1)[3:ncol(D.CI1)] ), v.names = "v", idvar = c("Country",'Series'), timevar='Year', time=1960:2023)
D.CI3 = reshape(D.CI2, idvar=c("Country","Year"), v.names=c('v'), timevar=c("Series"),direction="wide") ;  # head(D.CI3)


##########
##### Choose a sample country 
##########

unique(D.CI3$Country)

Dat = D.CI3[D.CI3$Country=='USA',]; nrow(Dat) # select a country
    sort(Dat$Year)
Dat = Dat

Dat = Dat[Dat$v.CO2>0, ]; nrow(Dat)
Dat = Dat[Dat$v.GDP>0, ]; nrow(Dat)
Dat = Dat[Dat$v.Fin>0, ]; nrow(Dat)

unique(Dat$Year)

Year = Dat$Year
CO2 = log(Dat$v.CO2); summary(CO2)
FD = log(Dat$v.Fin); summary(FD)
GDP = log(Dat$v.GDP); summary(GDP)


#$ ADF Test for stationary
####################

adf.test(CO2)
adf.test(FD)
adf.test(GDP)


#$ Johansen Test
#################

#$ Johansen: CO2 and FD  # ?ca.jo()
J_test1 <- ca.jo(data.frame(CO2,FD), type = "eigen", ecdet = c("const"), K = 2); summary(J_test1)
J_test2 <- ca.jo(data.frame(CO2,FD), type = "trace", ecdet = c("const"), K = 2); summary(J_test2)


#$ Johansen: CO2 and GDP
J_test3 <- ca.jo(data.frame(CO2,GDP), type = "eigen", ecdet = c("const"), K = 2); summary(J_test3)
J_test4 <- ca.jo(data.frame(CO2,GDP), type = "trace", ecdet = c("const"), K = 2); summary(J_test4)


#$ Johansen: FD and GDP
J_test5 <- ca.jo(data.frame(FD,GDP), type = "eigen", ecdet = c("const"), K = 2); summary(J_test5)
J_test6 <- ca.jo(data.frame(FD,GDP), type = "trace", ecdet = c("const"), K = 2); summary(J_test6)


#$ Johansen: CO2, FD, and GDP

VARselect(data.frame(CO2,FD,GDP), lag.max = 10, type = "trend")$selection

J_test7 <- ca.jo(data.frame(CO2,FD,GDP), type = "eigen", ecdet = c("const"), K = 2); summary(J_test7)
J_test8 <- ca.jo(data.frame(CO2,FD,GDP), type = "trace", ecdet = c("const"), K = 2); summary(J_test8)



#$ VECM Function CO2 and FD
#################

Vecm2 = VECM(data.frame(CO2,FD), lag = 1, r=1, estim = "ML",include=c('const') )# ?VECM()
summary(Vecm2)

#$ long-run, beta

round(Vecm2$model.specific$beta,3)
cov = cov( residuals (Vecm2))
t_stats <- Vecm2$model.specific$beta / sqrt(diag(cov))
p_values <- 2 * (1 - pt(abs(t_stats), df = length(CO2) - 2))
p_values

#$ short-run, alpha

round ( summary(Vecm2)$coefMat, 3)


#$ VECM for CO2, FD, and GDP
#################

Vecm2 = VECM(data.frame(CO2,FD,GDP), lag = 1, r=1, estim = "ML",include=c('const') )# ?VECM()
summary(Vecm2)

#$ long-run, beta

round(Vecm2$model.specific$beta,3)
cov = cov( residuals (Vecm2))
t_stats <- Vecm2$model.specific$beta / sqrt(diag(cov))
p_values <- 2 * (1 - pt(abs(t_stats), df = length(CO2) - 2))
p_values

#$ short-run, alpha

round ( summary(Vecm2)$coefMat, 3)

