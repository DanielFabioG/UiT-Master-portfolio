# Chapter 4

rm(list=ls())
library(mosaic)
library(rockchalk) 
library(stargazer)

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

#' Estimating the parameters of the simple linear regression model
m1 <- lm(food_exp ~ income, data=food)
summary(m1)

#' The predicted value: E(food_exp|income)
f <- makeFun(m1)                  
#' E(food_exp|income=20)
f(20) 
#' manually
coef(m1)[1]+coef(m1)[2]*20

#' Example 4.1
#' A prediction interval reports the interval estimate for an individual value of y (y0) for a particular value of x (x0).
f(20, interval='prediction')  
f(20, interval='prediction', level=0.9) # 90% PI

#' Plot of food expenditure model with prediction interval
plotSlopes(m1, plotx = "income", interval="prediction", col="red", opacity = 80)

#' A confidence interval reports the interval estimate for the mean value of y for a given value of x
f(20, interval='confidence')  
plotSlopes(m1, plotx = "income", interval="confidence", col="green", opacity = 80) 

#' Plot of both 95% PI and 95% CI   #at the same time , 
xyplot(food_exp~income, data=food, panel=panel.lmbands, cex=1.1)
ladd(panel.abline(v=20, col="red", lwd=3, alpha=.4 ) ) 


#######################################
#' Measuring Goodness-of-Fit
#######################################

#' The models subsequent sums of squares and ANOVA table
anova(m1)   
#' Sums of squared residuals, SSE
deviance(m1)    
#' The variance in the model 
deviance(m1)/m1$df.residual   
#' Residual standard error
sqrt(deviance(m1)/m1$df.residual)

#' Sums of Squares, using the fomulas .....
sum((food$food_exp - mean(food$food_exp))^2) # SST
sum((predict(m1) - mean(food$food_exp))^2)  # SSR
deviance(m1) # SSE
sum((food$food_exp - predict(m1))^2) # SSE

SST=sum((food$food_exp - mean(food$food_exp))^2)
SSR=sum((predict(m1) - mean(food$food_exp))^2)
SSE=deviance(m1) 

SST ; SSR+SSE

#' $R^{2}$
SSR/SST
#' or
1-SSE/SST   


#' Example 4.2 correlation
cor(food)     
r <- cov(food$food_exp,food$income)/(sd(food$food_exp)*sd(food$income)) ; r
r^2     

#' Example 4.3

summary(m1)   
stargazer(m1, type="text", intercept.bottom = FALSE) 


########################################################
#' Modeling issues: The Effect of Scaling the Data  
##########################################################

#' food_exp : weekly food expenditure in $
#' income  : weekly income in $100

#' units: $ ~ 100$   #that means 1unit of income equals 100$
head(food)
m1 <- lm(food_exp~income, data=food)
m1     

#' Generate a new income variable in dollars
food <- food %>% mutate(income.dollars=income*100)  
head(food)

#' units: $ ~ $
m2 <- lm(food_exp~income.dollars, data=food)

stargazer(m1, m2, type="text", intercept.bottom=FALSE)  


#' Generate a new food_exp variable in 100 dollars
food <- food %>% mutate(food_exp.100dollars=food_exp/100)  
head(food)

#' units: 100$ ~ 100$
m3 <- lm(food_exp.100dollars~income, data=food)

stargazer(m1, m2, m3, type="text", intercept.bottom=FALSE)  


#' units: 100$ ~ 1$    
m4 <- lm(food_exp.100dollars~income.dollars, data=food)

stargazer(m1, m2, m3, m4, type="text", intercept.bottom=FALSE)


#' slope in m1: $ vs 100$
coef(m1)[2]

#' slope in m1: in $ vs $
coef(m1)[2]/100

#' slope in m2: in $ vs $
coef(m2)[2]

#' slope in m3: 100$ ~ 100$ or $ ~ $
coef(m3)[2]

#' slope in m4: 100$ ~ $
coef(m4)[2]

#' slope in m4: $ ~ $
coef(m4)[2]*100


##################################
#' Choosing a functional form  
#####################################  

#' Some plots like Figure 4.5
plotFun(0.5+0.3*x^2 ~ x, x.lim=range(-5,5), main="Quadratic equations")
plotFun(3-0.3*x^2 ~ x, add=T , col="red" )

plotFun(0.5+0.3*x^3 ~ x, x.lim=range(-5,5), main="Cubic equations")
plotFun(3-0.3*x^3 ~ x, add=T , col="red" )

plotFun(exp(2+1.2*log(x)) ~ x, x.lim=range(0,4), main="Log-log equations")
plotFun(exp(2+0.9*log(x)) ~ x, add=T , col="red" )

plotFun(exp(2-1.2*log(x)) ~ x, x.lim=range(0,4), main="Log-log equations")
plotFun(exp(2-1*log(x)) ~ x, add=T , col="red" )
plotFun(exp(2-0.8*log(x)) ~ x, add=T , col="green" )

plotFun(exp(3+1.2*x) ~ x, x.lim=range(0,3), main="Log-lin equations")
plotFun(exp(3-0.9*x) ~ x, add=T , col="red" )

plotFun(2+1.2*log(x) ~ x, x.lim=range(0,4), main="Lin-log equations")
plotFun(2-1*log(x) ~ x, add=T , col="red" )

#' Example 4.4 Lin-Log Food Expenditure Model
m5 <- lm(food_exp~log(income), data=food)      
coef(summary(m5))       
coef(m5)[2]/100 

plotModel(m5)    

plotSlopes(m1, plotx = "income", interval="confidence", col="green", opacity = 80)  
plotCurves(m5, plotx = "income", interval="confidence", col="red", opacity = 80, ylim=c(0,600))

food %>% ggplot(aes(x=income, y=food_exp)) + geom_point() +   
  stat_smooth(method=lm, formula = y ~ log(x), se=TRUE, col="red")

food %>% ggplot(aes(x=income, y=food_exp)) + geom_point() +             
  stat_smooth(method=lm, formula = y ~ log(x), se=TRUE, col="red") +
  stat_smooth(method=lm, se=TRUE, col="green") 

summary(m1)   
summary(m5)   

# diff in SSE    
anova(m1,m5)

# Slope
g <- makeFun(m5) # E(y|x)
dg <- function(x) {coef(m5)[2]/x}
# diminishing marginal effect
plotFun(dg(x) ~ x, col="red", xlim = c(0,30))   


#' E(food_exp|income=10) = household with 10, i.e., 10*100=1000$ income  
g(10)                 
#' Marginal spending at a 1000$ more in income
dg(10)              

#' E(food_exp|income=10) = household with 20, i.e., 20*100=2000$ income
g(20) 
#' marginal spending at a 100$ more in income     
dg(20)

#' Formula on p. 164 before Example 4.4., $\Delta y \cong \frac{\beta _{2}}{100}(\%\Delta x)$ 
percent.dg <- function(x) {x*coef(m5)[2]/100}
#' a 1% increase in income will increase food expenditure by `r percent.dg(1)` dollars
percent.dg(1) 

percent.dg(10) 

##########################
#' Residual plots     
#########################        
par(mfrow=c(2,2))
plot(m5)
par(mfrow=c(1,1))

#' Fig 4.8
plot(food$income, resid(m5))  # Hetersoskedaticity


#' Example 4.6 Testing normality
#' <http://www.rdocumentation.org/packages/DescTools/functions/JarqueBeraTest>
library(tseries)
#' H0: Residuals are Normal
jarque.bera.test(resid(m1))

moments::jarque.test(resid(m1))

#' critical value    
qchisq(0.95,2)
#Keep null hypothesis

#' Global test of model assumptions
library(gvlma)                         
m1 <- lm(food_exp~income, data=food)
gvmodel <- gvlma(m1)  
summary(gvmodel)           


#' Influential observations     
mplot(m1)

# p. 170
car::dfbetaPlots(m1) 

dfbeta(m1)
dffits(m1)

library(broom)
augment(m1)  


###################################
#' Polynomial models  
####################################

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/wa_wheat.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"))

# New name on data, delete old dataframe
wheat <- wa_wheat  
rm(wa_wheat) 


#' Yield over time - linear model          
m1 <- lm(greenough~time, data=wheat)      
plotModel(m1) + ggtitle("Regression model for Greenough")  


# ----------------------------------------------------------
# Slight departure
#' Dealing with 4-groups at the same time
library(tidyr)

long <- wheat %>% pivot_longer(-time, names_to = "location", values_to = "yield")
head(long)

long %>% ggplot(aes(x=time, y=yield)) + geom_point() +
  facet_wrap(~location)

#' 4 OLS models by group, broom package
long %>% group_by(location) %>% do(tidy(lm(yield ~ time, data=.)))

#' Digression to Chapter 7, four regression models in one plot
long %>% ggplot(aes(x=time, y=yield)) + geom_point()  + 
  aes(colour=location)  + stat_smooth(method=lm, se=F) + 
  theme(legend.position="bottom") + labs(title="Yield over time per location")   

mod <- lm(yield ~ time + factor(location) + time:factor(location), data = long)
summary(mod)
plotModel(mod)

# m2 <- lm(yield ~ time * factor(location), data = long)
# summary(m2)
# f2 <- makeFun(m2)
# xyplot(yield ~ time, data = long)
# plotFun(f2(time, location = "northampton") ~ time, add = TRUE)
# plotFun(f2(time, location = "chapman") ~ time, add = TRUE, col="red")
# plotFun(f2(time, location = "mullewa") ~ time, add = TRUE, col="green")
# plotFun(f2(time, location = "greenough") ~ time, add = TRUE, col="black")

# ----------------------------------------------------------

#' Back to textbook again

#' Example 4.8
summary(m1)
plot(wheat$time, resid(m1), col="blue")

m3 <- lm(greenough~I(time^3), data=wheat)
summary(m3)
points(wheat$time, resid(m3), col="green", pch=19)

#' Digression to multiple regression, p 173
m4 <- lm(greenough~time+I(time^2)+I(time^3), data=wheat)
summary(m4)
plotCurves(m4, plotx = "time")#, ylim=c(0,600))
plotModel(m4)

#' A growth model, log-linear model, Example 4.9
m5 <- lm(log(greenough)~time, data=wheat)
summary(m5)

plotCurves(m5, plotx = "time")

g <- makeFun(m5)
#' E(log(yield)|time=20)
g(20) 

#plotModel(fit) does not work with transformed y models
xyplot(greenough~time, data=wheat)
plotFun(g(time) ~ time, col="red", add=TRUE)

#' Manually
h <- function(x) {exp(coef(m5)[1]+coef(m5)[2]*x)}

plot(wheat$time,wheat$greenough, col="aquamarine4", pch=19)
curve(h, 0,50, lwd=2, col="darkgreen", main="Log-Linear Relationship",
      xlab="Time", ylab="Yield", add = TRUE)

wheat %>% ggplot(aes(x=time, y=greenough)) + geom_point() +
  stat_function(fun=h) + xlim(c(0,50))


#' Example 4.10 - Wage Equation, log-linear model 
rm(list=ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))
head(cps5_small)

cps5 <- cps5_small
rm(cps5_small)

summary(cps5$wage)

#' Estimated model
m1 <- lm(log(wage)~educ, data=cps5)
summary(m1)

#' Sums of squared residuals, SSE
deviance(m1)

#' The variance in the model, sigma squared 
deviance(m1)/m1$df.residual
s2 <- deviance(m1)/m1$df.residual

#' Predictions in the log-linear model
yn <- function(x) {exp(coef(m1)[1]+coef(m1)[2]*x)} # not corrected
yc <- function(x) {exp(coef(m1)[1]+coef(m1)[2]*x+s2/2)} # corrected

plot(cps5$educ,cps5$wage, main="Not Correcated and Corrected predictions from Log-Linear Model")
curve(yn, 0,40, lwd=3, add=T, col="blue")
curve(yc, 0,40, lwd=3, col="red", add=T)
legend("topleft", c("standard prediction","corrected prediction"),
       col=c("blue","red"), inset = 0.05, lty=1, cex=1)

#' A generalized $R^{2}$ measure
cor(cps5$wage,yc(cps5$educ))
cor(cps5$wage,yc(cps5$educ))^2
summary(m1)$r.squared

#' Prediction intervals in a log-linear model
f <- makeFun(m1)
#' Expected value in levels
f(12, interval='prediction') 

#' The mosaic::makeFun function predict the dependent variable in levels but w/o se correction!
f(12)
yn(12)
yc(12)
f(12)*exp(s2/2) # must adjust

#' Fig 4.14, adjusted prediction interval, note in our textbook it is unadjusted
predint <- f(0:22, interval='prediction')*exp(s2/2)
# class(predint)
# str(predint)
plot(cps5$educ,cps5$wage, main="Figure 4.14 The 95% prediction interval for wage")
lines(0:22,predint[,2], lty=2) # lower
lines(0:22,predint[,3], lty=2) # upper
lines(0:22,predint[,1], col="blue", lwd=2) # fit


#' Log-Log model, Example 4.13
rm(list=ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/newbroiler.rdata"))
head(newbroiler)

#' Estimate model
m1 <- lm(log(q)~log(p), data=newbroiler)
summary(m1)

#' The variance in the model, sigma squared 
s2 <- deviance(m1)/m1$df.residual ; s2

#' Corrected predicted value in levels
f <- makeFun(m1)
yc <- function(x) {exp(coef(m1)[1]+coef(m1)[2]*log(x)+s2/2)}
ycA <- function(x) {f(x)*exp(s2/2)} # alternative
#ycA <- function(x) {exp(coef(m1)[1]+coef(m1)[2]*log(x))*exp(s2/2)} # alternative

#' A log-log model is a non-linear line in lin-lin world
xyplot(q~p, data=newbroiler, main="Poultry demand Log-Log Model")
plotFun(f(p) ~ p, add=TRUE)
plotFun(yc(p) ~ p, add=T, col="red", lwd=2)
plotFun(ycA(p) ~ p, add=T, col="green", lwd=2) # same as above

#' standard plot
newbroiler %>% ggplot(aes(x=p, y=q)) + geom_point() +
  stat_function(fun=yc)

#' the economist's view
newbroiler %>% ggplot(aes(x=p, y=q)) + geom_point() +
  stat_function(fun=yc) + xlim(c(0.5,3)) + coord_flip()    

#' note slight difference
f(1)
yc(1)
ycA(1)

#' A log-log model is a straight line in log-log world
plot(log(newbroiler$p),log(newbroiler$q), main="Poultry demand Log-Log Model")
abline(m1)

newbroiler %>% ggplot(aes(x=log(p), y=log(q))) + geom_point() +
  geom_smooth(method = lm, se = FALSE, color="red") +
  ggtitle("Poultry demand Log-Log Model") + coord_flip()
