#' chapter 5
rm(list=ls())

require(pacman)
p_load(rockchalk, mosaic)

#' A sample of hamburger franchises in 75 cities from Big Andy's Burger Barn.
#' Data definition file: 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/andy.def")

#' sales = S    Monthly sales revenue ($1000s)
#' price = P    A price index ($) for all products sold in a given month.
#' advert = A   Expenditure on advertising ($1000s)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))

summary(andy)

#' Multiple regression model, Example 5.2
m1 <- lm(sales ~ price + advert, data = andy)
summary(m1)


#' Multicollinearity, violates MR5
lin.advert.price <- 2*andy$advert+3*andy$price # perfect linear combination of advertising and price
summary(lm(sales~price+advert+lin.advert.price, data=andy)) # detected and removed as NA on variable

lin.advert.price.random <- 2*andy$advert+3*andy$price+rnorm(75,0,0.1) # add some random noise, close to 0
summary(lm(sales~price+advert+lin.advert.price.random, data=andy)) # notice large SE and insignificant t-values

cor(andy$advert,lin.advert.price.random) # might be a serious problem when cor > 0.7/0.8
cor(andy$price,lin.advert.price.random)


# prediction
f <- makeFun(m1)
f(price=5.5, advert=1.2)
f(5.5, 1.2)
f(price=5.5, advert=1.2, interval='prediction') # 95% Prediction Interval
f(price=5.5, advert=1.2, interval='prediction', level=0.9) 

plotSlopes(m1, plotx = "price", interval="prediction", col="red", opacity = 80)
plotSlopes(m1, plotx = "advert", interval="prediction", col="red", opacity = 80)

#' A confidence interval reports the interval estimate for the mean value of y for a given value of x, e.g.,
f(5.5,1.2, interval='confidence') 
plotSlopes(m1, plotx = "price", main="Linear Model", interval='confidence')
plotSlopes(m1, plotx = "price", modx = "advert")
plotSlopes(m1, plotx = "price", modx = "advert", modxVals = "std.dev.")
plotSlopes(m1, plotx = "price", modx = "advert", modxVals = c(0, 0.5, 1.8, 3.1))

plotPlane(m1, plotx1 = "price", plotx2 = "advert")

#' Example 5.3 Estimation of error variance (sigma squared)
length(andy$sales)
df.residual(m1) # N-K=df, where K is the number of parameters estimated (including the intercept)

deviance(m1) # SSE
deviance(m1)/df.residual(m1) # sigma squared
sqrt(deviance(m1)/df.residual(m1)) # standard error of the regression

#' Example 5.4, R-square
summary(m1)

#' Frish-Waugh-Lowell Theorem, partialling out (keeping constant) the effect of all other variables

# 1 sales squiggle, regressing sales on price, save residuals
u1 <- resid(lm(sales~price, data=andy))
# 2 advert squiggle, regressing advert on price, save residuals
u2 <- resid(lm(advert~price, data=andy))
# 3 regress the advert squiggle on sales squiggle, with no intercept
lm(u1~u2) # with intercept
lm(u1~0+u2) # without intercept
lm(sales~price+advert, data=andy) # note the same advert coefficient

plot(u2,u1)
abline(lm(u1~0+u2))

# Example 5.5 Variance, Covariance and se's
vcov(m1) # variance covariance matrix
diag(vcov(m1)) # variance
sqrt(diag(vcov(m1))) # se
coef(m1) # parameter estimates
coef(m1)/sqrt(diag(vcov(m1))) # t-values

#' Example 5.6 Interval Estimation
confint(m1) # confidence interval on the coeficients
round(confint(m1),3)
confint(m1, level = 0.9)

mplot(m1)

#-----------------------------------------------------------------------------
#' Interval estimation for a linear/non-linear combination/transformation of coefficients
p_load(car)

#' Using The Delta method (Appendix 5B)
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/mc20.rdata"))
names(mc20)

dm <- lm(y ~ x, data = mc20) # p. 248 & 250
summary(dm)

# Example 5.19
deltaMethod(dm, "exp(x/10)") # g1(b2) and se[g1(b2)]
deltaMethod(dm, "exp(b2/10)", parameterNames= paste("b", 1:2, sep="")) 

# manually, wrong in textbook p.249, due to to asymptotics, a non-linear hypothesis with q=1 df is N(0,1) distributed
# this follows from the Chebychev Inequality
#deltaMethod(dm, "exp(x/10)")$Estimate-qt(c(0.975), df=df.residual(dm))*deltaMethod(dm, "exp(x/10)")$SE # LB
#deltaMethod(dm, "exp(x/10)")$Estimate+qt(c(0.975), df=df.residual(dm))*deltaMethod(dm, "exp(x/10)")$SE # UB
deltaMethod(dm, "exp(x/10)")$Estimate-qnorm(0.975)*deltaMethod(dm, "exp(x/10)")$SE # LB
deltaMethod(dm, "exp(x/10)")$Estimate+qnorm(0.975)*deltaMethod(dm, "exp(x/10)")$SE # LB

# ----------------------------------------------------------------------------

#' Example 5.7 Interval estimate for change in sales
#' Increase advertising by $800 and drop the price by $0.4
deltaMethod(m1, "-0.4*price+0.8*advert", level=0.9) # Linear combination of two parameters, p. 184

# 90% Confidence interval - manually
qt(c(0.95), df=df.residual(m1)) # critical t-value
lamda <- deltaMethod(m1, "-0.4*price+0.8*advert")
tc <- qt(c(0.95), df=df.residual(m1))
LB <- lamda$Estimate - tc*lamda$SE
UB <- lamda$Estimate + tc*lamda$SE
1000*round(c(LB,lamda$Estimate,UB),3)

# http://www.rdocumentation.org/packages/multcomp
p_load(multcomp)

# linfct specifies the required combination In this case we want -0.4*b2+0.8*b3=0
summary(glht(m1, linfct = c("-0.4*price+0.8*advert = 0")))
confint(glht(m1, linfct = c("-0.4*price+0.8*advert = 0")), level = 0.9) # p. 184

#' EVEN MORE MANUALLY
#' Get a standard error for Lambda - which is just
#' a linear combination of b2 and b3 (b1-intercept is zero)
D <- c(0, -0.4, 0.8)
lambda.se <- sqrt(t(D) %*% vcov(m1) %*% D)
b <-coef(m1)
b
lambda <- -0.4*b[2]+0.8*b[3]
lambda
lambda-qt(0.95,72)*lambda.se
lambda+qt(0.95,72)*lambda.se


# Example 5.8 & 5.9, two-tail test
summary(m1)

# Is the demand elastic? p.220 Example 5.10
summary(m1) # Two tail test
summary(glht(m1, linfct = c("price >= 0"))) # multcomp::glht, one sided test, specify the H0 using linfct=

andy <- andy %>% mutate(quantity=sales/price)
mean(~price, data=andy)
mean(~sales, data=andy)
mean(~quantity, data=andy)

# sales elasticity
coef(m1)[2]*mean(~price, data=andy)/mean(~sales, data=andy) # (dS/dp)*(p/S)

# own price elasticity derived from sales elasticity
(coef(m1)[2]/mean(~quantity, data=andy))-1 # dq/dp*p/q=(dS/dp)*(1/q)-1 , see: browseURL("http://rpubs.com/omy000/107325")
mq <- mean(~quantity, data=andy)
deltaMethod(m1, "(price/mq)-1") # se for the elasticity

# or
coef(m1)[2]*mean(~price, data=andy)/mean(~sales, data=andy)-1 # OR: dq/dp*p/q=[(dS/dp)*(p/S)]-1

# ----------------------------------------------------------
#' Alternative model
m2 <- lm(quantity~price+advert, data=andy)
summary(m2)
coef(m2)[2]*mean(~price, data=andy)/mean(~quantity, data=andy) # OR: dq/dp*p/q
# ----------------------------------------------------------

# some plots
andy %>% ggplot(aes(x=price, y=sales)) + geom_point()
andy %>% ggplot(aes(x=price, y=quantity)) + geom_point()
andy %>% ggplot(aes(x=price, y=quantity)) + geom_point() + coord_flip()


# Own-price-elasticity function from model 1
favstats(~quantity, data = andy)
e1 <- function(quantity) {(coef(m1)[2]*1/quantity)-1} 
curve(e1, 9, 18, main="Own-Price Elasticity", xlab="quantity")

# Own-price-elasticity function from alternative model
f <- makeFun(m2)
mean(~advert, data=andy)
f(price = 5:7, advert=mean(~advert, data=andy))

# keeping advert at mean
e2 <- function(price) {(coef(m2)[2]*price/f(price = price, advert=mean(~advert, data=andy)))} 
curve(e2, 5,7, main="Own-Price Elasticity", xlab="price")

# -----------------------------------------------------------

# 5.11 Testing advertising Effectiveness
summary(m1)
summary(glht(m1, linfct = c("advert = 1"))) # Two sided test
# can do the one sided directly
summary(glht(m1, linfct = c("advert <= 1")))

#' 5.12  Linear combination
#' Dropping price by $0.2 is more effective than increase advertising by $500
#' or 0.2 b2 > 0.5 b3
#' as two tail test
summary(glht(m1, linfct = c("-0.2*price-0.5*advert = 0")))
# one tail
summary(glht(m1, linfct = c("-0.2*price-0.5*advert <= 0")))

#' Polynomial equations or Nonlinear Relationships
#' Optimal Advertising, Example 5.17
andy <- andy %>% mutate(advert2 =advert^2)
m3 <- lm(sales~price+advert+advert2, data=andy)
summary(m3)

#' Optimal level of advertising, Example 5.17
deltaMethod(m3, "(1-advert)/(2*advert2)") # Non-linear combination of two parameters, p. 193-195
# manually
lamda <- deltaMethod(m3, "(1-advert)/(2*advert2)")
tc <- qt(c(0.975), df=df.residual(m2))
LB <- lamda$Estimate - tc*lamda$SE
UB <- lamda$Estimate + tc*lamda$SE
c(LB,lamda$Estimate,UB) # optimal level 95% CI

#' Same as above, but without creating new poynomial
m4 <- lm(sales~price+advert+I(advert^2), data=andy)
summary(m4)

b <- coef(m4)
b

a <- deltaMethod(m4, "(1-b3)/(2*b4)", parameterNames= paste("b", 1:4, sep=""))
a

#' Optimal Advertising
plotCurves(m4, plotx = "advert", interval = "conf", col="green", opacity = 80 )
abline(v=a$Estimate)
abline(v=a$`2.5 %`, col="red")
abline(v=a$`97.5 %`, col="blue")


#' Interaction, Example 5.15
rm(list=ls()) # start with clean sheets

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))
names(cps5_small)

m1 <- lm(wage~educ+exper+I(educ*exper), data=cps5_small)
summary(m1)

#' Interaction
#' http://faculty.chicagobooth.edu/richard.hahn/teaching/FormulaNotation.pdf
m2 <- lm(wage~educ*exper, data=cps5_small)
summary(m2)

favstats(~exper, data = cps5_small)
plotSlopes(m2, plotx = "educ", modx = "exper")
plotSlopes(m2, plotx = "educ", modx = "exper", modxVals = c(12, 62))

favstats(~educ, data = cps5_small)
plotSlopes(m2, plotx = "exper", modx = "educ")
plotSlopes(m2, plotx = "exper", modx = "educ", modxVals = c(12, 21))


#' Log-Quadratic Wage Equation, Example 5.16
m3 <- lm(log(wage)~educ+exper+I(educ*exper)+I(exper^2), data=cps5_small)
coef(m3)

dyexper <- function(educ, exper) {coef(m3)[3]+coef(m3)[4]*educ+2*coef(m3)[5]*exper}

100*dyexper(educ = c(8,16), exper = 0)
100*dyexper(educ = c(8,16), exper = 20)

dyeduc <- function(educ, exper) {coef(m3)[2]+coef(m3)[4]*exper}

100*dyeduc(educ = c(8,16), exper = 0)
100*dyeduc(educ = c(8,16), exper = 20)

#' Example 5.18, How much experience maximizes wages
m3

deltaMethod(m3, "(b3+16*b4)/(-2*b5)", parameterNames= paste("b", 1:5, sep=""))
deltaMethod(m3, "(-b3-16*b4)/(2*b5)", parameterNames= paste("b", 1:5, sep=""))

# -----------------------------------------------------------
# One step further: Predicting wage at optimal exper, gived educ=16

g <- function(exper, educ) {coef(m3)[1]+coef(m3)[2]*educ+coef(m3)[3]*exper+coef(m3)[4]*educ*exper+coef(m3)[5]*exper^2}
g(10, educ = 16)
curve(g(x, educ = 16), 0, 40)

g1 <- function(exper) {coef(m3)[1]+coef(m3)[2]*16+coef(m3)[3]*exper+coef(m3)[4]*16*exper+coef(m3)[5]*exper^2}
optimize(g1, c(0, 40), tol = 0.000001, maximum = TRUE)

deltaMethod(m3, "exp(b1+b2*16+b3*30.17+b4*16*30.17+b5*30.17^2)", parameterNames= paste("b", 1:5, sep=""))

# Much easier!
f <- makeFun(m3)
f(exper=30.17299, educ = 16)
f(exper=30.17299, educ = 16, interval="confidence")
#f(exper=30.17299, educ = 16, interval="prediction")

#' The variance in the model, sigma squared 
s2 <- deviance(m3)/m3$df.residual
f(exper=30.17299, educ = 16, interval="confidence")*exp(s2/2) # must adjus