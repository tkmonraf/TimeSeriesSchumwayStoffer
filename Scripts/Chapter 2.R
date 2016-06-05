#Time series and its applications Chapter 2

library(astsa)
library(xts)
library(ggplot2)

#Problem 2.1 Johnson and Johnson Data
# Let x_t = log(y_t)

#a) Fit the regression:
# x_t = beta*t + alpha_i*Q_i(t)+w_t i=1,...,4

trend <- time(jj)-1970
Q <- factor(rep(1:4,21))
reg <- lm(log(jj)~0+trend+Q,na.action = NULL) 
model.matrix(reg)
summary(reg)

#b) What happens when you include an intercept

reg_int <- lm(log(jj)~trend+Q,na.action = NULL) 
model.matrix(reg_int)
summary(reg_int)
plot(reg_int)

#c) Plot reg
plot(reg)

#Looks like white noise somewhat

#Q2Mt = B1+B2t +B3(Tt-E(T))+B4(Tt-E(T))^2+B5Pt+w

plot(cmort)
plot(tempr)
plot(part)

Temp <- tempr-mean(tempr)
Temp2 <- Temp^2
t <- time(cmort)
t <- t - 1970
fitMort <- lm(cmort~t+Temp+Temp2+part,na.action = NULL)

summary(fitMort)
summary(aov(fitMort))


#a)Add P_t-4 to 2.25. State conclusions

P4 <- lag(part,4)

tmp <- end(part)-start(part)


part.xts <- xts(as.numeric(part),seq.Date(from = as.Date("1970-01-01"),by = "week",length.out = tmp[1]*52+tmp[2]+1))
P4 <- lag(part.xts,4)
P4 <- as.ts(P4)


fitMorta <- lm(cmort[5:508]~t[5:508]+Temp[5:508]+Temp2[5:508]+part[5:508]+P4[5:508],na.action = NULL)
summary(fitMorta)
predict.lm(fitMorta)

plot(fitMorta)


#b) Draw scatterplots of M_t,T_t,P_t,t-4 and calculate corrs. pairs function
#Compare rel between Mt and Pt vs Mt and Pt-4
pairs(cbind(cmort,t,Temp,part,P4))
cor(cbind(cmort,t,Temp,part))

cor(cbind(cmort,part))
cor(cbind(cmort[5:508],P4[5:508]))

plot(cbind(cmort[5:508],P4[5:508]))
plot(cbind(as.numeric(cmort),as.numeric(part)))

#Q3. 2.3 Repeat 6 times. Random walk with drift.
#length n=100, with delta = 0.01 ad sigma = 1.
#Call the data x for t=1...100. Fit x=Bt+w. Plot 
#the data, mean function, and fitted line. Discuss.

delta <- 0.01
sigma <- 1

t <- 1:100

reg <- list()

for(i in 1:6){
  x <- ts(cumsum(rnorm(100,delta,sigma)))
  reg <- lm(x~0+time(x),na.action = na.pass)
  plot(x)
  lines(0.01*time(x),col =2)
  abline(reg,col =3)
  }

#Q8: varve Glacial temperatures

#a) Argue x_t exhibits heteroscedasticity by comparing first half 
#sample var to second half sample var

n <- length(varve)
varve.first <- varve[1:round(n/2)]
varve.second <- varve[(round(n/2)+1):n]

sd(varve.first)-sd(varve.second)
#varve.second > varve.first therefore exhibits heteroscedasticity

#Does the log transform change this assumption

log.varve.first <- log(varve.first)
log.varve.second <- log(varve.second)

sd(log.varve.first)-sd(log.varve.second)

#Much less evidence of heteroscedasticity

#Compare histograms

qplot(log(varve),geom="histogram")
qplot(varve,geom="histogram")

#much more normal after log transform

#b) Plot y_t. Do any time intervals exhibit similar behaviour to Fig1.2
plot(log(varve))

#Behavior for first half of histogram is somewhat similar. 

#c)Examine the sample acf of y_t and comment

acf(log(varve))
#There is correaltion between the different lags of the series

#d) Examine differenced series of y, u_t = y_t-t_(t-1)?

u <- diff(log(varve))

#plot u
plot(u)

#Examine acf of u

acf(u)

#What is the practical interpretation of u_t? 
#THe percentage difference between the spots
