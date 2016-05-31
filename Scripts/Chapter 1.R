#Time series with Applications Chapter 1
library(astsa)

#Problems 1

data("EQ5")
data("EXP6")

plot(EQ5,col=2)
lines(EXP6,col=1)

#On the differences in this plot. You can see that the lower signals points
# and the upper middle range points have similar frequencies. The Erathquake 
# is louder at the end. The explosion is louder for a small while than soft again

#Problems 2

#x_t = s_t + _t. Simulate and plot n=200 of each of the following models

t <- 1:200

#a) s_t = 0 t ~ [1,100], 10*exp(-(t-100)/20)*cos(4pi*t/4) t ~ [101,200]

s <- ifelse(t>=100,10*exp(-(t-100)/20)*cos(2*pi*t/4),0)
x <- ts(s+rnorm(200,0,1))
plot(x)

#b) s_t = 0 t ~ [1,100], 10*exp(-(t-100)/200)*cos(4pi*t/4) t ~ [101,200]

s2 <- ifelse(t>=100,10*exp(-(t-100)/200)*cos(2*pi*t/4),0)
x2 <- ts(s2+rnorm(200,0,1))
plot(x2)

#c) i) Compare a) with earthquake and b) with explosion.

plot(x)
lines(EQ5,col=2)

plot(x2)
lines(EXP6,col=2)

#ii) Compare exo(-t/20) with exp(-t/200)
plot(exp(-(1:100)/20))
lines(exp(-(1:100)/200))

#Problem 1.3

#a)generate x_t = -0.9x_t-2+w_t

w <- rnorm(150,0,1)  #50 extra add the start 
x <- filter(w,c(0,-0.9),method="recursive")[-(1:50)] #filter values ignore first 50
v <- filter(x,sides=1,rep(1/4,4))

plot.ts(x)
lines(v,lty=2,col=2)

#b) x_t = cos(2pi*t/4)

x2 <- cos(2*pi*(1:100)/4)
plot.ts(x2)

#c) x_t = cos(2pi*t/4) + w_t

w <- rnorm(150,0,1)  #50 extra add the start 
x3 <- cos(2*pi*(1:100)/4)+w[-(1:50)]
plot.ts(x3)


#d) Compare a) - c)

plot.ts(x3)
lines(x,lty=2,col=2)

#Problem 1.20

n <- 500

w <- rnorm(n+50,0,1)  #50 extra add the start 
acf(w[51:550],lag.max = 20,type="correlation",plot=T)

n2 <- 50

w2 <- rnorm(n2+50,0,1)  #50 extra add the start 
acf(w2[51:100],lag.max = 20,type="correlation",plot=T)

#Problem 1.21

n <- 500

w <- rnorm(n+50,0,1)  #50 extra add the start 
v <- filter(w[51:550],sides=2,rep(1/3,3))
v <- v[!is.na(v)]

acf(v,lag.max = 20,type="correlation",plot=T)

n2 <- 50

w2 <- rnorm(n2+50,0,1)  #50 extra add the start 
v2 <- filter(w2[51:100],sides=2,rep(1/3,3))
v2 <- v2[!is.na(v2)]

acf(w2[51:100],lag.max = 20,type="correlation",plot=T)

#Ex1.22
t <- 1:200

#a) s_t = 0 t ~ [1,100], 10*exp(-(t-100)/20)*cos(4pi*t/4) t ~ [101,200]

s <- ifelse(t>=100,10*exp(-(t-100)/20)*cos(2*pi*t/4),0)
x <- ts(s+rnorm(200,0,1))

acf(x,lag.max=20,"covariance")
acf(x,lag.max=20,"correlation")

#Ex 1.23
t <- 1:500

#a) s_t = 0 t ~ [1,100], 10*exp(-(t-100)/20)*cos(4pi*t/4) t ~ [101,200]

x <- 2*cos(2*pi*t/50+0.6*pi)+rnorm(length(t),0,1) 
acf(x,lag.max=100,"correlation")

#Ex 1.24
x <- rbinom(500,2,0.5)
y <- 5 + x[2:length(x)] -0.7*x[1:(length(x)-1)]
acf(y,lag.max=10,"correlation",plot = F)
