#Figure 2.9 code

set.seed(1000)
x <- 2*cos(2*pi*1:500/50+0.6*pi)+rnorm(500,0,5)
z1 <- cos(2*pi*1:500/50); z2 <- sin(2*pi*1:500/50)
summary(fit <- lm(x~0+z1+z2))
plot.ts(x,lty="dashed")
lines(fitted(fit),lwd=2)
