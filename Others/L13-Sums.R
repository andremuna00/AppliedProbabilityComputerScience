## Seed the current RNG, i.e., set the RNG status
set.seed(45)

## Sum of standard normal random variables
# Non-efficient version
N<-1000
S<-rep(0,N)
Z<-rnorm(N)
for (n in 2:N){
  S[n]<-S[n-1]+Z[n] # n-th partial sum
}

head(S)
# More efficiently: 
# Z[1] <- 0
# S <- cumsum(Z)

# Three line plots to study the behaviors of the partial sums
plot(S[1:N], type = 'l', xlim = c(0,N), ylim = max(abs(S))*c(-1,1),xlab = "n", ylab = "S(n)")
plot(S[1:N]/(1:N), type = 'l', xlim = c(0,N), ylim = c(-2,2),xlab = "n", ylab = "S(n)/n")
plot(S[1:N]/sqrt(1:N), type = 'l', xlim = c(0,N), ylim = c(-2,2),xlab = "n", ylab = "S(n)/sqrt(n)")

# We can set a different seed: you can see this as choosing a "little omega" from the sample space that generate the full sequence of standard normal random variables (and therefore the sequence of sums)
set.seed(85)
Z<-rnorm(N)
Z[1] <- 0
S2 <- cumsum(Z)

# Three line plots to study the behaviors of the two sequences of partial sums
# S(n) in black and S2(n) in red
plot(S[1:N], type = 'l', xlim = c(0,N), ylim = max(abs(c(S,S2)))*c(-1,1),xlab = "n", ylab = "S(n)")
lines(S2[1:N], col=2)
# S(n)/n
plot(S[1:N]/(0:(N-1)), type = 'l', xlim = c(0,N), ylim = c(-2,2),xlab = "n", ylab = "S(n)/n")
lines(S2[1:N]/(1:N), col=2)

plot(S[1:N]/sqrt(0:(N-1)), type = 'l', xlim = c(0,N), ylim = c(-4,4),xlab = "n", ylab = "S(n)/sqrt(n)")
lines(S2[1:N]/sqrt(1:N), col=2)

# You may need to install the package first:
# install.packages("animation")
library(animation)

# Before saving, you may want to set your working directory to a known location
# setwd("The/Directory/Where/Files/Are/Saved")

des = c("This is a simulated example to show how partial sums of random variables behave\n\n", "S(n) is the sum of n independent standard normal random variables")
saveHTML({
  ani.options(interval = 0.05)
  idx = seq(1,N,10)
for(n in idx){
  plot(S[1:n], type = 'l', xlim = c(0,N), ylim = c(-115,115),xlab = "n", ylab = "S(n)")
  lines(S2[1:n], col=2)
}
}, img.name = "L13-Sums_plot", imgdir = "L13-Sums_dir", htmlfile = "L13-sums.html", 
autobrowse = TRUE, autoplay=FALSE, navigator = FALSE,  title = "Demo of Partial Sums Behavior", 
description = des)
dev.off()

# library(ggplot2)
# library(gganimate)

