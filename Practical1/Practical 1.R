
############################ Exercise 1 #######################################
# used preallocation to make it more efficient
P_function <- function(w){
  n = length(w)
  P <- rep(0, times=n)
  for (i in 1:n){
    if (w[i] < 3.5 | w[i] > 25) {
      P[i] <- 0
    } else if (w[i]<=14){
      P[i] <- exp(-2.13974*exp(-0.633*w[i]))
    } else if (w[i]<=25){
      P[i] <- 1
    } else {
      P[i] <- 0
    }
  }
  return(P)
}

# Plot
library(tidyverse)
w <- seq(0, 40, 0.01)
p <- P_function(w)
df1 <- data.frame(wind_speed = w, power = p)
plt1 <- df1 %>% ggplot(aes(x=wind_speed, y=power)) + geom_line() + xlab("Wind speed (knots)") + ylab("% of total kilowatts")
plt1
############################ Solution 1 #######################################
# Function
power <- function(w) {
  n <- length(w)
  p <- rep(0, n)
  for (i in 1:n) {
    if (w[i] < 3.5) {
      p[i] <- 0
    } else if (w[i] <= 14) {
      p[i] <- exp(-213.974 * exp(-0.633 * w[i]))
    } else if (w[i] < 25) {
      p[i] <- 1
    } else {
      p[i] <- 0
    }
  }
  return(p)
}

# Plot
w <- seq(0, 40, 0.01)
p <- power(w)
plot(w, p, type = "l", lwd = 1.5, xlab = "Wind speed (knots)",
     ylab = "% of total 
     kilowatts", main = "Wind turbine power output")

############################ Exercise 2 #######################################

dat <- read.csv("Practical1/WindData.csv")

# simulate values and plot histogram on probability density scale 
plt2 <- ggplot() + geom_histogram(dat, mapping = aes(x=speed, y = ..density..) ) + geom_line() +
  xlab('Hourly wind speeds (knots)') + ylab('Probability Density')
gr <- seq(0, 40, 0.01)
dw <- dweibull(gr, shape=1.679, scale=10.128)
df2 <- data.frame(grid = gr, dweibul = dw)
plt2 <- plt2 + geom_line(df2, mapping = aes(x=grid, y=dweibul))
plt2

############################ Solution 2 #######################################

k <- 1.679
lambda <- 10.128
dat1  <- read.csv("Practical1/WindData.csv")$speed
wgrid <- seq(0, 40, 0.01)
hist(dat1, col = "grey95", border = "grey75", prob = TRUE,
     xlab = "Hourly wind speed (knots)", ylab = "Probability density")
lines(wgrid, dweibull(wgrid, k, lambda), lwd = 1.5)


############################ Exercise 3 #######################################

# Part a
sample_generator <- function(n, k=1.679, lambda=10.128) {
  # determine bound of box to sample in
  mean <- lambda * gamma(1 + 1/k)
  sd <- lambda * sqrt(gamma(1 + 2/k) - gamma(1 + 1/k)^2)
  xrange <- mean + 5 * sd
  pdf <- dweibull(seq(0, xrange, length = 1000), k, lambda)
  yrange <- 1.1 * max(pdf)
  # create samples
  sample <- NULL
  while (length(sample) < n) {
    x <- runif(1, 0, xrange)
    y <- runif(1, 0, yrange)
    f <- dweibull(x, k, lambda)
    if (y <= f)
      sample <- c(sample, x)
  }
  return(sample)
}

# Part b
wind_samples <- sample_generator(1000, 1.679, 10.128)
power_of_samples <- P_function(wind_samples)
print("The mean time is:")
mean(power_of_samples)

############################ Solution 3 #######################################

# Function
rejectSample <- function(n, k, lambda) {
  # determine bound of box to sample in
  mean <- lambda * gamma(1 + 1/k)
  print(mean)
  sd <- lambda * sqrt(gamma(1 + 2/k) - gamma(1 + 1/k)^2)
  xrange <- mean + 5 * sd
  pdf <- dweibull(seq(0, xrange, length = 1000), k, lambda)
  yrange <- 1.1 * max(pdf)
  # create samples
  sample <- NULL
  while (length(sample) < n) {
    x <- runif(1, 0, xrange)
    y <- runif(1, 0, yrange)
    f <- dweibull(x, k, lambda)
    if (y <= f)
      sample <- c(sample, x)
  }
  return(sample)
}

# Compute mean hourly power
samps <- rejectSample(500, k, lambda)
powerSamps <- power(samps)
mean(powerSamps)


############################ Exercise 4 #######################################

monte_carlo <- function(m,n,k=1.679,lambda=10.128,f){
  mean_power <- rep(0, m)
  for (i in 1:m){
    wind_samples <- f(n, k, lambda)
    mean_power[i] <- mean(P_function(wind_samples))
  }
  #return(mean_power)
  return(sd(mean_power))
}
monte_carlo(500, 100, k, lambda, sample_generator)

############################ Solution 4 #######################################

estPowerSD <- function(n, m, k, lambda, f) {
  means <- rep(0, m)
  for (i in 1:m) {
    samp <- f(n, k, lambda)
    means[i] <- mean(power(samp))
  }
  return(sd(means))
}
estPowerSD(500, 100, k, lambda, rejectSample)

############################ Exercise 5 #######################################

#In this method, you pick a random number of the y-axis (between  0   and  1),
# then use the inverse cumulative distribution function to figure out the
# corresponding value on the  x-axis. You take this value as a sample.
#Notice, there is no rejection here, every iteration leads to a sample.

# 1. Use uniform distribution to generate n random samples between 0 and 1
# 2. Use qweibull to find the corresponding values on x-axis

# Exercise 5b
exercise5 <- function(n, k=1.679, lambda=10.128){
  ys <- runif(n)
  xs <- qweibull(ys, shape=k, scale=lambda)
  return(xs)
}

# Exercise 5c
#Using exercise 5 function to calculate mean power at this site
x_samples <- exercise5(1000)
power <- P_function(x_samples)
mean(power)

############################ Solution 5 #######################################

# Function
inverseCDFSample <- function(n, k, lambda) {
  y <- runif(n)
  x <- qweibull(y, k, lambda)
  return(x)
}

# Compute mean hourly power
samps <- inverseCDFSample(1000, k, lambda)
powerSamps <- power(samps)
mean(powerSamps)

############################ Exercise 6 #######################################

rejection_sample_time <- system.time(sample_generator(10000))
#    user  system elapsed 
# 0.626   0.322   1.035 
rejection_sample_mc <- monte_carlo(10, 10000)
# 0.003650684

cdf_method_time <- system.time(exercise5(10000))
#user  system elapsed 
#0.005   0.000   0.005
cdf_method_mc <- monte_carlo(10, 10000)
# 0.003229122

# The inverse cdf sampler should be used. The monte carlo estimates are very similar (with cdf being lower)
# but the time taken is significantly lower for this method. Therefore it is better to use.

############################ Solution 6 #######################################

system.time(rejectSample(10000, k, lambda))
system.time(inverseCDFSample(10000, k, lambda))
estPowerSD(10, 1000, k, lambda, inverseCDFSample)
estPowerSD(10, 1000, k, lambda, rejectSample)

############################ Exercise 7 #######################################
n = seq(5, 100, 5)
y = rep(0, length(n))
for (i in 1:length(n)){
  y[i] <- monte_carlo(n= n[i], m = 1000, f=monte_carlo)
}
plot(n,y, type='b', ylab='Monte Carlo Error', xlab='Number of samples')
abline(h=0.05, lty='dotted', col='red')

############################ Solution 7 #######################################

nsamp <- seq(5, 100, 5)
err <- rep(0, length(nsamp))
for (i in 1:length(nsamp)) {
  err[i] <- estPowerSD(nsamp[i], 1000, k, lambda, inverseCDFSample)
}
plot(nsamp, err, type = "b", xlab = "Number of samples",
     ylab = "Monte Carlo error")
abline(h = 0.05, col = "red", lty = "dotted")

############################ Exercise 8 #######################################
#What is the total daily average power that could be produced at the site?

#What percentage of the time is the wind speed too low to produce power?
  
#What percentage of the time does the wind speed produce maximum power?

############################ Solution 8 #######################################
# Example: how often does wind speed exceed 25 knots?
samples <- inverseCDFSample(1e+05, k, lambda)
unsafe <- samples > 25
percentageUnsafe <- 100 * mean(unsafe)

# Question 1: daily average power?
samples <- inverseCDFSample(24 * 1000, k, lambda)
psamples <- power(samples)
pmat <- matrix(psamples, nr = 24)
dailyp <- colSums(pmat)
mean(dailyp)
quantile(dailyp, prob = c(0.025, 0.975))  # range of plausible values 

# Question 2: percentage time wind speed too low (i.e.
# < 3.5)?
samples <- inverseCDFSample(1000, k, lambda)
100 * sum(samples < 3.5)/length(samples)

# Question 3: percentage time wind speed produces
# maximum power?
samples <- inverseCDFSample(1000, k, lambda)
100 * sum(samples > 14 & samples <= 25)/length(samples)
# True answer: 100 * (pweibull(25, k, lambda) -
# pweibull(14, k, lambda))












