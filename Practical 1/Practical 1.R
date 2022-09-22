
# Exercise 1
# change so it is vectorised, as this takes less time than apply 
P_function <- function(w){
  P <- c()
  for (i in 1:length(w)){
    if (w[i] < 3.5 | w[i] > 25) {
      P <- c(P, 0)
    } else if (w[i]>=3.5 & w[i]<=14){
      P <- c(P, exp(-2.13974*exp(-0.633*w[i])))
    } else if (w[i]>14 & w[i]<=25){
      P <- c(P, 1)
    }
  }
  return(P)
}

# Exercise 2
dat <- read.csv("WindData.csv")$speed
# simulate values and plot histogram on probability density scale 
x <- rweibull(10000, shape=1.679, scale=10.128)
hist(x, prob = TRUE, xlab='Hourly wind speeds (knots)', ylab='Probability Density')
gr <- seq(0, 40, 0.01)
# compute exact PDF on grid and plot 
lines(gr, dweibull(gr, shape=1.679, scale=10.128), col = "steelblue", lwd = 1.5)

# Exercise 3a
# attempt 4
sample_generator <- function(n, k=1.679, lambda=10.128) {
  sample <- NULL 
  while (length(sample) < n) {
    x <- runif(1, -20, 20)
    y <- runif(1, 0, 0.06)
    f <- dweibull(x, shape=k, scale=lambda)
    if (y <= f) sample <- c(sample, x)
  }
  return(sample)
}

# Exercise 3b
wind_samples <- sample_generator(1000, 1.679, 10.128)
power_of_samples <- P_function(wind_samples)
print("The mean time is:")
mean(power)


# Exercise 4
monte_carlo <- function(m,n,k,lambda){
  mean_power <- c()
  for (i in 1:m){
    wind_samples <- sample_generator(n, k=1.679, lambda=10.128)
    power_of_samples <- sapply(wind_samples, P_function)
    mean_sample <- mean(power_of_samples)
    mean_power <- c(mean_power, mean_sample)
  }
  #return(mean_power)
  return(sd(mean_power))
}


# Exercise 5a
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


# Exercise 6
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

# Exercise 7
n = 100
y <- c()
x <- c()
for (i in seq(1, n, 5)){
  error <- monte_carlo(m=100, n=i)
  y <- c(y, error)
  x <- c(x, i)
}
plot(x,y, type='b', ylab='Monte Carlo Error', xlab='Number of samples')
abline(h=0.05, lty='dotted', col='red')


# Exercise 8
#What is the total daily average power that could be produced at the site?

#What percentage of the time is the wind speed too low to produce power?
  
#What percentage of the time does the wind speed produce maximum power?













