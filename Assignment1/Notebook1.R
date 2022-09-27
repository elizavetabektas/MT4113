#Write an R function, my_rnorm() that returns a vector of pseudo-random values
#from a normal distribution using the Box-Muller algorithm.
#The algorithm delivers values with mean 0 and sd 1.
#You can transform them into values with mean μ and sd σ
#by multiplying the values by σ and then adding μ.
#The algorithm describes how to generate pairs of normally distributed deviates;
#think about how your code should behave to produce an odd number of deviates.
#rnorm() accepts vector arguments, but your functions should be designed to accept
#only single numbers. This makes it easier for you to code.
#Just like the R function rnorm(), the function should have the following arguments:

################################### rnorm #####################################

my_rnorm <- function(n, mean=0, sd=1){
  
  if (n%%2 == 0){
    size = n/2
  } else {
    size = (n+1)/2
  }
  
  a = runif(size)
  b = runif(size)
  
  x1 = rep(0, size)
  x2 = rep(0, size)  
  
  for (i in 1:size){
    p <- 2*pi*a[i]
    sq <- sqrt(-2*log(b[i]))
    x1[i] = sin(p)*sq*sd + mean
    x2[i] = cos(p)*sq*sd + mean
  }
  
  if (n%%2 == 0){
    return(c(x1, x2))
  } else {
    x2 <- x2[-1]
    return(c(x1, x2))
  }
}

library(nortest)

#a test for normality
lillie.test(my_rnorm(100, mean = 5, sd = 5))

#plot the estimation of the density
x <- rnorm(100, mean = 5, sd = 5)
plot(density(my_rnorm(100, mean = 5, sd = 5)))
lines(density(x))

################################## rchisq ####################################

my_rchisq <- function(n, df) {
  
  x <- rep(0, n)
  
  for (i in 1:n){
    x[i] <- sum(my_rnorm(df)**2)
  }
  
  return(x)
}

chi1 <- my_rchisq(1000, 10)
plot(density(chi1))


################################## r t ####################################

my_rt <- function(n, df){
  
  z <- my_rnorm(n)
  u <- my_rchisq(n, m)
  
  t <- rep(0, n)
  
  for (i in 1:n){
    t[i] <- z[i]/sqrt(u[i]/m)
  }
  return(t)
}








